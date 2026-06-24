pacman::p_load(
  shiny,
  tidyverse,
  bslib,
  DT,
  plotly,
  visNetwork,
  tidytext,
  igraph
)

# ============================================================
# LOAD AND PREPARE DATA
# ============================================================

prepared_data <- readRDS("mc1_prepared_data.rds")

communications <- if (is.data.frame(prepared_data)) {
  prepared_data
} else if ("communications" %in% names(prepared_data)) {
  prepared_data$communications
} else {
  stop("Cannot find a communications table inside mc1_prepared_data.rds.")
}

# The prepared data used by your project contains agent_id and hour.
if (!"agent_id" %in% names(communications)) {
  stop("The communications data must contain an agent_id column.")
}

if (!"hour" %in% names(communications) && !"timestamp" %in% names(communications)) {
  stop("The communications data must contain either an hour or timestamp column.")
}

# Add fallback columns only if the prepared data does not already contain them.
if (!"agent_label" %in% names(communications)) {
  communications$agent_label <- communications$agent_id
}

if (!"agent_role" %in% names(communications)) {
  communications$agent_role <- "Unknown"
}

if (!"channel" %in% names(communications)) {
  communications$channel <- "Unknown"
}

if (!"content" %in% names(communications)) {
  communications$content <- ""
}

if (!"internal_state_deliberating" %in% names(communications)) {
  communications$internal_state_deliberating <- ""
}

if (!"risk_signal_group" %in% names(communications)) {
  communications$risk_signal_group <- "No Signal"
}

# Use hour first because it is the same date field used in Alvin's original code.
date_source <- if ("hour" %in% names(communications)) {
  communications$hour
} else {
  communications$timestamp
}

communications <- communications |>
  mutate(
    agent_id = as.character(agent_id),
    agent_label = as.character(agent_label),
    agent_label = if_else(
      is.na(agent_label) | str_trim(agent_label) == "",
      agent_id,
      agent_label
    ),
    agent_role = replace_na(as.character(agent_role), "Unknown"),
    channel = replace_na(as.character(channel), "Unknown"),
    content = replace_na(as.character(content), ""),
    internal_state_deliberating = replace_na(
      as.character(internal_state_deliberating),
      ""
    ),
    risk_signal_group = replace_na(
      as.character(risk_signal_group),
      "No Signal"
    ),
    # Works for values such as 2046-06-04 or 2046-06-04T08:00:00.
    event_date = as.Date(substr(as.character(date_source), 1, 10)),
    full_text = str_squish(
      paste(internal_state_deliberating, content)
    )
  ) |>
  filter(!is.na(event_date))

if (nrow(communications) == 0) {
  stop("No valid dates could be read from the hour/timestamp column.")
}

min_date <- min(communications$event_date)
max_date <- max(communications$event_date)

breach_date <- as.Date("2046-06-04")

baseline_start_default <- min_date
baseline_end_default <- min(max_date, breach_date - 1)

if (baseline_end_default < min_date) {
  baseline_end_default <- min_date
}

critical_start_default <- max(min_date, breach_date)

if (critical_start_default > max_date) {
  critical_start_default <- min_date
}

critical_end_default <- max_date

all_agents <- sort(unique(communications$agent_label))
all_roles <- sort(unique(communications$agent_role))
all_channels <- sort(unique(communications$channel))
all_risks <- sort(unique(communications$risk_signal_group))

agent_palette <- c(
  "pr_agent"           = "#005f73",
  "pr_intern_agent"    = "#0a9396",
  "social_media_agent" = "#9b5de5",
  "legal_agent"        = "#1b4332",
  "judge_agent"        = "#6a040f",
  "quality_agent"      = "#ca6702",
  "intern_agent"       = "#f15bb5"
)

custom_stop_words <- tibble(
  word = c(
    "legal", "agent", "pr", "intern",
    "social", "media", "judge", "quality"
  )
)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

filter_messages <- function(data, date_range, selected_agents, selected_channels, selected_risks) {
  
  if (is.null(date_range) || length(date_range) != 2) {
    return(data[0, ])
  }
  
  if (is.null(selected_channels) || length(selected_channels) == 0) {
    selected_channels <- unique(data$channel)
  }
  
  if (is.null(selected_risks) || length(selected_risks) == 0) {
    selected_risks <- unique(data$risk_signal_group)
  }
  
  filtered <- data |>
    filter(
      event_date >= as.Date(date_range[1]),
      event_date <= as.Date(date_range[2]),
      channel %in% selected_channels,
      risk_signal_group %in% selected_risks
    )
  
  # "All" means no agent/role restriction.
  if (
    !is.null(selected_agents) &&
    length(selected_agents) > 0 &&
    !"All" %in% selected_agents
  ) {
    filtered <- filtered |>
      filter(
        agent_label %in% selected_agents |
          agent_role %in% selected_agents
      )
  }
  
  filtered
}

message_table <- function(data) {
  data |>
    transmute(
      Date = format(event_date, "%Y-%m-%d"),
      Agent = agent_label,
      Role = agent_role,
      Channel = channel,
      `Risk signal` = risk_signal_group,
      `Message evidence` = if_else(
        nchar(full_text) > 260,
        paste0(str_sub(full_text, 1, 260), "…"),
        full_text
      )
    )
}

empty_network <- function(message, height = "560px") {
  visNetwork(
    nodes = data.frame(
      id = "no_data",
      label = message,
      shape = "box",
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = character(),
      to = character(),
      stringsAsFactors = FALSE
    ),
    width = "100%",
    height = height
  ) |>
    visNodes(
      color = list(
        background = "#F1F5F9",
        border = "#94A3B8"
      ),
      font = list(
        color = "#475569",
        size = 15
      )
    ) |>
    visPhysics(enabled = FALSE)
}

# ============================================================
# QUESTION 2 PRIMARY VISUAL
# Alvin's original TF-IDF approach, adapted only for Shiny.
# ============================================================

build_alvin_keyword_network <- function(data, keywords_per_agent = 10) {
  
  words <- data |>
    filter(!is.na(agent_id), full_text != "") |>
    select(agent_id, full_text) |>
    unnest_tokens(word, full_text) |>
    anti_join(get_stopwords(), by = "word") |>
    anti_join(custom_stop_words, by = "word") |>
    filter(str_detect(word, "^[a-zA-Z]+$")) |>
    count(agent_id, word, name = "n") |>
    bind_tf_idf(word, agent_id, n) |>
    group_by(agent_id) |>
    slice_max(tf_idf, n = keywords_per_agent, with_ties = FALSE) |>
    ungroup()
  
  if (nrow(words) == 0) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }
  
  # Prefixes prevent an agent ID and a keyword with the same spelling
  # from being interpreted as the same node.
  agent_nodes <- words |>
    distinct(agent_id) |>
    transmute(
      id = paste0("agent::", agent_id),
      label = agent_id,
      group = "Agent",
      value = 35,
      color = coalesce(
        unname(agent_palette[agent_id]),
        "#3155E7"
      ),
      title = paste0("<b>Agent:</b> ", agent_id)
    )
  
  keyword_nodes <- words |>
    distinct(word) |>
    transmute(
      id = paste0("keyword::", word),
      label = word,
      group = "Keyword",
      value = 15,
      color = "#9ecae1",
      title = paste0("<b>Keyword:</b> ", word)
    )
  
  network_edges <- words |>
    mutate(
      edge_strength = if_else(
        is.finite(tf_idf) & tf_idf > 0,
        tf_idf,
        as.numeric(n)
      )
    ) |>
    transmute(
      from = paste0("agent::", agent_id),
      to = paste0("keyword::", word),
      value = pmax(1, edge_strength * 12),
      width = pmax(1, edge_strength * 6),
      title = paste0(
        "<b>Agent:</b> ", agent_id,
        "<br><b>Keyword:</b> ", word,
        "<br><b>Occurrences:</b> ", n,
        "<br><b>TF-IDF:</b> ", round(tf_idf, 3)
      )
    )
  
  list(
    nodes = bind_rows(agent_nodes, keyword_nodes),
    edges = network_edges
  )
}

draw_alvin_keyword_network <- function(network_data, height = "560px") {
  
  if (nrow(network_data$nodes) == 0) {
    return(
      empty_network(
        "No eligible terms were found for the selected period and filters.",
        height
      )
    )
  }
  
  visNetwork(
    nodes = network_data$nodes,
    edges = network_data$edges,
    width = "100%",
    height = height
  ) |>
    visGroups(
      groupname = "Agent",
      font = list(
        size = 16,
        face = "bold",
        color = "white"
      )
    ) |>
    visGroups(
      groupname = "Keyword",
      font = list(
        size = 12,
        color = "#24364B"
      )
    ) |>
    visEdges(
      smooth = FALSE,
      color = list(
        color = "#BFC9D4",
        opacity = 0.8
      )
    ) |>
    visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ),
      nodesIdSelection = TRUE
    ) |>
    visPhysics(
      solver = "forceAtlas2Based",
      stabilization = TRUE
    ) |>
    visLayout(randomSeed = 123)
}

# ============================================================
# QUESTION 2 SUPPORTING VISUAL
# Michelle's agent-channel network, built from communications.
# ============================================================

build_michelle_channel_network <- function(
    data,
    reference_node = "ALL",
    search_text = "",
    network_size = 8,
    show_connected = TRUE
) {
  
  edge_data <- data |>
    filter(
      !is.na(agent_label),
      agent_label != "",
      !is.na(channel),
      channel != "",
      channel != "Unknown"
    ) |>
    count(agent_label, channel, name = "weight") |>
    transmute(
      agent = as.character(agent_label),
      channel = as.character(channel),
      weight = as.numeric(weight),
      from = paste0("agent::", agent),
      to = paste0("channel::", channel)
    )
  
  empty_result <- list(
    nodes = data.frame(),
    edges = data.frame(),
    ranking = data.frame()
  )
  
  if (nrow(edge_data) == 0) {
    return(empty_result)
  }
  
  search_text <- str_trim(search_text)
  
  if (nchar(search_text) > 0) {
    edge_data <- edge_data |>
      filter(
        str_detect(
          str_to_lower(agent),
          fixed(str_to_lower(search_text))
        ) |
          str_detect(
            str_to_lower(channel),
            fixed(str_to_lower(search_text))
          )
      )
  }
  
  if (nrow(edge_data) == 0) {
    return(empty_result)
  }
  
  if (!is.null(reference_node) && reference_node != "ALL") {
    
    direct_edges <- edge_data |>
      filter(from == reference_node | to == reference_node)
    
    if (nrow(direct_edges) == 0) {
      return(empty_result)
    }
    
    if (isTRUE(show_connected)) {
      linked_agents <- unique(direct_edges$agent)
      linked_channels <- unique(direct_edges$channel)
      
      edge_data <- edge_data |>
        filter(
          agent %in% linked_agents |
            channel %in% linked_channels
        )
    } else {
      edge_data <- direct_edges
    }
    
    # Keep the reference view readable.
    edge_data <- edge_data |>
      slice_max(weight, n = network_size * 4, with_ties = FALSE)
    
  } else {
    
    top_agents <- edge_data |>
      group_by(agent) |>
      summarise(messages = sum(weight), .groups = "drop") |>
      slice_max(messages, n = network_size, with_ties = FALSE) |>
      pull(agent)
    
    top_channels <- edge_data |>
      group_by(channel) |>
      summarise(messages = sum(weight), .groups = "drop") |>
      slice_max(messages, n = network_size, with_ties = FALSE) |>
      pull(channel)
    
    edge_data <- edge_data |>
      filter(agent %in% top_agents, channel %in% top_channels)
  }
  
  if (nrow(edge_data) == 0) {
    return(empty_result)
  }
  
  graph_data <- igraph::graph_from_data_frame(
    edge_data |>
      select(from, to, weight),
    directed = TRUE
  )
  
  degree_scores <- igraph::degree(
    graph_data,
    mode = "all"
  )
  
  raw_betweenness <- igraph::betweenness(
    graph_data,
    directed = FALSE
  )
  
  number_of_nodes <- igraph::vcount(graph_data)
  normalising_denominator <- max(
    1,
    (number_of_nodes - 1) * (number_of_nodes - 2) / 2
  )
  
  node_ids <- names(degree_scores)
  
  vis_nodes <- tibble(
    id = node_ids,
    type = if_else(
      str_detect(node_ids, "^agent::"),
      "Agent",
      "Channel"
    ),
    label = str_remove(node_ids, "^(agent|channel)::"),
    degree_centrality = as.numeric(degree_scores[node_ids]),
    betweenness_stat = as.numeric(
      raw_betweenness[node_ids] / normalising_denominator
    )
  ) |>
    mutate(
      color = case_when(
        type == "Agent" ~ "#1abc9c",
        type == "Channel" ~ "#34495e",
        TRUE ~ "#7f8c8d"
      ),
      shape = case_when(
        type == "Agent" ~ "dot",
        type == "Channel" ~ "square",
        TRUE ~ "dot"
      ),
      size = pmax(18, log1p(degree_centrality) * 14),
      title = paste0(
        "<b>", label, "</b>",
        "<br>Type: ", type,
        "<br>Degree Centrality: ", round(degree_centrality, 2),
        "<br>Betweenness: ", round(betweenness_stat, 3)
      )
    )
  
  vis_edges <- edge_data |>
    transmute(
      from = from,
      to = to,
      value = weight,
      width = pmax(1, log1p(weight) * 2),
      color = "#bdc3c7",
      arrows = "to",
      title = paste0("Messages exchanged: ", weight)
    )
  
  ranking <- vis_nodes |>
    transmute(
      Node = label,
      Type = type,
      `Degree centrality` = round(degree_centrality, 2),
      Betweenness = round(betweenness_stat, 3)
    ) |>
    arrange(desc(`Degree centrality`), desc(Betweenness), Node)
  
  list(
    nodes = vis_nodes,
    edges = vis_edges,
    ranking = ranking
  )
}

draw_michelle_channel_network <- function(network_data, height = "610px") {
  
  if (nrow(network_data$nodes) == 0) {
    return(
      empty_network(
        "No agent-channel relationships were found for the selected period and filters.",
        height
      )
    )
  }
  
  visNetwork(
    nodes = network_data$nodes,
    edges = network_data$edges,
    width = "100%",
    height = height
  ) |>
    visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ),
      nodesIdSelection = TRUE
    ) |>
    visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -50
      )
    ) |>
    visLayout(randomSeed = 42)
}

# ============================================================
# USER INTERFACE
# ============================================================

ui <- navbarPage(
  title = "TenantThread Dashboard",
  
  theme = bs_theme(
    version = 5,
    primary = "#3155E7",
    bg = "#F7F9FC",
    fg = "#24364B"
  ),
  
  header = tags$head(
    tags$style(HTML("
      .filter-panel {
        background: #FFFFFF;
        border: 1px solid #D9E2EC;
        border-radius: 8px;
        padding: 16px;
        box-shadow: 0 2px 5px rgba(16,42,67,0.05);
      }
      .filter-panel h3 {
        margin-top: 0;
        font-size: 21px;
        font-weight: 700;
        color: #102A43;
      }
      .guide-box {
        background: #EEF7FC;
        border-left: 4px solid #55A9C8;
        border-radius: 4px;
        padding: 12px 14px;
        margin-bottom: 16px;
        color: #486581;
      }
      .primary-tag, .support-tag {
        display: inline-block;
        padding: 4px 8px;
        border-radius: 4px;
        font-size: 11px;
        font-weight: 700;
        text-transform: uppercase;
        margin-bottom: 8px;
      }
      .primary-tag {
        background: #E6F6FF;
        color: #1F5A8A;
      }
      .support-tag {
        background: #FFF3D6;
        color: #8A5D00;
      }
      .small-note {
        font-size: 13px;
        color: #627D98;
        margin-bottom: 8px;
      }
    "))
  ),
  
  # ----------------------------------------------------------
  # OVERVIEW
  # ----------------------------------------------------------
  tabPanel(
    "Overview",
    br(),
    card(
      card_header("TenantThread: Embargo Breach Investigation"),
      p("Use the tabs to reconstruct the breach path, compare behaviour before and during the critical period, examine leading indicators, and search the underlying evidence.")
    ),
    br(),
    layout_columns(
      card(card_header("Total Messages"), h2(textOutput("total_messages"))),
      card(card_header("Unique Agents"), h2(textOutput("total_agents"))),
      card(card_header("Channels"), h2(textOutput("total_channels"))),
      card(card_header("High-Risk Messages"), h2(textOutput("high_risk_messages"))),
      col_widths = c(3, 3, 3, 3)
    )
  ),
  
  # ----------------------------------------------------------
  # QUESTION 1
  # ----------------------------------------------------------
  tabPanel(
    tagList(icon("road"), "1. Path to Breach"),
    br(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(
          class = "filter-panel",
          h3("Investigation Filters"),
          dateRangeInput("q1_dates", "Date range", min_date, max_date, min = min_date, max = max_date),
          selectizeInput("q1_agents", "Agent / role", c("All", all_agents, all_roles), selected = "All", multiple = TRUE),
          selectizeInput("q1_channels", "Channel", all_channels, selected = all_channels, multiple = TRUE),
          checkboxGroupInput("q1_risks", "Risk level", all_risks, selected = all_risks)
        )
      ),
      mainPanel(
        width = 9,
        div(class = "guide-box", tags$b("Purpose: "), "Identify message activity and high-risk communications leading up to the inappropriate release."),
        card(card_header("Communication Activity by Day"), plotlyOutput("q1_plot", height = "360px")),
        br(),
        card(card_header("Filtered Message Evidence"), DTOutput("q1_table"))
      )
    )
  ),
  
  # ----------------------------------------------------------
  # QUESTION 2
  # ----------------------------------------------------------
  tabPanel(
    tagList(icon("exchange-alt"), "2. Behaviour Change"),
    br(),
    card(
      card_header("Question 2: Was the embargo evasion a new behaviour?"),
      p("The primary visual compares agent vocabulary before and during the critical period. The supporting visual examines agent-channel relationships within the selected period.")
    ),
    br(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(
          class = "filter-panel",
          h3("Investigation Filters"),
          
          dateRangeInput(
            "baseline_dates",
            "Baseline period",
            start = baseline_start_default,
            end = baseline_end_default,
            min = min_date,
            max = max_date
          ),
          
          dateRangeInput(
            "critical_dates",
            "Critical period",
            start = critical_start_default,
            end = critical_end_default,
            min = min_date,
            max = max_date
          ),
          
          selectizeInput(
            "q2_agents",
            "Agent / role",
            choices = c("All", all_agents, all_roles),
            selected = "All",
            multiple = TRUE
          ),
          
          selectizeInput(
            "q2_channels",
            "Channel",
            choices = all_channels,
            selected = all_channels,
            multiple = TRUE
          ),
          
          checkboxGroupInput(
            "q2_risks",
            "Risk level",
            choices = all_risks,
            selected = all_risks
          ),
          
          sliderInput(
            "keywords_per_agent",
            "Keywords per agent",
            min = 3,
            max = 15,
            value = 10
          ),
          
          tags$hr(),
          h3("Supporting Explorer"),
          
          radioButtons(
            "support_period",
            "Period to inspect",
            choices = c("Baseline" = "Baseline", "Critical period" = "Critical"),
            selected = "Critical"
          ),
          
          selectizeInput(
            "support_reference",
            "Reference node",
            choices = c("All nodes" = "ALL"),
            selected = "ALL"
          ),
          
          textInput(
            "support_search",
            "Search agent or channel",
            placeholder = "Type a name"
          ),
          
          sliderInput(
            "support_size",
            "Network size",
            min = 3,
            max = 15,
            value = 8
          ),
          
          checkboxInput(
            "support_expand",
            "Show one-hop connected nodes",
            value = TRUE
          )
        )
      ),
      
      mainPanel(
        width = 9,
        
        div(
          class = "guide-box",
          tags$b("How to read this: "),
          "Terms linked to an agent in the critical-period network but not in the baseline network are candidate evidence of changed communication behaviour. This identifies patterns for investigation; it does not establish intent."
        ),
        
        div(class = "primary-tag", "Primary evidence: agent-keyword behaviour"),
        
        fluidRow(
          column(
            6,
            card(
              card_header("Baseline Network Structure"),
              div(class = "small-note", "Typical agent vocabulary before 4 June 2046."),
              visNetworkOutput("baseline_network", height = "560px")
            )
          ),
          column(
            6,
            card(
              card_header("Critical-Period Network Structure"),
              div(class = "small-note", "Distinctive agent vocabulary from 4 June 2046 onward."),
              visNetworkOutput("critical_network", height = "560px")
            )
          )
        ),
        
        br(),
        
        card(
          card_header("Behaviour Comparison Summary"),
          DTOutput("q2_summary")
        ),
        
        br(),
        
        div(class = "support-tag", "Supporting evidence: agent-channel relationships"),
        
        card(
          card_header("Agent-Channel Relationship Network"),
          div(class = "small-note", "Agents are circles and channels are squares. Hover over a node to view degree centrality and betweenness."),
          visNetworkOutput("support_network", height = "610px")
        ),
        
        br(),
        
        card(
          card_header("Node Centrality Summary"),
          DTOutput("support_table")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # QUESTION 3
  # ----------------------------------------------------------
  tabPanel(
    tagList(icon("warning"), "3. Leading Indicators"),
    br(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(
          class = "filter-panel",
          h3("Investigation Filters"),
          dateRangeInput("q3_dates", "Prior period", min_date, baseline_end_default, min = min_date, max = max_date),
          selectizeInput("q3_agents", "Agent / role", c("All", all_agents, all_roles), selected = "All", multiple = TRUE),
          selectizeInput("q3_channels", "Channel", all_channels, selected = all_channels, multiple = TRUE),
          checkboxGroupInput("q3_risks", "Risk level", all_risks, selected = all_risks)
        )
      ),
      mainPanel(
        width = 9,
        div(class = "guide-box", tags$b("Purpose: "), "Review earlier signals and communications that may have indicated the release was possible."),
        card(card_header("Prior-Period Activity"), plotlyOutput("q3_plot", height = "360px")),
        br(),
        card(card_header("Prior-Period Evidence"), DTOutput("q3_table"))
      )
    )
  ),
  
  # ----------------------------------------------------------
  # QUESTION 4
  # ----------------------------------------------------------
  tabPanel(
    tagList(icon("search"), "4. Evidence Explorer"),
    br(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(
          class = "filter-panel",
          h3("Investigation Filters"),
          dateRangeInput("q4_dates", "Date range", min_date, max_date, min = min_date, max = max_date),
          selectizeInput("q4_agents", "Agent / role", c("All", all_agents, all_roles), selected = "All", multiple = TRUE),
          selectizeInput("q4_channels", "Channel", all_channels, selected = all_channels, multiple = TRUE),
          checkboxGroupInput("q4_risks", "Risk level", all_risks, selected = all_risks),
          textInput("q4_search", "Search text", placeholder = "e.g., embargo, post, release")
        )
      ),
      mainPanel(
        width = 9,
        div(class = "guide-box", tags$b("Purpose: "), "Search and inspect the underlying communications in context."),
        card(card_header("Searchable Message Evidence"), DTOutput("q4_table"))
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Overview
  output$total_messages <- renderText(scales::comma(nrow(communications)))
  output$total_agents <- renderText(scales::comma(n_distinct(communications$agent_id)))
  output$total_channels <- renderText(scales::comma(n_distinct(communications$channel)))
  output$high_risk_messages <- renderText(
    scales::comma(sum(communications$risk_signal_group == "High Signal", na.rm = TRUE))
  )
  
  # Question 1
  q1_data <- reactive({
    filter_messages(
      communications,
      input$q1_dates,
      input$q1_agents,
      input$q1_channels,
      input$q1_risks
    )
  })
  
  output$q1_plot <- renderPlotly({
    plot_data <- q1_data() |>
      count(event_date, risk_signal_group)
    
    validate(need(nrow(plot_data) > 0, "No messages match the selected filters."))
    
    plot_ly(
      plot_data,
      x = ~event_date,
      y = ~n,
      type = "scatter",
      mode = "lines+markers",
      color = ~risk_signal_group
    ) |>
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Number of messages")
      )
  })
  
  output$q1_table <- renderDT({
    datatable(
      message_table(q1_data()),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Question 2 - primary
  baseline_data <- reactive({
    filter_messages(
      communications,
      input$baseline_dates,
      input$q2_agents,
      input$q2_channels,
      input$q2_risks
    )
  })
  
  critical_data <- reactive({
    filter_messages(
      communications,
      input$critical_dates,
      input$q2_agents,
      input$q2_channels,
      input$q2_risks
    )
  })
  
  output$baseline_network <- renderVisNetwork({
    draw_alvin_keyword_network(
      build_alvin_keyword_network(
        baseline_data(),
        input$keywords_per_agent
      )
    )
  })
  
  output$critical_network <- renderVisNetwork({
    draw_alvin_keyword_network(
      build_alvin_keyword_network(
        critical_data(),
        input$keywords_per_agent
      )
    )
  })
  
  output$q2_summary <- renderDT({
    comparison <- bind_rows(
      baseline_data() |>
        summarise(
          Period = "Baseline",
          Messages = n(),
          Agents = n_distinct(agent_id),
          Channels = n_distinct(channel),
          `High-risk messages` = sum(risk_signal_group == "High Signal", na.rm = TRUE)
        ),
      critical_data() |>
        summarise(
          Period = "Critical period",
          Messages = n(),
          Agents = n_distinct(agent_id),
          Channels = n_distinct(channel),
          `High-risk messages` = sum(risk_signal_group == "High Signal", na.rm = TRUE)
        )
    )
    
    datatable(
      comparison,
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE)
    )
  })
  
  # Question 2 - supporting
  support_period_data <- reactive({
    if (is.null(input$support_period) || input$support_period == "Baseline") {
      baseline_data()
    } else {
      critical_data()
    }
  })
  
  observeEvent(
    list(
      input$support_period,
      input$baseline_dates,
      input$critical_dates,
      input$q2_agents,
      input$q2_channels,
      input$q2_risks
    ),
    {
      current_data <- support_period_data()
      
      agents <- sort(unique(current_data$agent_label))
      channels <- sort(unique(current_data$channel[current_data$channel != "Unknown"]))
      
      choices <- c("All nodes" = "ALL")
      
      if (length(agents) > 0) {
        choices <- c(
          choices,
          setNames(
            paste0("agent::", agents),
            paste0("Agent — ", agents)
          )
        )
      }
      
      if (length(channels) > 0) {
        choices <- c(
          choices,
          setNames(
            paste0("channel::", channels),
            paste0("Channel — ", channels)
          )
        )
      }
      
      current_value <- isolate(input$support_reference)
      
      if (is.null(current_value) || !current_value %in% unname(choices)) {
        current_value <- "ALL"
      }
      
      updateSelectizeInput(
        session,
        "support_reference",
        choices = choices,
        selected = current_value,
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )
  
  support_data <- reactive({
    reference_value <- input$support_reference
    
    if (is.null(reference_value)) {
      reference_value <- "ALL"
    }
    
    build_michelle_channel_network(
      data = support_period_data(),
      reference_node = reference_value,
      search_text = input$support_search,
      network_size = input$support_size,
      show_connected = input$support_expand
    )
  })
  
  output$support_network <- renderVisNetwork({
    draw_michelle_channel_network(support_data())
  })
  
  output$support_table <- renderDT({
    ranking <- support_data()$ranking
    
    if (nrow(ranking) == 0) {
      return(
        datatable(
          data.frame(
            Message = "No supporting-network nodes match the selected filters."
          ),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }
    
    datatable(
      ranking,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthChange = FALSE,
        scrollX = TRUE
      )
    )
  })
  
  # Question 3
  q3_data <- reactive({
    filter_messages(
      communications,
      input$q3_dates,
      input$q3_agents,
      input$q3_channels,
      input$q3_risks
    )
  })
  
  output$q3_plot <- renderPlotly({
    plot_data <- q3_data() |>
      count(event_date, risk_signal_group)
    
    validate(need(nrow(plot_data) > 0, "No messages match the selected filters."))
    
    plot_ly(
      plot_data,
      x = ~event_date,
      y = ~n,
      type = "bar",
      color = ~risk_signal_group
    ) |>
      layout(
        barmode = "stack",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Number of messages")
      )
  })
  
  output$q3_table <- renderDT({
    datatable(
      message_table(q3_data()),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Question 4
  q4_data <- reactive({
    data <- filter_messages(
      communications,
      input$q4_dates,
      input$q4_agents,
      input$q4_channels,
      input$q4_risks
    )
    
    search_value <- str_trim(input$q4_search)
    
    if (nchar(search_value) > 0) {
      data <- data |>
        filter(
          str_detect(
            str_to_lower(full_text),
            fixed(str_to_lower(search_value))
          )
        )
    }
    
    data
  })
  
  output$q4_table <- renderDT({
    datatable(
      message_table(q4_data()),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
}

shinyApp(ui = ui, server = server)