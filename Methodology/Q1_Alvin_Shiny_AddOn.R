# ============================================================
# Q1_ALVIN_SHINY_ADDON.R
# Path to Breach: Alvin's analyses adapted for the TenantThread
# Shiny dashboard.
#
# This is an ADD-ON for your existing app.R, not a standalone app.
#
# It adds:
# 1. Morning-versus-afternoon agent-to-channel/recipient networks
#    with subgraph centrality.
# 2. Emerging bigrams: phrases that increased in the afternoon.
# 3. A forensic pivot view using vocabulary Jaccard similarity
#    and an Adamic-Adar-style shared-connection score.
# 4. A focused message-evidence table around the detected pivot.
# ============================================================


# ============================================================
# PART 1 — ADD "igraph" TO YOUR PACKAGE LIST
# ============================================================
# At the top of app.R, change:
#
#   tidytext
#
# to:
#
#   tidytext,
#   igraph
#
# Example:
#
# pacman::p_load(
#   shiny, tidyverse, bslib, DT, plotly, visNetwork,
#   tidytext, igraph
# )


# ============================================================
# PART 2 — ADD THIS AFTER YOUR communications DATA IS PREPARED
# ============================================================
# Paste immediately AFTER the section where communications has
# event_date and full_text, and BEFORE:
#   min_date <- min(communications$event_date)
#
# ------------------------------------------------------------

q1_parse_datetime <- function(x) {

  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  x <- as.character(x)

  parsed <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE))

  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(lubridate::ymd_hm(x, tz = "UTC", quiet = TRUE))
  }

  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(lubridate::ymd(x, tz = "UTC", quiet = TRUE))
  }

  parsed
}

q1_time_source <- if ("hour" %in% names(communications)) {
  communications$hour
} else if ("timestamp" %in% names(communications)) {
  communications$timestamp
} else {
  communications$event_date
}

communications$q1_event_time <- q1_parse_datetime(q1_time_source)

# Alvin's original code uses recipients when the analysis is
# relationship-based. This makes the recipient field optional.
q1_recipient_column <- intersect(
  c("recipients", "recipient", "recipient_label", "to"),
  names(communications)
)

communications$q1_recipient <- if (length(q1_recipient_column) > 0) {
  as.character(communications[[q1_recipient_column[1]]])
} else {
  NA_character_
}

q1_default_date <- if (as.Date("2046-06-05") %in% communications$event_date) {
  as.Date("2046-06-05")
} else {
  max(communications$event_date, na.rm = TRUE)
}

q1_target_choices <- c(
  "Agent → Channel" = "Channel"
)

if (any(!is.na(communications$q1_recipient) & communications$q1_recipient != "")) {
  q1_target_choices <- c(
    q1_target_choices,
    "Agent → Recipient" = "Recipient"
  )
}


# ============================================================
# PART 3 — ADD THESE HELPER FUNCTIONS
# ============================================================
# Paste these BEFORE:
#   # ============================================================
#   # USER INTERFACE
#   # ============================================================
#
# ------------------------------------------------------------

q1_scale_size <- function(x, lower = 0, upper = 30) {

  if (length(x) == 0) {
    return(numeric())
  }

  x <- replace_na(as.numeric(x), 0)

  if (max(x) == min(x)) {
    return(rep((lower + upper) / 2, length(x)))
  }

  lower + (x - min(x)) / (max(x) - min(x)) * (upper - lower)
}

q1_prepare_day_data <- function(
  data,
  selected_date,
  selected_agents,
  selected_channels,
  selected_risks,
  target_view = "Channel"
) {

  selected_data <- filter_messages(
    data,
    as.Date(c(selected_date, selected_date)),
    selected_agents,
    selected_channels,
    selected_risks
  )

  if (nrow(selected_data) == 0) {
    return(selected_data)
  }

  selected_data |>
    mutate(
      q1_hour = lubridate::hour(q1_event_time),
      q1_period = case_when(
        q1_hour < 12 ~ "Morning",
        q1_hour >= 12 ~ "Afternoon",
        TRUE ~ "Unknown"
      ),
      q1_target = case_when(
        target_view == "Recipient" &
          !is.na(q1_recipient) &
          q1_recipient != "" ~ q1_recipient,
        TRUE ~ channel
      ),
      q1_risk_weight = case_when(
        risk_signal_group == "High Signal" ~ 4,
        risk_signal_group == "Moderate Signal" ~ 2,
        risk_signal_group == "Low Signal" ~ 1,
        TRUE ~ 0
      )
    ) |>
    filter(
      !is.na(q1_event_time),
      !is.na(q1_target),
      q1_target != "",
      q1_target != "Unknown",
      q1_period != "Unknown"
    )
}

q1_build_centrality_network <- function(data, top_nodes = 8) {

  empty_result <- list(
    nodes = data.frame(),
    edges = data.frame(),
    agent_ranking = data.frame(),
    target_ranking = data.frame()
  )

  if (nrow(data) == 0) {
    return(empty_result)
  }

  edge_summary <- data |>
    group_by(agent_label, q1_target) |>
    summarise(
      interactions = n(),
      total_risk = sum(q1_risk_weight, na.rm = TRUE),
      high_risk_messages = sum(
        risk_signal_group == "High Signal",
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    mutate(
      from = paste0("agent::", agent_label),
      to = paste0("target::", q1_target)
    )

  if (nrow(edge_summary) == 0) {
    return(empty_result)
  }

  # Keep the view readable while retaining the strongest relationships.
  if (top_nodes < length(unique(edge_summary$agent_label))) {

    top_agents <- edge_summary |>
      group_by(agent_label) |>
      summarise(total_interactions = sum(interactions), .groups = "drop") |>
      slice_max(total_interactions, n = top_nodes, with_ties = FALSE) |>
      pull(agent_label)

    edge_summary <- edge_summary |>
      filter(agent_label %in% top_agents)
  }

  graph_object <- igraph::graph_from_data_frame(
    edge_summary |>
      select(from, to),
    directed = FALSE
  )

  centrality_scores <- tryCatch(
    igraph::subgraph_centrality(graph_object),
    error = function(e) igraph::degree(graph_object, mode = "all")
  )

  node_ids <- igraph::V(graph_object)$name

  node_connections <- bind_rows(
    edge_summary |>
      group_by(agent_label) |>
      summarise(
        connections = n_distinct(q1_target),
        interactions = sum(interactions),
        .groups = "drop"
      ) |>
      transmute(
        id = paste0("agent::", agent_label),
        connections,
        interactions
      ),

    edge_summary |>
      group_by(q1_target) |>
      summarise(
        connections = n_distinct(agent_label),
        interactions = sum(interactions),
        .groups = "drop"
      ) |>
      transmute(
        id = paste0("target::", q1_target),
        connections,
        interactions
      )
  )

  nodes <- tibble(
    id = node_ids,
    node_type = if_else(str_detect(node_ids, "^agent::"), "Agent", "Target"),
    label = str_remove(node_ids, "^(agent|target)::"),
    subgraph_centrality = as.numeric(centrality_scores[node_ids])
  ) |>
    left_join(node_connections, by = "id") |>
    mutate(
      subgraph_centrality = replace_na(subgraph_centrality, 0),
      connections = replace_na(connections, 0L),
      interactions = replace_na(interactions, 0L),
      shape = if_else(node_type == "Agent", "square", "dot"),
      color = if_else(node_type == "Agent", "#F472B6", "#8ECAE6"),
      size = if_else(
        node_type == "Agent",
        22 + q1_scale_size(subgraph_centrality, 0, 30),
        14 + q1_scale_size(subgraph_centrality, 0, 20)
      ),
      title = paste0(
        "<b>", label, "</b>",
        "<br>Type: ", if_else(node_type == "Agent", "Agent", "Channel / Recipient"),
        "<br>Subgraph centrality: ", round(subgraph_centrality, 2),
        "<br>Connections: ", connections,
        "<br>Interactions: ", interactions
      )
    )

  edges <- edge_summary |>
    transmute(
      from,
      to,
      value = interactions,
      width = pmax(1, log1p(interactions) * 2),
      arrows = "to",
      title = paste0(
        "<b>Interactions:</b> ", interactions,
        "<br><b>Total risk score:</b> ", total_risk,
        "<br><b>High-risk messages:</b> ", high_risk_messages
      )
    )

  agent_ranking <- nodes |>
    filter(node_type == "Agent") |>
    transmute(
      Agent = label,
      `Subgraph centrality` = round(subgraph_centrality, 2),
      Connections = connections,
      Interactions = interactions
    ) |>
    arrange(desc(`Subgraph centrality`), desc(Interactions))

  target_ranking <- nodes |>
    filter(node_type == "Target") |>
    transmute(
      `Channel / Recipient` = label,
      `Subgraph centrality` = round(subgraph_centrality, 2),
      Connections = connections,
      Interactions = interactions
    ) |>
    arrange(desc(`Subgraph centrality`), desc(Interactions))

  list(
    nodes = nodes,
    edges = edges,
    agent_ranking = agent_ranking,
    target_ranking = target_ranking
  )
}

q1_draw_centrality_network <- function(network_data, height = "600px") {

  if (nrow(network_data$nodes) == 0) {
    return(
      empty_network(
        "No interactions match the selected filters and time period.",
        height
      )
    )
  }

  visNetwork(
    network_data$nodes,
    network_data$edges,
    width = "100%",
    height = height
  ) |>
    visNodes(
      borderWidth = 1,
      color = list(
        border = "#64748B"
      )
    ) |>
    visEdges(
      color = list(
        color = "#94A3B8",
        highlight = "#DC2626"
      ),
      smooth = FALSE
    ) |>
    visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ),
      nodesIdSelection = FALSE
    ) |>
    visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -65,
        springLength = 115,
        springConstant = 0.04
      ),
      stabilization = TRUE
    ) |>
    visLayout(randomSeed = 42)
}

q1_emerging_bigrams <- function(
  data,
  afternoon_network,
  phrase_count = 12
) {

  empty_result <- tibble(
    bigram = character(),
    Morning = integer(),
    Afternoon = integer(),
    Shift = integer()
  )

  if (nrow(data) == 0) {
    return(empty_result)
  }

  top_agents <- afternoon_network$agent_ranking |>
    slice_head(n = 3) |>
    pull(Agent)

  top_targets <- afternoon_network$target_ranking |>
    slice_head(n = 3) |>
    pull(`Channel / Recipient`)

  focused_data <- data |>
    filter(
      agent_label %in% top_agents,
      q1_target %in% top_targets,
      full_text != ""
    )

  if (nrow(focused_data) == 0) {
    return(empty_result)
  }

  phrases <- focused_data |>
    select(q1_period, full_text) |>
    unnest_tokens(bigram, full_text, token = "ngrams", n = 2) |>
    separate(bigram, into = c("word1", "word2"), sep = " ", remove = FALSE) |>
    filter(
      !is.na(word1),
      !is.na(word2),
      !word1 %in% tidytext::stop_words$word,
      !word2 %in% tidytext::stop_words$word,
      str_detect(word1, "^[a-zA-Z]+$"),
      str_detect(word2, "^[a-zA-Z]+$")
    ) |>
    count(q1_period, bigram, name = "n") |>
    pivot_wider(
      names_from = q1_period,
      values_from = n,
      values_fill = 0
    )

  if (!"Morning" %in% names(phrases)) {
    phrases$Morning <- 0
  }

  if (!"Afternoon" %in% names(phrases)) {
    phrases$Afternoon <- 0
  }

  phrases |>
    mutate(
      Shift = Afternoon - Morning
    ) |>
    filter(Shift > 0) |>
    arrange(desc(Shift), desc(Afternoon), bigram) |>
    slice_head(n = phrase_count)
}

q1_vocabulary_similarity <- function(data) {

  empty_result <- tibble(
    time_bin = as.POSIXct(character()),
    jaccard_similarity = numeric()
  )

  if (nrow(data) == 0) {
    return(empty_result)
  }

  bin_words <- data |>
    mutate(
      time_bin = lubridate::floor_date(q1_event_time, "30 minutes")
    ) |>
    select(time_bin, full_text) |>
    unnest_tokens(word, full_text) |>
    anti_join(tidytext::stop_words, by = "word") |>
    filter(str_detect(word, "^[a-zA-Z]+$")) |>
    group_by(time_bin) |>
    summarise(
      words = list(unique(word)),
      .groups = "drop"
    ) |>
    arrange(time_bin)

  if (nrow(bin_words) < 2) {
    return(empty_result)
  }

  purrr::map_dfr(
    seq_len(nrow(bin_words) - 1),
    function(i) {

      words_a <- bin_words$words[[i]]
      words_b <- bin_words$words[[i + 1]]

      union_size <- length(union(words_a, words_b))
      similarity <- if (union_size == 0) {
        0
      } else {
        length(intersect(words_a, words_b)) / union_size
      }

      tibble(
        time_bin = bin_words$time_bin[[i + 1]],
        jaccard_similarity = similarity
      )
    }
  )
}

q1_adamic_adar_timeline <- function(data, top_agents) {

  empty_result <- tibble(
    time_bin = as.POSIXct(character()),
    adamic_adar = numeric()
  )

  if (nrow(data) == 0 || length(top_agents) < 2) {
    return(empty_result)
  }

  selected_actors <- top_agents[1:2]

  data |>
    mutate(
      time_bin = lubridate::floor_date(q1_event_time, "30 minutes")
    ) |>
    group_by(time_bin) |>
    group_modify(
      ~ {
        edges <- .x |>
          filter(agent_label %in% selected_actors) |>
          distinct(agent_label, q1_target)

        if (nrow(edges) == 0 ||
            !all(selected_actors %in% edges$agent_label)) {
          return(tibble(adamic_adar = 0))
        }

        targets_a <- edges |>
          filter(agent_label == selected_actors[1]) |>
          pull(q1_target)

        targets_b <- edges |>
          filter(agent_label == selected_actors[2]) |>
          pull(q1_target)

        shared_targets <- intersect(targets_a, targets_b)

        if (length(shared_targets) == 0) {
          return(tibble(adamic_adar = 0))
        }

        target_degrees <- .x |>
          distinct(agent_label, q1_target) |>
          count(q1_target, name = "target_degree")

        score <- target_degrees |>
          filter(q1_target %in% shared_targets) |>
          summarise(
            value = sum(1 / log(pmax(target_degree, 2)))
          ) |>
          pull(value)

        tibble(adamic_adar = replace_na(score, 0))
      }
    ) |>
    ungroup() |>
    arrange(time_bin)
}

q1_empty_plot <- function(message) {
  plot_ly() |>
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        text = message,
        x = 0.5,
        y = 0.5,
        showarrow = FALSE,
        font = list(size = 15, color = "#627D98")
      ),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    )
}


# ============================================================
# PART 4 — REPLACE YOUR ENTIRE QUESTION 1 UI TAB WITH THIS
# ============================================================
# Replace the complete tabPanel that begins with:
#
#   tabPanel(
#     tagList(icon("road"), "1. Path to Breach"),
#
# and ends immediately before the Question 2 comment.
#
# ------------------------------------------------------------

tabPanel(
  tagList(icon("road"), "1. Path to Breach"),

  br(),

  card(
    card_header(
      "Question 1: What events and relationships led to the inappropriate release?"
    ),
    p(
      "This view reconstructs the breach day by comparing morning and afternoon interaction structures, ",
      "identifying emerging discussion themes, and surfacing the narrow evidence window where communication changed most sharply."
    )
  ),

  br(),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      div(
        class = "filter-panel",

        h3("Investigation Filters"),

        dateInput(
          "q1_focus_date",
          "Breach-day focus",
          value = q1_default_date,
          min = min_date,
          max = max_date
        ),

        selectizeInput(
          "q1_agents",
          "Agent / role",
          choices = c("All", all_agents, all_roles),
          selected = "All",
          multiple = TRUE
        ),

        selectizeInput(
          "q1_channels",
          "Channel",
          choices = all_channels,
          selected = all_channels,
          multiple = TRUE
        ),

        checkboxGroupInput(
          "q1_risks",
          "Risk level",
          choices = all_risks,
          selected = all_risks
        ),

        tags$hr(),

        h4("Network Detail"),

        radioButtons(
          "q1_target_view",
          "Relationship view",
          choices = q1_target_choices,
          selected = "Channel"
        ),

        sliderInput(
          "q1_network_size",
          "Maximum agents shown",
          min = 3,
          max = max(3, length(all_agents)),
          value = min(7, max(3, length(all_agents)))
        ),

        sliderInput(
          "q1_phrase_count",
          "Emerging phrases shown",
          min = 6,
          max = 20,
          value = 12
        )
      )
    ),

    mainPanel(
      width = 9,

      div(
        class = "guide-box",
        tags$b("How to read this: "),
        "Squares are agents and circles are channels or recipients. ",
        "Larger nodes have higher subgraph centrality, meaning they sit within more tightly connected interaction structures. ",
        "Compare the morning and afternoon networks to identify the changing coordination pathway."
      ),

      tabsetPanel(

        tabPanel(
          "1. Interaction Shift",

          br(),

          fluidRow(
            column(
              6,
              card(
                card_header("Morning Interaction Structure"),
                div(
                  class = "small-note",
                  "Before 12:00 PM. Nodes are sized by subgraph centrality."
                ),
                visNetworkOutput(
                  "q1_morning_network",
                  height = "570px"
                )
              )
            ),

            column(
              6,
              card(
                card_header("Afternoon Interaction Structure"),
                div(
                  class = "small-note",
                  "From 12:00 PM onward. Compare node size and relationship concentration."
                ),
                visNetworkOutput(
                  "q1_afternoon_network",
                  height = "570px"
                )
              )
            )
          ),

          br(),

          fluidRow(
            column(
              6,
              card(
                card_header("Morning: Most Central Agents"),
                tableOutput("q1_morning_agents")
              )
            ),

            column(
              6,
              card(
                card_header("Afternoon: Most Central Agents"),
                tableOutput("q1_afternoon_agents")
              )
            )
          )
        ),

        tabPanel(
          "2. Emerging Phrases",

          br(),

          card(
            card_header("War-Room Analysis: Emerging Phrases"),
            div(
              class = "small-note",
              "Phrases are calculated from the most central afternoon agents and channels/recipients. ",
              "Only phrases that became more frequent in the afternoon are shown."
            ),
            plotlyOutput(
              "q1_phrase_plot",
              height = "480px"
            )
          ),

          br(),

          card(
            card_header("Emerging Phrase Detail"),
            DTOutput("q1_phrase_table")
          )
        ),

        tabPanel(
          "3. Forensic Pivot and Evidence",

          br(),

          div(
            class = "guide-box",
            tags$b("Forensic logic: "),
            "A low Jaccard similarity marks a sharp vocabulary shift between consecutive 30-minute windows. ",
            "The Adamic-Adar-style score identifies when the two most central afternoon agents shared the strongest interaction context."
          ),

          fluidRow(
            column(
              6,
              card(
                card_header("Vocabulary Continuity"),
                div(
                  class = "small-note",
                  "Lower values indicate a sharper change in discussion vocabulary."
                ),
                plotlyOutput(
                  "q1_similarity_plot",
                  height = "310px"
                )
              )
            ),

            column(
              6,
              card(
                card_header("Interaction Cohesion"),
                div(
                  class = "small-note",
                  "Higher values indicate a stronger shared connection pattern between the two most central afternoon agents."
                ),
                plotlyOutput(
                  "q1_adamic_plot",
                  height = "310px"
                )
              )
            )
          ),

          br(),

          card(
            card_header("Detected Pivot Window"),
            uiOutput("q1_pivot_note")
          ),

          br(),

          card(
            card_header("Focused Message Evidence"),
            div(
              class = "small-note",
              "Messages within ±30 minutes of the strongest detected pivot, limited to the two most central afternoon agents."
            ),
            DTOutput("q1_evidence_table")
          )
        )
      )
    )
  )
),


# ============================================================
# PART 5 — ADD THIS INSIDE server <- function(input, output, session)
# ============================================================
# Paste it AFTER your Overview outputs and BEFORE the Question 2 server code.
#
# ------------------------------------------------------------

# ------------------------------------------------------------
# Question 1: Alvin's breach-day reconstruction
# ------------------------------------------------------------

q1_day_data <- reactive({
  q1_prepare_day_data(
    data = communications,
    selected_date = input$q1_focus_date,
    selected_agents = input$q1_agents,
    selected_channels = input$q1_channels,
    selected_risks = input$q1_risks,
    target_view = input$q1_target_view
  )
})

q1_morning_data <- reactive({
  q1_day_data() |>
    filter(q1_period == "Morning")
})

q1_afternoon_data <- reactive({
  q1_day_data() |>
    filter(q1_period == "Afternoon")
})

q1_morning_structure <- reactive({
  q1_build_centrality_network(
    q1_morning_data(),
    top_nodes = input$q1_network_size
  )
})

q1_afternoon_structure <- reactive({
  q1_build_centrality_network(
    q1_afternoon_data(),
    top_nodes = input$q1_network_size
  )
})

output$q1_morning_network <- renderVisNetwork({
  q1_draw_centrality_network(q1_morning_structure())
})

output$q1_afternoon_network <- renderVisNetwork({
  q1_draw_centrality_network(q1_afternoon_structure())
})

output$q1_morning_agents <- renderTable({
  q1_morning_structure()$agent_ranking |>
    slice_head(n = 5)
}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s", rownames = FALSE)

output$q1_afternoon_agents <- renderTable({
  q1_afternoon_structure()$agent_ranking |>
    slice_head(n = 5)
}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s", rownames = FALSE)

q1_phrases <- reactive({
  q1_emerging_bigrams(
    data = q1_day_data(),
    afternoon_network = q1_afternoon_structure(),
    phrase_count = input$q1_phrase_count
  )
})

output$q1_phrase_plot <- renderPlotly({

  phrase_data <- q1_phrases()

  if (nrow(phrase_data) == 0) {
    return(
      q1_empty_plot(
        "No afternoon-emerging phrases match the current filters."
      )
    )
  }

  plot_data <- phrase_data |>
    arrange(Shift)

  plot_ly(
    plot_data,
    x = ~Shift,
    y = ~bigram,
    type = "bar",
    orientation = "h",
    marker = list(color = "#991B1B"),
    text = ~paste0(
      "Phrase: ", bigram,
      "<br>Morning: ", Morning,
      "<br>Afternoon: ", Afternoon,
      "<br>Increase: +", Shift
    ),
    hoverinfo = "text"
  ) |>
    layout(
      xaxis = list(title = "Frequency increase: Afternoon minus Morning"),
      yaxis = list(title = ""),
      margin = list(l = 170, r = 30, b = 60, t = 25),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    )
})

output$q1_phrase_table <- renderDT({

  phrase_data <- q1_phrases()

  if (nrow(phrase_data) == 0) {
    return(
      datatable(
        data.frame(
          Message = "No afternoon-emerging phrases match the current filters."
        ),
        rownames = FALSE,
        options = list(dom = "t")
      )
    )
  }

  datatable(
    phrase_data,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      lengthChange = FALSE,
      scrollX = TRUE,
      order = list(list(3, "desc"))
    )
  )
})

q1_vocabulary_data <- reactive({
  q1_vocabulary_similarity(q1_day_data())
})

q1_top_afternoon_agents <- reactive({
  q1_afternoon_structure()$agent_ranking |>
    slice_head(n = 2) |>
    pull(Agent)
})

q1_adamic_data <- reactive({
  q1_adamic_adar_timeline(
    q1_day_data(),
    q1_top_afternoon_agents()
  )
})

output$q1_similarity_plot <- renderPlotly({

  similarity_data <- q1_vocabulary_data()

  if (nrow(similarity_data) == 0) {
    return(
      q1_empty_plot(
        "At least two 30-minute communication windows are required."
      )
    )
  }

  plot_ly(
    similarity_data,
    x = ~time_bin,
    y = ~jaccard_similarity,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "#2563EB", width = 3),
    marker = list(color = "#2563EB", size = 7),
    text = ~paste0(
      "Time: ", format(time_bin, "%H:%M"),
      "<br>Jaccard similarity: ", round(jaccard_similarity, 3)
    ),
    hoverinfo = "text"
  ) |>
    layout(
      xaxis = list(title = "Time"),
      yaxis = list(
        title = "Vocabulary similarity",
        range = c(0, 1)
      ),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    )
})

output$q1_adamic_plot <- renderPlotly({

  adamic_data <- q1_adamic_data()

  if (nrow(adamic_data) == 0) {
    return(
      q1_empty_plot(
        "Two central afternoon agents are required to calculate this score."
      )
    )
  }

  plot_ly(
    adamic_data,
    x = ~time_bin,
    y = ~adamic_adar,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "#DC2626", width = 3),
    marker = list(color = "#DC2626", size = 7),
    text = ~paste0(
      "Time: ", format(time_bin, "%H:%M"),
      "<br>Shared-connection score: ", round(adamic_adar, 3)
    ),
    hoverinfo = "text"
  ) |>
    layout(
      xaxis = list(title = "Time"),
      yaxis = list(title = "Adamic-Adar-style score"),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    )
})

q1_pivot_time <- reactive({

  vocabulary_data <- q1_vocabulary_data()
  adamic_data <- q1_adamic_data()

  text_pivot <- if (nrow(vocabulary_data) > 0) {
    vocabulary_data |>
      slice_min(jaccard_similarity, n = 1, with_ties = FALSE) |>
      pull(time_bin)
  } else {
    as.POSIXct(NA)
  }

  connection_peak <- if (
    nrow(adamic_data) > 0 &&
    any(adamic_data$adamic_adar > 0)
  ) {
    adamic_data |>
      slice_max(adamic_adar, n = 1, with_ties = FALSE) |>
      pull(time_bin)
  } else {
    as.POSIXct(NA)
  }

  list(
    text_pivot = text_pivot,
    connection_peak = connection_peak,
    anchor = if (!is.na(connection_peak)) connection_peak else text_pivot
  )
})

output$q1_pivot_note <- renderUI({

  pivot <- q1_pivot_time()
  actors <- q1_top_afternoon_agents()

  if (is.na(pivot$anchor)) {
    return(
      div(
        class = "small-note",
        "No pivot window can be identified with the current filters."
      )
    )
  }

  tagList(
    tags$p(
      tags$b("Primary evidence window: "),
      format(pivot$anchor, "%d %B %Y, %H:%M")
    ),
    tags$p(
      tags$b("Most central afternoon agents: "),
      paste(actors, collapse = " and ")
    ),
    tags$p(
      "The message table below shows the surrounding 60-minute window for targeted review."
    )
  )
})

q1_focused_evidence <- reactive({

  pivot <- q1_pivot_time()
  actors <- q1_top_afternoon_agents()
  data <- q1_day_data()

  if (is.na(pivot$anchor) || length(actors) == 0) {
    return(data[0, ])
  }

  data |>
    filter(
      q1_event_time >= pivot$anchor - lubridate::minutes(30),
      q1_event_time <= pivot$anchor + lubridate::minutes(30),
      agent_label %in% actors
    ) |>
    arrange(q1_event_time)
})

output$q1_evidence_table <- renderDT({

  evidence <- q1_focused_evidence()

  if (nrow(evidence) == 0) {
    return(
      datatable(
        data.frame(
          Message = "No messages were found in the detected evidence window."
        ),
        rownames = FALSE,
        options = list(dom = "t")
      )
    )
  }

  evidence_table <- evidence |>
    transmute(
      Time = format(q1_event_time, "%Y-%m-%d %H:%M"),
      Agent = agent_label,
      Role = agent_role,
      Channel = channel,
      `Risk signal` = risk_signal_group,
      `Internal state` = internal_state_deliberating,
      `Message evidence` = if_else(
        nchar(content) > 300,
        paste0(str_sub(content, 1, 300), "…"),
        content
      )
    )

  datatable(
    evidence_table,
    rownames = FALSE,
    filter = "top",
    options = list(
      pageLength = 10,
      scrollX = TRUE
    )
  )
})


# ============================================================
# PART 6 — OPTIONAL CSS FOR www/style.css
# ============================================================
# Paste this at the END of www/style.css.
#
# ------------------------------------------------------------

/* ---------- Q1: Path to Breach ---------- */
#q1_morning_agents,
#q1_afternoon_agents {
  width: 100%;
  margin-top: 8px;
}

#q1_morning_agents th,
#q1_afternoon_agents th {
  background: #FCE7F3;
  color: #831843;
  font-weight: 750;
}

#q1_morning_agents th,
#q1_morning_agents td,
#q1_afternoon_agents th,
#q1_afternoon_agents td {
  padding: 9px 10px;
  border-color: #F3D1E3;
}

