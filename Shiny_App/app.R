pacman::p_load(shiny, dplyr, tidyr, stringr, lubridate, tidytext, visNetwork, DT)

# ============================================================
# LOAD PREPARED DATA
# ============================================================

prepared_data <- readRDS("data/mc1_prepared_data.rds")

# The prepared RDS may either be:
# 1. a data frame itself, or
# 2. a list containing a communications table.

if (is.data.frame(prepared_data)) {
  df_communications <- prepared_data
} else if ("communications" %in% names(prepared_data)) {
  df_communications <- prepared_data$communications
} else {
  stop(
    "Cannot find communications data in mc1_prepared_data.rds. ",
    "Please check whether the table is named communications."
  )
}

# ============================================================
# PREPARE COLUMNS
# ============================================================

# Find timestamp column
if ("timestamp" %in% names(df_communications)) {
  df_communications <- df_communications %>%
    mutate(event_time = as.POSIXct(timestamp, tz = "UTC"))
} else if ("hour" %in% names(df_communications)) {
  df_communications <- df_communications %>%
    mutate(event_time = as.POSIXct(hour, tz = "UTC"))
} else {
  stop("No timestamp or hour column found in the prepared dataset.")
}

# Create fallback columns if some are unavailable
if (!"agent_label" %in% names(df_communications)) {
  df_communications$agent_label <- df_communications$agent_id
}

if (!"agent_role" %in% names(df_communications)) {
  df_communications$agent_role <- "Unknown Role"
}

if (!"channel" %in% names(df_communications)) {
  df_communications$channel <- "Unknown Channel"
}

if (!"content" %in% names(df_communications)) {
  df_communications$content <- ""
}

if (!"internal_state_deliberating" %in% names(df_communications)) {
  df_communications$internal_state_deliberating <- ""
}

if (!"risk_signal_group" %in% names(df_communications)) {
  df_communications$risk_signal_group <- "No Signal"
}

if (!"exposure_risk_level" %in% names(df_communications)) {
  df_communications$exposure_risk_level <- "Unknown"
}

if (!"message_id" %in% names(df_communications)) {
  df_communications$message_id <- seq_len(nrow(df_communications))
}

df_communications <- df_communications %>%
  mutate(
    event_date = as.Date(event_time),
    event_hour = hour(event_time),
    full_text = paste(
      replace_na(internal_state_deliberating, ""),
      replace_na(content, "")
    )
  )

min_date <- min(df_communications$event_date, na.rm = TRUE)
max_date <- max(df_communications$event_date, na.rm = TRUE)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

custom_stop_words <- tibble(
  word = c(
    "legal", "agent", "pr", "intern", "social",
    "media", "judge", "quality", "tenantthread",
    "civicloom", "project", "harborcrest"
  )
)

build_keyword_network <- function(data, top_keywords = 10) {
  
  if (nrow(data) == 0) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }
  
  keyword_data <- data %>%
    select(agent_id, agent_label, full_text) %>%
    filter(!is.na(agent_id), !is.na(full_text), full_text != "") %>%
    unnest_tokens(word, full_text) %>%
    anti_join(get_stopwords(), by = "word") %>%
    anti_join(custom_stop_words, by = "word") %>%
    filter(str_detect(word, "^[a-zA-Z]+$")) %>%
    count(agent_id, agent_label, word, sort = TRUE) %>%
    bind_tf_idf(word, agent_id, n) %>%
    group_by(agent_id, agent_label) %>%
    slice_max(tf_idf, n = top_keywords, with_ties = FALSE) %>%
    ungroup()
  
  if (nrow(keyword_data) == 0) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }
  
  edges <- keyword_data %>%
    mutate(
      from = paste0("agent_", agent_id),
      to = paste0("word_", word),
      title = paste0(
        "Agent: ", agent_label,
        "<br>Keyword: ", word,
        "<br>TF-IDF: ", round(tf_idf, 3)
      ),
      value = tf_idf
    ) %>%
    select(from, to, title, value)
  
  agent_nodes <- keyword_data %>%
    distinct(agent_id, agent_label) %>%
    mutate(
      id = paste0("agent_", agent_id),
      label = agent_label,
      group = "Agent",
      shape = "dot",
      size = 28,
      title = paste0("<b>Agent:</b> ", agent_label)
    ) %>%
    select(id, label, group, shape, size, title)
  
  keyword_nodes <- keyword_data %>%
    distinct(word) %>%
    mutate(
      id = paste0("word_", word),
      label = word,
      group = "Keyword",
      shape = "dot",
      size = 14,
      title = paste0("<b>Keyword:</b> ", word)
    ) %>%
    select(id, label, group, shape, size, title)
  
  nodes <- bind_rows(agent_nodes, keyword_nodes)
  
  list(nodes = nodes, edges = edges)
}

create_network <- function(nodes, edges, input_id) {
  
  if (nrow(nodes) == 0 || nrow(edges) == 0) {
    return(NULL)
  }
  
  visNetwork(
    nodes = nodes,
    edges = edges,
    width = "100%",
    height = "100%"
  ) %>%
    visGroups(
      groupname = "Agent",
      color = list(
        background = "#3155E7",
        border = "#1D3CB4"
      ),
      font = list(color = "white", size = 16, face = "bold")
    ) %>%
    visGroups(
      groupname = "Keyword",
      color = list(
        background = "#A8D5E2",
        border = "#6EAFC1"
      ),
      font = list(color = "#1F2D3D", size = 13)
    ) %>%
    visEdges(
      color = list(color = "#AAB7C4"),
      smooth = FALSE
    ) %>%
    visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ),
      nodesIdSelection = TRUE
    ) %>%
    visPhysics(
      solver = "forceAtlas2Based",
      stabilization = TRUE
    ) %>%
    visLayout(randomSeed = 123) %>%
    visEvents(
      select = paste0(
        "function(properties) {
          Shiny.setInputValue(
            '", input_id, "',
            properties.nodes[0],
            {priority: 'event'}
          );
        }"
      )
    )
}

# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #F7F9FC;
      }

      .title-text {
        color: #1F2D3D;
        font-weight: 700;
      }

      .subtitle-text {
        color: #667085;
        margin-bottom: 20px;
      }

      .section-card {
        background-color: white;
        border: 1px solid #E5E7EB;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }

      .summary-box {
        background-color: #EEF5FF;
        border-left: 5px solid #3155E7;
        padding: 14px;
        border-radius: 4px;
        margin-top: 15px;
      }
    "))
  ),
  
  titlePanel("TenantThread: Behaviour Change Investigation"),
  
  fluidRow(
    column(
      12,
      h3(
        class = "title-text",
        "Question 2: How did behaviour during the embargo-breach period differ from prior behaviour?"
      ),
      p(
        class = "subtitle-text",
        "Compare communication themes, agents and channels across two user-defined time periods."
      )
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Comparison Periods"),
      
      dateRangeInput(
        "baseline_date_range",
        "Baseline period",
        start = min_date,
        end = min(max_date, min_date + 15),
        min = min_date,
        max = max_date
      ),
      
      selectInput(
        "baseline_start_hour",
        "Baseline start time",
        choices = sprintf("%02d:00", 0:23),
        selected = "00:00"
      ),
      
      selectInput(
        "baseline_end_hour",
        "Baseline end time",
        choices = sprintf("%02d:00", 0:23),
        selected = "23:00"
      ),
      
      hr(),
      
      dateRangeInput(
        "comparison_date_range",
        "Comparison period",
        start = max(min_date, max_date - 1),
        end = max_date,
        min = min_date,
        max = max_date
      ),
      
      selectInput(
        "comparison_start_hour",
        "Comparison start time",
        choices = sprintf("%02d:00", 0:23),
        selected = "00:00"
      ),
      
      selectInput(
        "comparison_end_hour",
        "Comparison end time",
        choices = sprintf("%02d:00", 0:23),
        selected = "23:00"
      ),
      
      hr(),
      
      h4("Shared Filters"),
      
      selectizeInput(
        "agent_filter",
        "Agent / role",
        choices = c(
          "All" = "All",
          sort(unique(df_communications$agent_label)),
          sort(unique(df_communications$agent_role))
        ),
        selected = "All",
        multiple = TRUE
      ),
      
      selectizeInput(
        "channel_filter",
        "Channel",
        choices = sort(unique(df_communications$channel)),
        selected = sort(unique(df_communications$channel)),
        multiple = TRUE
      ),
      
      checkboxGroupInput(
        "risk_filter",
        "Risk-signal level",
        choices = sort(unique(df_communications$risk_signal_group)),
        selected = sort(unique(df_communications$risk_signal_group))
      ),
      
      checkboxGroupInput(
        "exposure_filter",
        "Exposure-risk level",
        choices = sort(unique(df_communications$exposure_risk_level)),
        selected = sort(unique(df_communications$exposure_risk_level))
      ),
      
      sliderInput(
        "keyword_count",
        "Keywords per agent",
        min = 3,
        max = 15,
        value = 8
      ),
      
      hr(),
      
      p(
        "Click an agent or keyword node to show related messages in the evidence tabs.",
        style = "font-size: 12px; color: #667085;"
      )
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        
        column(
          6,
          
          div(
            class = "section-card",
            
            h4("Baseline Network Structure"),
            
            p(
              textOutput("baseline_label"),
              style = "color: #667085;"
            ),
            
            visNetworkOutput(
              "baseline_network",
              height = "500px"
            )
          )
        ),
        
        column(
          6,
          
          div(
            class = "section-card",
            
            h4("Comparison Network Structure"),
            
            p(
              textOutput("comparison_label"),
              style = "color: #667085;"
            ),
            
            visNetworkOutput(
              "comparison_network",
              height = "500px"
            )
          )
        )
      ),
      
      tabsetPanel(
        
        tabPanel(
          "Related Messages",
          
          br(),
          
          DTOutput("related_messages_table")
        ),
        
        tabPanel(
          "Behaviour Change Summary",
          
          br(),
          
          DTOutput("behaviour_summary_table"),
          
          div(
            class = "summary-box",
            
            h4("Interpretation"),
            
            uiOutput("behaviour_interpretation")
          )
        ),
        
        tabPanel(
          "Selected Node Detail",
          
          br(),
          
          uiOutput("selected_node_detail")
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  filter_period <- function(data, date_range, start_hour, end_hour) {
    
    start_hour_num <- as.numeric(str_sub(start_hour, 1, 2))
    end_hour_num <- as.numeric(str_sub(end_hour, 1, 2))
    
    data %>%
      filter(
        event_date >= date_range[1],
        event_date <= date_range[2],
        event_hour >= start_hour_num,
        event_hour <= end_hour_num
      )
  }
  
  apply_shared_filters <- function(data) {
    
    selected_agents <- input$agent_filter
    
    filtered_data <- data %>%
      filter(
        channel %in% input$channel_filter,
        risk_signal_group %in% input$risk_filter,
        exposure_risk_level %in% input$exposure_filter
      )
    
    if (!"All" %in% selected_agents && length(selected_agents) > 0) {
      filtered_data <- filtered_data %>%
        filter(
          agent_label %in% selected_agents |
            agent_role %in% selected_agents
        )
    }
    
    filtered_data
  }
  
  baseline_messages <- reactive({
    
    df_communications %>%
      filter_period(
        input$baseline_date_range,
        input$baseline_start_hour,
        input$baseline_end_hour
      ) %>%
      apply_shared_filters()
  })
  
  comparison_messages <- reactive({
    
    df_communications %>%
      filter_period(
        input$comparison_date_range,
        input$comparison_start_hour,
        input$comparison_end_hour
      ) %>%
      apply_shared_filters()
  })
  
  baseline_network_data <- reactive({
    build_keyword_network(
      baseline_messages(),
      input$keyword_count
    )
  })
  
  comparison_network_data <- reactive({
    build_keyword_network(
      comparison_messages(),
      input$keyword_count
    )
  })
  
  output$baseline_label <- renderText({
    paste0(
      format(input$baseline_date_range[1], "%d %b %Y"),
      " to ",
      format(input$baseline_date_range[2], "%d %b %Y"),
      " | ",
      input$baseline_start_hour,
      "–",
      input$baseline_end_hour
    )
  })
  
  output$comparison_label <- renderText({
    paste0(
      format(input$comparison_date_range[1], "%d %b %Y"),
      " to ",
      format(input$comparison_date_range[2], "%d %b %Y"),
      " | ",
      input$comparison_start_hour,
      "–",
      input$comparison_end_hour
    )
  })
  
  output$baseline_network <- renderVisNetwork({
    
    network <- baseline_network_data()
    
    validate(
      need(
        nrow(network$nodes) > 0,
        "No baseline communication data matches the selected filters."
      )
    )
    
    create_network(
      network$nodes,
      network$edges,
      "baseline_selected_node"
    )
  })
  
  output$comparison_network <- renderVisNetwork({
    
    network <- comparison_network_data()
    
    validate(
      need(
        nrow(network$nodes) > 0,
        "No comparison communication data matches the selected filters."
      )
    )
    
    create_network(
      network$nodes,
      network$edges,
      "comparison_selected_node"
    )
  })
  
  selected_node <- reactive({
    
    if (!is.null(input$comparison_selected_node)) {
      return(input$comparison_selected_node)
    }
    
    if (!is.null(input$baseline_selected_node)) {
      return(input$baseline_selected_node)
    }
    
    NULL
  })
  
  related_messages <- reactive({
    
    data <- bind_rows(
      baseline_messages() %>%
        mutate(period = "Baseline"),
      comparison_messages() %>%
        mutate(period = "Comparison")
    )
    
    node <- selected_node()
    
    if (is.null(node) || node == "") {
      return(data)
    }
    
    if (str_detect(node, "^agent_")) {
      
      selected_agent_id <- str_remove(node, "^agent_")
      
      return(
        data %>%
          filter(as.character(agent_id) == selected_agent_id)
      )
    }
    
    if (str_detect(node, "^word_")) {
      
      selected_word <- str_remove(node, "^word_")
      
      return(
        data %>%
          filter(
            str_detect(
              tolower(full_text),
              fixed(tolower(selected_word))
            )
          )
      )
    }
    
    data
  })
  
  output$related_messages_table <- renderDT({
    
    data <- related_messages() %>%
      select(
        period,
        message_id,
        event_time,
        agent_label,
        agent_role,
        channel,
        exposure_risk_level,
        risk_signal_group,
        content
      ) %>%
      arrange(desc(event_time))
    
    datatable(
      data,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 8,
        scrollX = TRUE
      )
    )
  })
  
  output$behaviour_summary_table <- renderDT({
    
    baseline_summary <- baseline_messages() %>%
      summarise(
        Period = "Baseline",
        Messages = n(),
        Unique_Agents = n_distinct(agent_id),
        Unique_Channels = n_distinct(channel),
        High_Risk_Messages = sum(
          risk_signal_group == "High Signal",
          na.rm = TRUE
        ),
        Average_Risk_Score = if (
          "risk_signal_score" %in% names(.)
        ) {
          mean(risk_signal_score, na.rm = TRUE)
        } else {
          NA
        }
      )
    
    comparison_summary <- comparison_messages() %>%
      summarise(
        Period = "Comparison",
        Messages = n(),
        Unique_Agents = n_distinct(agent_id),
        Unique_Channels = n_distinct(channel),
        High_Risk_Messages = sum(
          risk_signal_group == "High Signal",
          na.rm = TRUE
        ),
        Average_Risk_Score = if (
          "risk_signal_score" %in% names(.)
        ) {
          mean(risk_signal_score, na.rm = TRUE)
        } else {
          NA
        }
      )
    
    summary_data <- bind_rows(
      baseline_summary,
      comparison_summary
    )
    
    datatable(
      summary_data,
      rownames = FALSE,
      options = list(
        dom = "t",
        scrollX = TRUE
      )
    )
  })
  
  output$behaviour_interpretation <- renderUI({
    
    baseline_n <- nrow(baseline_messages())
    comparison_n <- nrow(comparison_messages())
    
    if (baseline_n == 0 || comparison_n == 0) {
      return(
        tags$p(
          "Select periods containing messages to generate a behaviour comparison."
        )
      )
    }
    
    change_pct <- round(
      ((comparison_n - baseline_n) / baseline_n) * 100,
      1
    )
    
    message_text <- if (comparison_n > baseline_n) {
      paste0(
        "Communication activity increased by ",
        abs(change_pct),
        "% in the comparison period relative to the baseline period."
      )
    } else if (comparison_n < baseline_n) {
      paste0(
        "Communication activity decreased by ",
        abs(change_pct),
        "% in the comparison period relative to the baseline period."
      )
    } else {
      "Communication activity was similar across the two selected periods."
    }
    
    tags$p(message_text)
  })
  
  output$selected_node_detail <- renderUI({
    
    node <- selected_node()
    
    if (is.null(node) || node == "") {
      return(
        tags$p(
          "Click an agent or keyword node in either network to view its related messages."
        )
      )
    }
    
    if (str_detect(node, "^agent_")) {
      node_type <- "Agent"
      node_label <- str_remove(node, "^agent_")
    } else {
      node_type <- "Keyword"
      node_label <- str_remove(node, "^word_")
    }
    
    related_count <- nrow(related_messages())
    
    tagList(
      h4(paste(node_type, "Selected")),
      p(tags$b("Value: "), node_label),
      p(tags$b("Related messages: "), related_count),
      p(
        "Use the Related Messages tab to inspect the communication evidence associated with this selected node."
      )
    )
  })
}

shinyApp(ui = ui, server = server)