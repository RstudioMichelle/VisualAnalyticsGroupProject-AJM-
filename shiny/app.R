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

q1_alvin_raw_communications <- if (is.data.frame(prepared_data)) {
  prepared_data
} else if ("communications" %in% names(prepared_data)) {
  prepared_data$communications
} else {
  stop("Cannot find a communications table inside mc1_prepared_data.rds.")
}

communications <- if (is.data.frame(prepared_data)) {
  prepared_data
} else if ("communications" %in% names(prepared_data)) {
  prepared_data$communications
} else {
  stop("Cannot find a communications table inside mc1_prepared_data.rds.")
}

if (!"agent_id" %in% names(communications)) {
  stop("The communications data must contain an agent_id column.")
}

if (!"hour" %in% names(communications) && !"timestamp" %in% names(communications)) {
  stop("The communications data must contain either an hour or timestamp column.")
}

if (!"agent_label"                 %in% names(communications)) communications$agent_label <- communications$agent_id
if (!"agent_role"                  %in% names(communications)) communications$agent_role  <- "Unknown"
if (!"channel"                     %in% names(communications)) communications$channel     <- "Unknown"
if (!"content"                     %in% names(communications)) communications$content     <- ""
if (!"internal_state_deliberating" %in% names(communications)) communications$internal_state_deliberating <- ""
if (!"risk_signal_group"           %in% names(communications)) communications$risk_signal_group <- "No Signal"

date_source <- if ("hour" %in% names(communications)) communications$hour else communications$timestamp

communications <- communications |>
  mutate(
    agent_id    = as.character(agent_id),
    agent_label = as.character(agent_label),
    agent_label = if_else(is.na(agent_label) | str_trim(agent_label) == "", agent_id, agent_label),
    agent_role  = replace_na(as.character(agent_role), "Unknown"),
    channel     = replace_na(as.character(channel),    "Unknown"),
    content     = replace_na(as.character(content),    ""),
    internal_state_deliberating = replace_na(as.character(internal_state_deliberating), ""),
    risk_signal_group = replace_na(as.character(risk_signal_group), "No Signal"),
    event_date  = as.Date(substr(as.character(date_source), 1, 10)),
    event_hour  = as.integer(format(as.POSIXct(date_source, tz = "UTC"), "%H")),
    full_text   = str_squish(paste(internal_state_deliberating, content))
  ) |>
  filter(!is.na(event_date))

if (nrow(communications) == 0) stop("No valid dates could be read from the hour/timestamp column.")

q1_parse_datetime <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
  x <- as.character(x)
  parsed <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE))
  if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd_hm(x, tz = "UTC", quiet = TRUE))
  if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd(x, tz = "UTC", quiet = TRUE))
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

q1_recipient_column <- intersect(c("recipients","recipient","recipient_label","to"), names(communications))
communications$q1_recipient <- if (length(q1_recipient_column) > 0)
  as.character(communications[[q1_recipient_column[1]]]) else NA_character_

q1_default_date <- if (as.Date("2046-06-05") %in% communications$event_date)
  as.Date("2046-06-05") else max(communications$event_date, na.rm = TRUE)

q1_target_choices <- c("Agent -> Channel" = "Channel")
if (any(!is.na(communications$q1_recipient) & communications$q1_recipient != ""))
  q1_target_choices <- c(q1_target_choices, "Agent -> Recipient" = "Recipient")

min_date <- min(communications$event_date)
max_date <- max(communications$event_date)
breach_date <- as.Date("2046-06-04")
baseline_start_default <- min_date
baseline_end_default   <- min(max_date, breach_date - 1)
if (baseline_end_default < min_date) baseline_end_default <- min_date
critical_start_default <- max(min_date, breach_date)
if (critical_start_default > max_date) critical_start_default <- min_date
critical_end_default <- max_date

all_agents   <- sort(unique(communications$agent_label))
all_roles    <- sort(unique(communications$agent_role))
all_channels <- sort(unique(communications$channel))
all_risks    <- sort(unique(communications$risk_signal_group))

agent_palette <- c(
  "pr_agent"="$005f73","pr_intern_agent"="#0a9396","social_media_agent"="#9b5de5",
  "legal_agent"="#1b4332","judge_agent"="#6a040f","quality_agent"="#ca6702","intern_agent"="#f15bb5"
)
custom_stop_words <- tibble(word=c("legal","agent","pr","intern","social","media","judge","quality"))

# ============================================================
# HELPER FUNCTIONS
# ============================================================

filter_messages <- function(data, date_range, selected_agents, selected_channels, selected_risks) {
  if (is.null(date_range) || length(date_range) != 2) return(data[0,])
  if (is.null(selected_channels) || length(selected_channels)==0) selected_channels <- unique(data$channel)
  if (is.null(selected_risks)    || length(selected_risks)==0)    selected_risks    <- unique(data$risk_signal_group)
  filtered <- data |>
    filter(event_date >= as.Date(date_range[1]), event_date <= as.Date(date_range[2]),
           channel %in% selected_channels, risk_signal_group %in% selected_risks)
  if (!is.null(selected_agents) && length(selected_agents)>0 && !"All" %in% selected_agents)
    filtered <- filtered |> filter(agent_label %in% selected_agents | agent_role %in% selected_agents)
  filtered
}

message_table <- function(data) {
  data |> transmute(
    Date = format(event_date,"%Y-%m-%d"), Agent=agent_label, Role=agent_role,
    Channel=channel, `Risk signal`=risk_signal_group,
    `Message evidence`=if_else(nchar(full_text)>260, paste0(str_sub(full_text,1,260),"\u2026"), full_text)
  )
}

empty_network <- function(message, height="560px") {
  visNetwork(
    nodes=data.frame(id="no_data",label=message,shape="box",stringsAsFactors=FALSE),
    edges=data.frame(from=character(),to=character(),stringsAsFactors=FALSE),
    width="100%",height=height
  ) |> visNodes(color=list(background="#F1F5F9",border="#94A3B8"),font=list(color="#475569",size=15)) |>
    visPhysics(enabled=FALSE)
}

# ============================================================
# QUESTION 2 PRIMARY VISUAL
# ============================================================

build_alvin_keyword_network <- function(data, keywords_per_agent=10) {
  words <- data |>
    filter(!is.na(agent_id), full_text!="") |>
    select(agent_id, full_text) |>
    unnest_tokens(word, full_text) |>
    anti_join(get_stopwords(), by="word") |>
    anti_join(custom_stop_words, by="word") |>
    filter(str_detect(word,"^[a-zA-Z]+$")) |>
    count(agent_id, word, name="n") |>
    bind_tf_idf(word, agent_id, n) |>
    group_by(agent_id) |> slice_max(tf_idf, n=keywords_per_agent, with_ties=FALSE) |> ungroup()
  if (!nrow(words)) return(list(nodes=data.frame(),edges=data.frame()))
  agent_nodes <- words |> distinct(agent_id) |>
    transmute(id=paste0("agent::",agent_id),label=agent_id,group="Agent",value=35,
              color=coalesce(unname(agent_palette[agent_id]),"#3155E7"),
              title=paste0("<b>Agent:</b> ",agent_id))
  keyword_nodes <- words |> distinct(word) |>
    transmute(id=paste0("keyword::",word),label=word,group="Keyword",value=15,
              color="#9ecae1",title=paste0("<b>Keyword:</b> ",word))
  network_edges <- words |>
    mutate(edge_strength=if_else(is.finite(tf_idf)&tf_idf>0,tf_idf,as.numeric(n))) |>
    transmute(from=paste0("agent::",agent_id),to=paste0("keyword::",word),
              value=pmax(1,edge_strength*12),width=pmax(1,edge_strength*6),
              title=paste0("<b>Agent:</b> ",agent_id,"<br><b>Keyword:</b> ",word,
                           "<br><b>Occurrences:</b> ",n,"<br><b>TF-IDF:</b> ",round(tf_idf,3)))
  list(nodes=bind_rows(agent_nodes,keyword_nodes),edges=network_edges)
}

draw_alvin_keyword_network <- function(network_data, height="560px") {
  if (!nrow(network_data$nodes))
    return(empty_network("No eligible terms were found for the selected period and filters.",height))
  visNetwork(nodes=network_data$nodes,edges=network_data$edges,width="100%",height=height) |>
    visGroups(groupname="Agent",  font=list(size=16,face="bold",color="white")) |>
    visGroups(groupname="Keyword",font=list(size=12,color="#24364B")) |>
    visEdges(smooth=FALSE,color=list(color="#BFC9D4",opacity=0.8)) |>
    visOptions(highlightNearest=list(enabled=TRUE,degree=1,hover=TRUE),nodesIdSelection=TRUE) |>
    visPhysics(solver="forceAtlas2Based",stabilization=TRUE) |>
    visLayout(randomSeed=123)
}

q1_scale_size <- function(x,lower=0,upper=30) {
  if (!length(x)) return(numeric())
  x <- replace_na(as.numeric(x),0)
  if (max(x)==min(x)) return(rep((lower+upper)/2,length(x)))
  lower+(x-min(x))/(max(x)-min(x))*(upper-lower)
}

q1_prepare_day_data <- function(data,selected_date,selected_agents,selected_channels,selected_risks,target_view="Channel") {
  selected_data <- filter_messages(data,as.Date(c(selected_date,selected_date)),
                                   selected_agents,selected_channels,selected_risks)
  if (!nrow(selected_data)) return(selected_data)
  selected_data |>
    mutate(q1_hour=lubridate::hour(q1_event_time),
           q1_period=case_when(q1_hour<12~"Morning",q1_hour>=12~"Afternoon",TRUE~"Unknown"),
           q1_target=case_when(target_view=="Recipient"&!is.na(q1_recipient)&q1_recipient!=""~q1_recipient,
                               TRUE~channel),
           q1_risk_weight=case_when(risk_signal_group=="High Signal"~4,risk_signal_group=="Moderate Signal"~2,
                                    risk_signal_group=="Low Signal"~1,TRUE~0)) |>
    filter(!is.na(q1_event_time),!is.na(q1_target),q1_target!="",q1_target!="Unknown",q1_period!="Unknown")
}

q1_build_centrality_network <- function(data,top_nodes=8) {
  empty_result <- list(nodes=data.frame(),edges=data.frame(),agent_ranking=data.frame(),target_ranking=data.frame())
  if (!nrow(data)) return(empty_result)
  edge_summary <- data |>
    group_by(agent_label,q1_target) |>
    summarise(interactions=n(),total_risk=sum(q1_risk_weight,na.rm=TRUE),
              high_risk_messages=sum(risk_signal_group=="High Signal",na.rm=TRUE),.groups="drop") |>
    mutate(from=paste0("agent::",agent_label),to=paste0("target::",q1_target))
  if (!nrow(edge_summary)) return(empty_result)
  if (top_nodes < length(unique(edge_summary$agent_label))) {
    top_agents <- edge_summary |> group_by(agent_label) |>
      summarise(total_interactions=sum(interactions),.groups="drop") |>
      slice_max(total_interactions,n=top_nodes,with_ties=FALSE) |> pull(agent_label)
    edge_summary <- edge_summary |> filter(agent_label %in% top_agents)
  }
  graph_object <- igraph::graph_from_data_frame(edge_summary|>select(from,to),directed=FALSE)
  centrality_scores <- tryCatch(igraph::subgraph_centrality(graph_object),
                                error=function(e) igraph::degree(graph_object,mode="all"))
  node_ids <- igraph::V(graph_object)$name
  node_connections <- bind_rows(
    edge_summary|>group_by(agent_label)|>
      summarise(connections=n_distinct(q1_target),interactions=sum(interactions),.groups="drop")|>
      transmute(id=paste0("agent::",agent_label),connections,interactions),
    edge_summary|>group_by(q1_target)|>
      summarise(connections=n_distinct(agent_label),interactions=sum(interactions),.groups="drop")|>
      transmute(id=paste0("target::",q1_target),connections,interactions)
  )
  nodes <- tibble(id=node_ids,
                  node_type=if_else(str_detect(node_ids,"^agent::"),"Agent","Target"),
                  label=str_remove(node_ids,"^(agent|target)::"),
                  subgraph_centrality=as.numeric(centrality_scores[node_ids])) |>
    left_join(node_connections,by="id") |>
    mutate(subgraph_centrality=replace_na(subgraph_centrality,0),
           connections=replace_na(connections,0L),interactions=replace_na(interactions,0L),
           shape=if_else(node_type=="Agent","square","dot"),
           color=if_else(node_type=="Agent","#F472B6","#8ECAE6"),
           size=if_else(node_type=="Agent",22+q1_scale_size(subgraph_centrality,0,30),
                        14+q1_scale_size(subgraph_centrality,0,20)),
           title=paste0("<b>",label,"</b><br>Type: ",if_else(node_type=="Agent","Agent","Channel / Recipient"),
                        "<br>Subgraph centrality: ",round(subgraph_centrality,2),
                        "<br>Connections: ",connections,"<br>Interactions: ",interactions))
  edges <- edge_summary |>
    transmute(from,to,value=interactions,width=pmax(1,log1p(interactions)*2),arrows="to",
              title=paste0("<b>Interactions:</b> ",interactions,"<br><b>Total risk score:</b> ",total_risk,
                           "<br><b>High-risk messages:</b> ",high_risk_messages))
  agent_ranking <- nodes|>filter(node_type=="Agent")|>
    transmute(Agent=label,`Subgraph centrality`=round(subgraph_centrality,2),
              Connections=connections,Interactions=interactions)|>arrange(desc(`Subgraph centrality`),desc(Interactions))
  target_ranking <- nodes|>filter(node_type=="Target")|>
    transmute(`Channel / Recipient`=label,`Subgraph centrality`=round(subgraph_centrality,2),
              Connections=connections,Interactions=interactions)|>arrange(desc(`Subgraph centrality`),desc(Interactions))
  list(nodes=nodes,edges=edges,agent_ranking=agent_ranking,target_ranking=target_ranking)
}

q1_draw_centrality_network <- function(network_data,height="600px") {
  if (!nrow(network_data$nodes))
    return(empty_network("No interactions match the selected filters and time period.",height))
  visNetwork(network_data$nodes,network_data$edges,width="100%",height=height) |>
    visNodes(borderWidth=1,color=list(border="#64748B")) |>
    visEdges(color=list(color="#94A3B8",highlight="#DC2626"),smooth=FALSE) |>
    visOptions(highlightNearest=list(enabled=TRUE,degree=1,hover=TRUE),nodesIdSelection=FALSE) |>
    visPhysics(solver="forceAtlas2Based",
               forceAtlas2Based=list(gravitationalConstant=-65,springLength=115,springConstant=0.04),
               stabilization=TRUE) |>
    visLayout(randomSeed=42)
}

q1_alvin_prepare_raw <- function(data) {
  data <- as_tibble(data)
  required_columns <- c("agent_label","channel","content")
  missing_columns  <- setdiff(required_columns,names(data))
  if (length(missing_columns)>0)
    stop("The communications table is missing columns: ",paste(missing_columns,collapse=", "))
  if (!"communication_date" %in% names(data)) {
    source_time <- if ("hour" %in% names(data)) data$hour else data$timestamp
    data$communication_date <- as.character(as.Date(substr(as.character(source_time),1,10)))
  }
  if (!"communication_hour" %in% names(data)) {
    source_time <- if ("timestamp" %in% names(data)) data$timestamp else data$hour
    data$communication_hour <- lubridate::hour(
      suppressWarnings(lubridate::ymd_hms(as.character(source_time),quiet=TRUE)))
  }
  if (!"timestamp" %in% names(data)) {
    source_time <- if ("hour" %in% names(data)) data$hour else data$communication_date
    data$timestamp <- as.character(source_time)
  }
  if (!"recipients"         %in% names(data)) data$recipients        <- NA_character_
  if (!"risk_signal_score"  %in% names(data)) data$risk_signal_score <- 0
  data
}

q1_alvin_subgraph_scores <- function(data,period_name) {
  period_df <- if (period_name=="Morning") data|>filter(communication_hour<12) else data|>filter(communication_hour>=12)
  edges <- period_df |>
    mutate(target=ifelse(!is.na(channel)&!str_detect(tolower(channel),"dm"),channel,recipients)) |>
    filter(!is.na(target)) |>
    group_by(agent_label,target) |>
    summarise(total_risk_score=sum(risk_signal_score,na.rm=TRUE),interaction_count=n(),.groups="drop") |>
    mutate(from=agent_label,to=target,value=ifelse(total_risk_score<=0,0.1,total_risk_score),arrows="to")
  if (!nrow(edges)) return(numeric())
  igraph::subgraph_centrality(igraph::graph_from_data_frame(edges,directed=TRUE))
}

q1_alvin_war_room <- function(raw_data) {
  june5_data <- q1_alvin_prepare_raw(raw_data) |>
    filter(as.character(communication_date)=="2046-06-05") |>
    mutate(timestamp=suppressWarnings(lubridate::ymd_hms(timestamp,quiet=TRUE)))
  morning_scores   <- q1_alvin_subgraph_scores(june5_data,"Morning")
  afternoon_scores <- q1_alvin_subgraph_scores(june5_data,"Afternoon")
  if (!length(morning_scores)||!length(afternoon_scores))
    return(list(data=june5_data[0,],morning_scores=morning_scores,afternoon_scores=afternoon_scores,
                agents=character(),channels=character()))
  sorted_morning_scores   <- sort(morning_scores,  decreasing=TRUE)
  sorted_afternoon_scores <- sort(afternoon_scores,decreasing=TRUE)
  afternoon_agent_only_scores   <- sorted_morning_scores[grepl("Agent",names(sorted_afternoon_scores))]
  afternoon_channel_only_scores <- sorted_morning_scores[grepl("huddle|post|chat",names(sorted_afternoon_scores))]
  list(data=june5_data,morning_scores=morning_scores,afternoon_scores=afternoon_scores,
       agents=names(afternoon_agent_only_scores)[1:3],channels=names(afternoon_channel_only_scores)[1:3])
}

q1_alvin_emerging_phrases <- function(raw_data) {
  war_room <- q1_alvin_war_room(raw_data)
  if (!nrow(war_room$data)||!length(war_room$agents)||!length(war_room$channels))
    return(tibble(bigram=character(),Morning=integer(),Afternoon=integer(),shift_score=integer()))
  war_room$data |>
    filter(agent_label %in% war_room$agents, channel %in% war_room$channels) |>
    mutate(time_period=ifelse(communication_hour<12,"Morning","Afternoon")) |>
    unnest_tokens(bigram,content,token="ngrams",n=2) |>
    separate(bigram,c("word1","word2"),sep=" ") |>
    filter(!word1 %in% tidytext::stop_words$word,!word2 %in% tidytext::stop_words$word) |>
    unite(bigram,word1,word2,sep=" ") |>
    count(time_period,bigram) |>
    pivot_wider(names_from=time_period,values_from=n,values_fill=0) |>
    mutate(shift_score=Afternoon-Morning) |>
    filter(shift_score>0) |>
    arrange(desc(shift_score)) |>
    slice_head(n=15)
}

q1_vocabulary_similarity <- function(data) {
  empty_result <- tibble(time_bin=as.POSIXct(character()),jaccard_similarity=numeric())
  if (!nrow(data)) return(empty_result)
  bin_words <- data |>
    mutate(time_bin=lubridate::floor_date(q1_event_time,"30 minutes")) |>
    select(time_bin,full_text) |>
    unnest_tokens(word,full_text) |>
    anti_join(tidytext::stop_words,by="word") |>
    filter(str_detect(word,"^[a-zA-Z]+$")) |>
    group_by(time_bin) |> summarise(words=list(unique(word)),.groups="drop") |> arrange(time_bin)
  if (nrow(bin_words)<2) return(empty_result)
  purrr::map_dfr(seq_len(nrow(bin_words)-1), function(i) {
    words_a <- bin_words$words[[i]]; words_b <- bin_words$words[[i+1]]
    union_size <- length(union(words_a,words_b))
    tibble(time_bin=bin_words$time_bin[[i+1]],
           jaccard_similarity=if(union_size==0) 0 else length(intersect(words_a,words_b))/union_size)
  })
}

q1_adamic_adar_timeline <- function(data,top_agents) {
  empty_result <- tibble(time_bin=as.POSIXct(character()),adamic_adar=numeric())
  if (!nrow(data)||length(top_agents)<2) return(empty_result)
  selected_actors <- top_agents[1:2]
  data |>
    mutate(time_bin=lubridate::floor_date(q1_event_time,"30 minutes")) |>
    group_by(time_bin) |>
    group_modify(~{
      edges <- .x|>filter(agent_label %in% selected_actors)|>distinct(agent_label,q1_target)
      if (!nrow(edges)||!all(selected_actors %in% edges$agent_label)) return(tibble(adamic_adar=0))
      targets_a <- edges|>filter(agent_label==selected_actors[1])|>pull(q1_target)
      targets_b <- edges|>filter(agent_label==selected_actors[2])|>pull(q1_target)
      shared_targets <- intersect(targets_a,targets_b)
      if (!length(shared_targets)) return(tibble(adamic_adar=0))
      target_degrees <- .x|>distinct(agent_label,q1_target)|>count(q1_target,name="target_degree")
      score <- target_degrees|>filter(q1_target %in% shared_targets)|>
        summarise(value=sum(1/log(pmax(target_degree,2))))|>pull(value)
      tibble(adamic_adar=replace_na(score,0))
    }) |>
    ungroup()|>arrange(time_bin)
}

q1_empty_plot <- function(message) {
  plot_ly()|>layout(xaxis=list(visible=FALSE),yaxis=list(visible=FALSE),
                    annotations=list(text=message,x=0.5,y=0.5,showarrow=FALSE,
                                     font=list(size=15,color="#627D98")),
                    paper_bgcolor="#FFFFFF",plot_bgcolor="#FFFFFF")
}

# ============================================================
# SHARED COLOUR SCALE FOR Q3
# Red=High Signal, Orange=Moderate Signal, Green=Low Signal, Grey=No Signal
# ============================================================
Q3_RISK_COLOURS <- c(
  "High Signal"     = "#C00000",
  "Moderate Signal" = "#E8820C",
  "Low Signal"      = "#4CAF50",
  "No Signal"       = "#BDBDBD"
)
Q3_RISK_LEVELS <- c("High Signal","Moderate Signal","Low Signal","No Signal")

# ============================================================
# USER INTERFACE
# ============================================================

ui <- navbarPage(
  title = "TenantThread Dashboard",
  theme = bs_theme(version=5,primary="#3155E7",bg="#F7F9FC",fg="#24364B"),
  header = tags$head(
    tags$link(rel="stylesheet",type="text/css",href="style.css"),
    tags$style(HTML("
      .filter-panel{background:#FFFFFF;border:1px solid #D9E2EC;border-radius:8px;
        padding:16px;box-shadow:0 2px 5px rgba(16,42,67,0.05);}
      .filter-panel h3{margin-top:0;font-size:21px;font-weight:700;color:#102A43;}
      .guide-box{background:#EEF7FC;border-left:4px solid #55A9C8;border-radius:4px;
        padding:12px 14px;margin-bottom:16px;color:#486581;}
      .primary-tag,.support-tag{display:inline-block;padding:4px 8px;border-radius:4px;
        font-size:11px;font-weight:700;text-transform:uppercase;margin-bottom:8px;}
      .primary-tag{background:#E6F6FF;color:#1F5A8A;}
      .support-tag{background:#FFF3D6;color:#8A5D00;}
      .small-note{font-size:13px;color:#627D98;margin-bottom:8px;}
    "))
  ),
  
  # ── Overview ────────────────────────────────────────────────────────────────
  tabPanel("Overview", br(),
           fluidRow(
             column(width=8,
                    div(class="ov-hero-card",
                        div(class="ov-hero-title","TenantThread: Embargo Breach Investigation"),
                        div(class="ov-hero-subtitle","Explore communication activity, behavioural changes, warning signals, and supporting evidence surrounding the embargo breach."),
                        fluidRow(
                          column(6,div(class="ov-period-card",div(class="ov-period-label",icon("calendar")," Baseline Period"),div(class="ov-period-value","17 May 2046 \u2013 3 June 2046"))),
                          column(6,div(class="ov-period-card",div(class="ov-period-label",icon("calendar")," Critical Period"),div(class="ov-period-value","4 June 2046 \u2013 5 June 2046")))
                        )
                    )
             ),
             column(width=4,
                    div(class="ov-side-card",
                        div(class="ov-side-title",icon("bullseye")," What this dashboard helps answer"),
                        tags$ul(class="ov-side-list",
                                tags$li(icon("check-circle")," How did the breach unfold and who was involved?"),
                                tags$li(icon("check-circle")," How did communication behaviour change?"),
                                tags$li(icon("check-circle")," What were the leading warning signals?"),
                                tags$li(icon("check-circle")," What evidence supports the findings?")
                        )
                    )
             )
           ),
           br(),
           fluidRow(
             column(3,div(class="ov-kpi-card kpi-blue",div(class="ov-kpi-icon",icon("comment-alt")),div(class="ov-kpi-body",div(class="ov-kpi-title","Total Messages"),div(class="ov-kpi-value",textOutput("total_messages")),div(class="ov-kpi-sub","Communication records analysed")))),
             column(3,div(class="ov-kpi-card kpi-green",div(class="ov-kpi-icon",icon("users")),div(class="ov-kpi-body",div(class="ov-kpi-title","Unique Agents"),div(class="ov-kpi-value",textOutput("total_agents")),div(class="ov-kpi-sub","Agents involved in communication")))),
             column(3,div(class="ov-kpi-card kpi-purple",div(class="ov-kpi-icon",icon("hashtag")),div(class="ov-kpi-body",div(class="ov-kpi-title","Channels"),div(class="ov-kpi-value",textOutput("total_channels")),div(class="ov-kpi-sub","Communication channels monitored")))),
             column(3,div(class="ov-kpi-card kpi-red",div(class="ov-kpi-icon",icon("exclamation-triangle")),div(class="ov-kpi-body",div(class="ov-kpi-title","High-Risk Messages"),div(class="ov-kpi-value",textOutput("high_risk_messages")),div(class="ov-kpi-sub","Messages requiring investigation"))))
           ),
           br(),
           div(class="ov-section-title",icon("map-marked-alt")," Investigation Roadmap"),
           div(class="ov-section-subtitle","Explore the analysis in four connected steps."),
           br(),
           fluidRow(
             column(3,div(class="ov-roadmap-card",div(class="ov-roadmap-number blue-dot","1"),div(class="ov-roadmap-title","Path to Breach"),div(class="ov-roadmap-text","Reconstruct the sequence of events and communication activity leading to the inappropriate release."),div(class="ov-roadmap-mini","Early signals  \u2022  Escalation  \u2022  Breach"),div(class="ov-roadmap-link","Explore Path to Breach ",icon("arrow-right")))),
             column(3,div(class="ov-roadmap-card",div(class="ov-roadmap-number green-dot","2"),div(class="ov-roadmap-title","Behaviour Change"),div(class="ov-roadmap-text","Compare agent vocabulary, relationships, and channels before and during the critical period."),div(class="ov-roadmap-mini","Baseline network  vs  Critical network"),div(class="ov-roadmap-link","Explore Behaviour Change ",icon("arrow-right")))),
             column(3,div(class="ov-roadmap-card",div(class="ov-roadmap-number purple-dot","3"),div(class="ov-roadmap-title","Leading Indicators"),div(class="ov-roadmap-text","Identify early warning signals, repeated patterns, and possible oversight gaps before the breach."),div(class="ov-roadmap-mini","Risk signals  \u2022  Similar episodes  \u2022  Gaps"),div(class="ov-roadmap-link","Explore Leading Indicators ",icon("arrow-right")))),
             column(3,div(class="ov-roadmap-card",div(class="ov-roadmap-number orange-dot","4"),div(class="ov-roadmap-title","Evidence Explorer"),div(class="ov-roadmap-text","Search and inspect message-level evidence, risk signals, agents, and communication channels."),div(class="ov-roadmap-mini","Search  \u2022  Filter  \u2022  Validate"),div(class="ov-roadmap-link","Explore Evidence Explorer ",icon("arrow-right"))))
           ),
           br(),
           fluidRow(
             column(7,
                    div(class="ov-section-title",icon("star")," Key Investigation Highlights"),br(),
                    fluidRow(
                      column(4,div(class="ov-highlight-card highlight-blue",div(class="ov-highlight-icon",icon("users")),div(class="ov-highlight-title","Concentration of Communication"),div(class="ov-highlight-text","Identify agents and channels with elevated communication activity during the critical period."),div(class="ov-highlight-link","Review agents ",icon("arrow-right")))),
                      column(4,div(class="ov-highlight-card highlight-red",div(class="ov-highlight-icon",icon("exclamation-circle")),div(class="ov-highlight-title","Elevated Risk Activity"),div(class="ov-highlight-text","Compare high-risk messages and public or anonymous actions across the investigation windows."),div(class="ov-highlight-link","Review high-risk messages ",icon("arrow-right")))),
                      column(4,div(class="ov-highlight-card highlight-green",div(class="ov-highlight-icon",icon("chart-line")),div(class="ov-highlight-title","Early Warning Detected"),div(class="ov-highlight-text","Examine prior episodes, unusual behaviour, and potential oversight gaps before the breach."),div(class="ov-highlight-link","Review leading indicators ",icon("arrow-right"))))
                    )
             ),
             column(5,
                    div(class="ov-section-title",icon("info-circle")," Investigation Guide"),br(),
                    div(class="ov-guide-card",
                        div(class="ov-guide-title","Suggested investigation flow"),
                        tags$ol(class="ov-guide-list",
                                tags$li(tags$b("Start with Q1:")," reconstruct the breach timeline."),
                                tags$li(tags$b("Move to Q2:")," compare baseline and critical-period behaviour."),
                                tags$li(tags$b("Use Q3:")," identify earlier warning signals and similar occasions."),
                                tags$li(tags$b("Confirm in Q4:")," inspect message-level evidence.")
                        ),
                        tags$hr(),
                        div(class="ov-guide-note",tags$b("Interpretation note: "),"Visual patterns identify evidence for investigation. They should be considered together with message content, communication timing, channels, and agent roles.")
                    )
             )
           ),
           br(),
           div(class="ov-footer-note",icon("info-circle")," All dashboard findings should be interpreted as supporting evidence, not proof of individual responsibility."),
           br()
  ),
  
  # ── Q1 ───────────────────────────────────────────────────────────────────────
  tabPanel(tagList(icon("road"),"1. Path to Breach"),br(),
           card(card_header("Question 1: What events and relationships led to the inappropriate release?"),
                p("This view reconstructs the breach day by comparing morning and afternoon interaction structures, ",
                  "identifying emerging discussion themes, and surfacing the narrow evidence window where communication changed most sharply.")),
           br(),
           sidebarLayout(
             sidebarPanel(width=3,
                          div(class="filter-panel",
                              h3("Investigation Filters"),
                              dateInput("q1_focus_date","Breach-day focus",value=q1_default_date,min=min_date,max=max_date),
                              selectizeInput("q1_agents","Agent / role",c("All",all_agents,all_roles),selected="All",multiple=TRUE),
                              selectizeInput("q1_channels","Channel",all_channels,selected=all_channels,multiple=TRUE),
                              checkboxGroupInput("q1_risks","Risk level",all_risks,selected=all_risks),
                              tags$hr(),
                              h4("Network Detail"),
                              radioButtons("q1_target_view","Relationship view",choices=q1_target_choices,selected="Channel"),
                              sliderInput("q1_network_size","Maximum agents shown",min=3,max=max(3,length(all_agents)),value=min(7,max(3,length(all_agents)))),
                              tags$hr(),
                              tags$p(class="small-note",tags$b("Emerging phrases chart: "),"Fixed to Alvin's original June 5, top-15 calculation so it can be compared directly with his result.")
                          )
             ),
             mainPanel(width=9,
                       div(class="guide-box",tags$b("How to read this: "),"Squares are agents and circles are channels or recipients. ","Larger nodes have higher subgraph centrality, meaning they sit within more tightly connected interaction structures. ","Compare the morning and afternoon networks to identify the changing coordination pathway."),
                       tabsetPanel(
                         tabPanel("1. Interaction Shift",br(),
                                  fluidRow(
                                    column(6,card(card_header("Morning Interaction Structure"),div(class="small-note","Before 12:00 PM. Nodes are sized by subgraph centrality."),visNetworkOutput("q1_morning_network",height="570px"))),
                                    column(6,card(card_header("Afternoon Interaction Structure"),div(class="small-note","From 12:00 PM onward. Compare node size and relationship concentration."),visNetworkOutput("q1_afternoon_network",height="570px")))
                                  ),
                                  br(),
                                  fluidRow(
                                    column(6,card(card_header("Morning: Most Central Agents"),tableOutput("q1_morning_agents"))),
                                    column(6,card(card_header("Afternoon: Most Central Agents"),tableOutput("q1_afternoon_agents")))
                                  )
                         ),
                         tabPanel("2. Emerging Phrases",br(),
                                  card(card_header("War-Room Analysis: Emerging Phrases"),
                                       div(class="small-note","Exact replication of Alvin's fixed June 5 calculation using the untouched communications table inside mc1_prepared_data.rds. The top 15 phrases are not changed by the sidebar filters."),
                                       plotOutput("q1_phrase_plot",height="480px")),
                                  br(),
                                  card(card_header("Emerging Phrase Detail"),DTOutput("q1_phrase_table"))
                         ),
                         tabPanel("3. Forensic Pivot and Evidence",br(),
                                  div(class="guide-box",tags$b("Forensic logic: "),"A low Jaccard similarity marks a sharp vocabulary shift between consecutive 30-minute windows. ","The Adamic-Adar-style score identifies when the two most central afternoon agents shared the strongest interaction context."),
                                  fluidRow(
                                    column(6,card(card_header("Vocabulary Continuity"),div(class="small-note","Lower values indicate a sharper change in discussion vocabulary."),plotlyOutput("q1_similarity_plot",height="310px"))),
                                    column(6,card(card_header("Interaction Cohesion"),div(class="small-note","Higher values indicate a stronger shared connection pattern between the two most central afternoon agents."),plotlyOutput("q1_adamic_plot",height="310px")))
                                  ),
                                  br(),
                                  card(card_header("Detected Pivot Window"),uiOutput("q1_pivot_note")),
                                  br(),
                                  card(card_header("Focused Message Evidence"),div(class="small-note","Messages within \u00b130 minutes of the strongest detected pivot, limited to the two most central afternoon agents."),DTOutput("q1_evidence_table"))
                         )
                       )
             )
           )
  ),
  
  # ── Q2 ───────────────────────────────────────────────────────────────────────
  tabPanel(tagList(icon("exchange-alt"),"2. Behaviour Change"),br(),
           card(card_header("Question 2: How did communication behaviour during the breach differ from prior behaviour?"),
                p("Compare the distinctive terms used by agents before the breach with those used during the critical period. ","Terms that are new or more prominent in the critical period are candidate indicators of behavioural change.")),
           br(),
           sidebarLayout(
             sidebarPanel(width=3,
                          div(class="filter-panel",
                              h3("Investigation Filters"),
                              dateRangeInput("baseline_dates","Baseline period",start=baseline_start_default,end=baseline_end_default,min=min_date,max=max_date),
                              dateRangeInput("critical_dates","Critical period",start=critical_start_default,end=critical_end_default,min=min_date,max=max_date),
                              selectizeInput("q2_agents","Agent / role",c("All",all_agents,all_roles),selected="All",multiple=TRUE),
                              selectizeInput("q2_channels","Channel",all_channels,selected=all_channels,multiple=TRUE),
                              checkboxGroupInput("q2_risks","Risk level",all_risks,selected=all_risks),
                              sliderInput("keywords_per_agent","Keywords per agent",min=3,max=15,value=10)
                          )
             ),
             mainPanel(width=9,
                       div(class="guide-box",tags$b("How to read this: "),"Each large coloured node is an agent and each light-blue node is a distinctive term. ","Compare the two networks to identify terms that are absent, new, or more prominent during the critical period. ","This identifies patterns for investigation; it does not establish intent."),
                       div(class="primary-tag","Primary evidence: agent-keyword behaviour comparison"),
                       fluidRow(
                         column(6,card(card_header("Baseline Network Structure"),div(class="small-note","Distinctive agent vocabulary before 4 June 2046."),visNetworkOutput("baseline_network",height="560px"))),
                         column(6,card(card_header("Critical-Period Network Structure"),div(class="small-note","Distinctive agent vocabulary during 4\u20135 June 2046."),visNetworkOutput("critical_network",height="560px")))
                       ),
                       br(),
                       card(card_header("Behaviour Comparison Summary"),div(class="small-note","A direct comparison of communication volume, active agents, channels, and high-risk messages across the two selected periods."),tableOutput("q2_summary"))
             )
           )
  ),
  
  # ── Q3: Leading Indicators ── UPDATED ────────────────────────────────────────
  tabPanel(tagList(icon("warning"),"3. Leading Indicators"),br(),
           sidebarLayout(
             sidebarPanel(width=3,
                          div(class="filter-panel",
                              h3("Investigation Filters"),
                              dateRangeInput("q3_dates","Prior period",min_date,baseline_end_default,min=min_date,max=max_date),
                              selectizeInput("q3_agents","Agent / role",c("All",all_agents,all_roles),selected="All",multiple=TRUE),
                              selectizeInput("q3_channels","Channel",all_channels,selected=all_channels,multiple=TRUE),
                              checkboxGroupInput("q3_risks","Risk level",all_risks,selected=all_risks)
                          )
             ),
             mainPanel(width=9,
                       div(class="guide-box",
                           tags$b("Purpose: "),
                           "Review earlier signals and communications that may have indicated the release was possible. ",
                           "Use the two tabs below to compare the pre-breach period (up to 16:00, 5 Jun) with the breach window (from 17:00, 5 Jun). ",
                           "The agent chart beneath shows who was responsible for High Signal messages in each period."),
                       
                       # ── Tab set: pre-breach vs breach ─────────────────────────────────────
                       tabsetPanel(
                         tabPanel(
                           "Up to 16:00, 5 Jun \u2014 Prior Period",
                           br(),
                           card(
                             card_header("Prior-Period Activity by Risk Level"),
                             plotlyOutput("q3_plot_pre", height="340px")
                           )
                         ),
                         tabPanel(
                           "From 17:00, 5 Jun \u2014 Breach Window",
                           br(),
                           card(
                             card_header("Breach Window Activity by Risk Level"),
                             plotlyOutput("q3_plot_breach", height="340px")
                           )
                         )
                       ),
                       
                       br(),
                       
                       # ── Agent High Signal chart ────────────────────────────────────────────
                       card(
                         card_header("High Signal Messages by Agent"),
                         div(class="small-note",
                             "Compares which agents sent High Signal messages before the breach (dark bars) versus during the breach window from 17:00 on 5 Jun (red bars)."),
                         plotlyOutput("q3_agent_plot", height="300px")
                       ),
                       
                       br(),
                       
                       # ── Evidence table ─────────────────────────────────────────────────────
                       card(
                         card_header("Prior-Period Evidence"),
                         DTOutput("q3_table")
                       )
             )
           )
  ),
  
  # ── Q4 ───────────────────────────────────────────────────────────────────────
  tabPanel(tagList(icon("search"),"4. Evidence Explorer"),br(),
           sidebarLayout(
             sidebarPanel(width=3,
                          div(class="filter-panel",
                              h3("Investigation Filters"),
                              dateRangeInput("q4_dates","Date range",min_date,max_date,min=min_date,max=max_date),
                              selectizeInput("q4_agents","Agent / role",c("All",all_agents,all_roles),selected="All",multiple=TRUE),
                              selectizeInput("q4_channels","Channel",all_channels,selected=all_channels,multiple=TRUE),
                              checkboxGroupInput("q4_risks","Risk level",all_risks,selected=all_risks),
                              textInput("q4_search","Search text",placeholder="e.g., embargo, post, release")
                          )
             ),
             mainPanel(width=9,
                       div(class="guide-box",tags$b("Purpose: "),"Search and inspect the underlying communications in context."),
                       card(card_header("Searchable Message Evidence"),DTOutput("q4_table"))
             )
           )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Overview
  output$total_messages     <- renderText(scales::comma(nrow(communications)))
  output$total_agents       <- renderText(scales::comma(n_distinct(communications$agent_id)))
  output$total_channels     <- renderText(scales::comma(n_distinct(communications$channel)))
  output$high_risk_messages <- renderText(
    scales::comma(sum(communications$risk_signal_group=="High Signal",na.rm=TRUE)))
  
  # ── Q1 ────────────────────────────────────────────────────────────────────────
  q1_day_data <- reactive({
    q1_prepare_day_data(communications,input$q1_focus_date,input$q1_agents,
                        input$q1_channels,input$q1_risks,input$q1_target_view)
  })
  q1_morning_data   <- reactive({ q1_day_data()|>filter(q1_period=="Morning")   })
  q1_afternoon_data <- reactive({ q1_day_data()|>filter(q1_period=="Afternoon") })
  q1_morning_structure   <- reactive({ q1_build_centrality_network(q1_morning_data(),  input$q1_network_size) })
  q1_afternoon_structure <- reactive({ q1_build_centrality_network(q1_afternoon_data(),input$q1_network_size) })
  output$q1_morning_network   <- renderVisNetwork({ q1_draw_centrality_network(q1_morning_structure())   })
  output$q1_afternoon_network <- renderVisNetwork({ q1_draw_centrality_network(q1_afternoon_structure()) })
  output$q1_morning_agents  <- renderTable({ q1_morning_structure()$agent_ranking  |>slice_head(n=5) },striped=TRUE,bordered=TRUE,hover=TRUE,spacing="s",rownames=FALSE)
  output$q1_afternoon_agents <- renderTable({ q1_afternoon_structure()$agent_ranking|>slice_head(n=5) },striped=TRUE,bordered=TRUE,hover=TRUE,spacing="s",rownames=FALSE)
  q1_phrases <- reactive({ q1_alvin_emerging_phrases(q1_alvin_raw_communications) })
  output$q1_phrase_plot <- renderPlot({
    phrase_data <- q1_phrases()
    validate(need(nrow(phrase_data)>0,"No emerging phrases could be calculated from the June 5 communications."))
    ggplot(phrase_data,aes(x=reorder(bigram,shift_score),y=shift_score))+
      geom_col(fill="darkred",alpha=0.8)+coord_flip()+theme_minimal()+
      labs(title="War Room Analysis: Emerging Phrases (Afternoon Vs Morning)",
           subtitle="What Legal, Social, and Platform Trust started discussing post-12:00 PM",
           x="Emerging Phrase",y="Frequency Increase (Afternoon vs. Morning)")
  })
  output$q1_phrase_table <- renderDT({
    phrase_data <- q1_phrases()
    if (!nrow(phrase_data))
      return(datatable(data.frame(Message="No afternoon-emerging phrases match the current filters."),rownames=FALSE,options=list(dom="t")))
    datatable(phrase_data,rownames=FALSE,options=list(pageLength=10,lengthChange=FALSE,scrollX=TRUE,order=list(list(3,"desc"))))
  })
  q1_vocabulary_data      <- reactive({ q1_vocabulary_similarity(q1_day_data()) })
  q1_top_afternoon_agents <- reactive({ q1_afternoon_structure()$agent_ranking|>slice_head(n=2)|>pull(Agent) })
  q1_adamic_data          <- reactive({ q1_adamic_adar_timeline(q1_day_data(),q1_top_afternoon_agents()) })
  output$q1_similarity_plot <- renderPlotly({
    similarity_data <- q1_vocabulary_data()
    if (!nrow(similarity_data)) return(q1_empty_plot("At least two 30-minute communication windows are required."))
    plot_ly(similarity_data,x=~time_bin,y=~jaccard_similarity,type="scatter",mode="lines+markers",
            line=list(color="#2563EB",width=3),marker=list(color="#2563EB",size=7),
            text=~paste0("Time: ",format(time_bin,"%H:%M"),"<br>Jaccard similarity: ",round(jaccard_similarity,3)),hoverinfo="text")|>
      layout(xaxis=list(title="Time"),yaxis=list(title="Vocabulary similarity",range=c(0,1)),paper_bgcolor="#FFFFFF",plot_bgcolor="#FFFFFF")
  })
  output$q1_adamic_plot <- renderPlotly({
    adamic_data <- q1_adamic_data()
    if (!nrow(adamic_data)) return(q1_empty_plot("Two central afternoon agents are required to calculate this score."))
    plot_ly(adamic_data,x=~time_bin,y=~adamic_adar,type="scatter",mode="lines+markers",
            line=list(color="#DC2626",width=3),marker=list(color="#DC2626",size=7),
            text=~paste0("Time: ",format(time_bin,"%H:%M"),"<br>Shared-connection score: ",round(adamic_adar,3)),hoverinfo="text")|>
      layout(xaxis=list(title="Time"),yaxis=list(title="Adamic-Adar-style score"),paper_bgcolor="#FFFFFF",plot_bgcolor="#FFFFFF")
  })
  q1_pivot_time <- reactive({
    vocabulary_data <- q1_vocabulary_data(); adamic_data <- q1_adamic_data()
    text_pivot <- if (nrow(vocabulary_data)>0) vocabulary_data|>slice_min(jaccard_similarity,n=1,with_ties=FALSE)|>pull(time_bin) else as.POSIXct(NA)
    connection_peak <- if (nrow(adamic_data)>0&&any(adamic_data$adamic_adar>0)) adamic_data|>slice_max(adamic_adar,n=1,with_ties=FALSE)|>pull(time_bin) else as.POSIXct(NA)
    list(text_pivot=text_pivot,connection_peak=connection_peak,anchor=if(!is.na(connection_peak)) connection_peak else text_pivot)
  })
  output$q1_pivot_note <- renderUI({
    pivot <- q1_pivot_time(); actors <- q1_top_afternoon_agents()
    if (is.na(pivot$anchor)) return(div(class="small-note","No pivot window can be identified with the current filters."))
    tagList(tags$p(tags$b("Primary evidence window: "),format(pivot$anchor,"%d %B %Y, %H:%M")),
            tags$p(tags$b("Most central afternoon agents: "),paste(actors,collapse=" and ")),
            tags$p("The message table below shows the surrounding 60-minute window for targeted review."))
  })
  q1_focused_evidence <- reactive({
    pivot <- q1_pivot_time(); actors <- q1_top_afternoon_agents(); data <- q1_day_data()
    if (is.na(pivot$anchor)||!length(actors)) return(data[0,])
    data|>filter(q1_event_time>=pivot$anchor-lubridate::minutes(30),q1_event_time<=pivot$anchor+lubridate::minutes(30),agent_label %in% actors)|>arrange(q1_event_time)
  })
  output$q1_evidence_table <- renderDT({
    evidence <- q1_focused_evidence()
    if (!nrow(evidence)) return(datatable(data.frame(Message="No messages were found in the detected evidence window."),rownames=FALSE,options=list(dom="t")))
    datatable(evidence|>transmute(Time=format(q1_event_time,"%Y-%m-%d %H:%M"),Agent=agent_label,Role=agent_role,Channel=channel,`Risk signal`=risk_signal_group,`Internal state`=internal_state_deliberating,`Message evidence`=if_else(nchar(content)>300,paste0(str_sub(content,1,300),"\u2026"),content)),
              rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE))
  })
  
  # ── Q2 ────────────────────────────────────────────────────────────────────────
  baseline_data <- reactive({ filter_messages(communications,input$baseline_dates,input$q2_agents,input$q2_channels,input$q2_risks) })
  critical_data <- reactive({ filter_messages(communications,input$critical_dates,input$q2_agents,input$q2_channels,input$q2_risks) })
  output$baseline_network <- renderVisNetwork({ draw_alvin_keyword_network(build_alvin_keyword_network(baseline_data(),input$keywords_per_agent)) })
  output$critical_network <- renderVisNetwork({ draw_alvin_keyword_network(build_alvin_keyword_network(critical_data(),input$keywords_per_agent)) })
  output$q2_summary <- renderTable({
    b  <- baseline_data()|>summarise(Messages=n(),`Active agents`=n_distinct(agent_id),`Active channels`=n_distinct(channel),`High-risk messages`=sum(risk_signal_group=="High Signal",na.rm=TRUE))
    cr <- critical_data()|>summarise(Messages=n(),`Active agents`=n_distinct(agent_id),`Active channels`=n_distinct(channel),`High-risk messages`=sum(risk_signal_group=="High Signal",na.rm=TRUE))
    mn <- names(b); bv <- as.numeric(b[1,]); cv <- as.numeric(cr[1,]); dv <- cv-bv
    change_labels <- ifelse(bv==0,ifelse(cv==0,"No change","New in critical period"),
                            paste0(ifelse(dv>0,"+",""),dv," (",ifelse(round(dv/bv*100,1)>0,"+",""),round(dv/bv*100,1),"%)"))
    tibble(Metric=mn,Baseline=bv,`Critical period`=cv,Change=change_labels)
  },striped=TRUE,bordered=TRUE,hover=TRUE,spacing="s",rownames=FALSE)
  
  # ── Q3: Leading Indicators ── UPDATED ─────────────────────────────────────────
  
  # Base filtered data from sidebar
  q3_data <- reactive({
    filter_messages(communications, input$q3_dates, input$q3_agents,
                    input$q3_channels, input$q3_risks)
  })
  
  # Helper: build stacked bar plotly
  # Reversed factor levels so Red (High Signal) renders on top of the stack
  q3_make_bar <- function(df, title_label) {
    validate(need(nrow(df) > 0, "No messages match the selected filters."))
    stacking_levels <- rev(Q3_RISK_LEVELS)
    # Build ordered character labels so plotly treats x as discrete
    # categories and every date gets its own bar with no gaps
    all_dates   <- sort(unique(df$event_date))
    date_labels <- format(all_dates, "%d %b")
    plot_data <- df |>
      count(event_date, risk_signal_group) |>
      mutate(
        risk_signal_group = factor(risk_signal_group, levels = stacking_levels),
        date_label        = factor(format(event_date, "%d %b"), levels = date_labels)
      )
    plot_ly(plot_data,
            x            = ~as.character(date_label),
            y            = ~n,
            type         = "bar",
            color        = ~risk_signal_group,
            colors       = Q3_RISK_COLOURS,
            text         = ~paste0(format(event_date, "%Y-%m-%d"), "<br>",
                                   risk_signal_group, "<br>Messages: ", n),
            hoverinfo    = "text",
            textposition = "none") |>
      layout(barmode = "stack",
             xaxis   = list(
               title         = "Date",
               type          = "category",
               categoryorder = "array",
               categoryarray = date_labels,
               tickangle     = -45
             ),
             yaxis  = list(title = "Number of messages"),
             legend = list(title = list(text = "Risk level"), traceorder = "reversed"))
  }
  
  # Tab 1: pre-breach = filtered data PLUS Jun 5 up to 16:00
  output$q3_plot_pre <- renderPlotly({
    d <- q3_data()
    # Include rows from the selected date range, and also Jun 5 rows before 17:00
    d_pre <- bind_rows(
      d |> filter(event_date < as.Date("2046-06-05")),
      communications |>
        filter(event_date == as.Date("2046-06-05"), event_hour < 17) |>
        # apply same agent/channel/risk filters
        filter(if ("All" %in% input$q3_agents || is.null(input$q3_agents)) TRUE
               else agent_label %in% input$q3_agents | agent_role %in% input$q3_agents) |>
        filter(channel %in% input$q3_channels) |>
        filter(risk_signal_group %in% input$q3_risks)
    ) |> distinct()
    q3_make_bar(d_pre, "Prior Period")
  })
  
  # Tab 2: breach window = Jun 5 from 17:00 onward
  output$q3_plot_breach <- renderPlotly({
    d_breach <- communications |>
      filter(event_date == as.Date("2046-06-05"), event_hour >= 17) |>
      filter(if ("All" %in% input$q3_agents || is.null(input$q3_agents)) TRUE
             else agent_label %in% input$q3_agents | agent_role %in% input$q3_agents) |>
      filter(channel %in% input$q3_channels) |>
      filter(risk_signal_group %in% input$q3_risks)
    validate(need(nrow(d_breach) > 0, "No messages found from 17:00 on 5 June 2046."))
    plot_data <- d_breach |>
      mutate(hour_label = paste0(formatC(event_hour, width=2, flag="0"), ":00")) |>
      count(hour_label, risk_signal_group) |>
      mutate(risk_signal_group = factor(risk_signal_group, levels = Q3_RISK_LEVELS))
    plot_ly(plot_data,
            x      = ~hour_label,
            y      = ~n,
            type   = "bar",
            color  = ~risk_signal_group,
            colors = Q3_RISK_COLOURS,
            text   = ~paste0(hour_label, "<br>", risk_signal_group, "<br>Messages: ", n),
            hoverinfo = "text") |>
      layout(barmode = "stack",
             xaxis  = list(title = "Hour (5 June 2046)", categoryorder = "array",
                           categoryarray = c("17:00","18:00")),
             yaxis  = list(title = "Number of messages"),
             legend = list(title = list(text = "Risk level"), traceorder = "normal"))
  })
  
  # Agent chart: side-by-side bars comparing pre-breach vs breach High Signal counts
  output$q3_agent_plot <- renderPlotly({
    # Pre-breach High Signal
    pre_agents <- bind_rows(
      q3_data() |> filter(event_date < as.Date("2046-06-05"), risk_signal_group == "High Signal"),
      communications |> filter(event_date == as.Date("2046-06-05"), event_hour < 17,
                               risk_signal_group == "High Signal") |>
        filter(if ("All" %in% input$q3_agents || is.null(input$q3_agents)) TRUE
               else agent_label %in% input$q3_agents | agent_role %in% input$q3_agents) |>
        filter(channel %in% input$q3_channels)
    ) |> distinct() |>
      count(agent_label) |>
      mutate(Period = "Prior period (up to 16:00, 5 Jun)")
    
    # Breach window High Signal
    breach_agents <- communications |>
      filter(event_date == as.Date("2046-06-05"), event_hour >= 17,
             risk_signal_group == "High Signal") |>
      filter(if ("All" %in% input$q3_agents || is.null(input$q3_agents)) TRUE
             else agent_label %in% input$q3_agents | agent_role %in% input$q3_agents) |>
      filter(channel %in% input$q3_channels) |>
      count(agent_label) |>
      mutate(Period = "Breach window (from 17:00, 5 Jun)")
    
    combined <- bind_rows(pre_agents, breach_agents)
    validate(need(nrow(combined) > 0, "No High Signal messages found for the current filters."))
    
    period_cols <- c(
      "Prior period (up to 16:00, 5 Jun)" = "#2E75B6",
      "Breach window (from 17:00, 5 Jun)" = "#C00000"
    )
    
    plot_ly(combined,
            x      = ~agent_label,
            y      = ~n,
            color  = ~Period,
            colors = period_cols,
            type   = "bar",
            text   = ~paste0(agent_label, "<br>", Period, "<br>High Signal msgs: ", n),
            hoverinfo = "text") |>
      layout(barmode = "group",
             xaxis  = list(title = "Agent"),
             yaxis  = list(title = "High Signal messages"),
             legend = list(title = list(text = "Period")))
  })
  
  # Evidence table
  output$q3_table <- renderDT({
    datatable(message_table(q3_data()), rownames=FALSE, filter="top",
              options=list(pageLength=10, scrollX=TRUE))
  })
  
  # ── Q4 ────────────────────────────────────────────────────────────────────────
  q4_data <- reactive({
    data <- filter_messages(communications,input$q4_dates,input$q4_agents,input$q4_channels,input$q4_risks)
    search_value <- str_trim(input$q4_search)
    if (nchar(search_value)>0)
      data <- data|>filter(str_detect(str_to_lower(full_text),fixed(str_to_lower(search_value))))
    data
  })
  output$q4_table <- renderDT({
    datatable(message_table(q4_data()),rownames=FALSE,filter="top",options=list(pageLength=15,scrollX=TRUE))
  })
}

shinyApp(ui=ui, server=server)
