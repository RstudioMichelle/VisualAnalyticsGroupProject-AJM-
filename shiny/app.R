library(shiny)

ui <- navbarPage(
  "TenantThread Dashboard",
  
  tabPanel("Overview",
           h2("VAST MC1 Interactive Dashboard"),
           p("Explore timeline, communication network, and sensitive topic patterns.")
  ),
  
  tabPanel("Timeline",
           h2("Timeline Analysis"),
           plotOutput("timeline_plot")
  ),
  
  tabPanel("Network",
           h2("Communication Network"),
           p("Network visualisation will go here.")
  ),
  
  tabPanel("Keywords",
           h2("Sensitive Topics"),
           p("Keyword heatmap will go here.")
  )
)

server <- function(input, output) {
  output$timeline_plot <- renderPlot({
    plot(1:10, main = "Sample Timeline Plot")
  })
}

shinyApp(ui, server)