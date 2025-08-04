library(shiny)

fluidPage(
  titlePanel("Shot Chart App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("competition", "Select League", choices = c("EuroLeague", "EuroCup")),
      uiOutput("season_ui"),
      uiOutput("player_ui"),
      selectInput("chart_type", "Chart Style", choices = c("Scatter", "Hex")),
      actionButton("update", "Update Chart"),
      
      # Logo UI
      uiOutput("league_logo")
    ),
    
    mainPanel(
      plotOutput("court_plot", height = "700px")
    )
  ),
  
  tags$div(
    style = "text-align:center; margin-top: 30px; font-size: 13px; color: #666;",
    HTML("
      Data: euroleaguer<br>
      Built by Can Sahin
    ")
  )
)
