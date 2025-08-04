library(shiny)
library(dplyr)
library(readr)
source("helpers.R") 
source("hex_chart.R")

shinyServer(function(input, output, session) {
  
  # Load correct players.csv and extract available seasons
  players_df <- reactive({
    file <- if (input$competition == "EuroLeague") {
      "data/players.csv"
    } else {
      "data/eurocup_players.csv"
    }
    read_csv(file)
  })
  
  # Generate season choices dynamically
  output$season_ui <- renderUI({
    df <- players_df()
    season_codes <- sort(unique(df$season_code))
    
    # Dynamically convert E2023 or U2024 → 2023–2024
    season_labels <- gsub("^[EU]", "", season_codes) |> 
      as.numeric() |> 
      (\(x) paste0(x, "–", x + 1))()
    
    names(season_codes) <- season_labels
    
    selectInput("season", "Season", choices = season_codes)
  })
  
  
  # Generate player list based on selected league and season
  output$player_ui <- renderUI({
    req(input$season)  # wait for season input
    df <- players_df()
    filtered <- df %>%
      filter(season_code == input$season) %>%
      arrange(Player)
    
    if (nrow(filtered) == 0) {
      return(tags$p("⚠️ No players available for this season."))
    }
    
    selectInput("player", "Player", choices = filtered$Player)
  })
  
  # Load correct RDS based on league
  shot_data <- reactive({
    file <- if (input$competition == "EuroLeague") {
      "data/shot_data.rds"
    } else {
      "data/eurocup_shot_data.rds"
    }
    readRDS(file)
  })
  
  output$league_logo <- renderUI({
    req(input$competition)
    
    logo_file <- if (input$competition == "EuroLeague") {
      "EL_Logo-Horiz-CMYK-Positive.png"
    } else {
      "EC_BKT_Logo_23_24_Positive-Landscape.png"
    }
    
    tags$img(src = logo_file, height = "80px", style = "display: block; margin-top: 20px; margin-left: auto; margin-right: auto;")
  })
  
  # Render court chart
  observeEvent(input$update, {
    output$court_plot <- renderPlot({
      req(input$player, input$season, input$chart_type)
      data <- shot_data() %>%
        filter(Player == input$player, season_code == input$season)
      
      base_court <- generate_base_court()
      
      if (input$chart_type == "Scatter") {
        data$isShotMade <- as.logical(data$isShotMade)
        base_court +
          geom_point(
            data = data,
            aes(x = loc_x, y = loc_y, color = factor(isShotMade, levels = c("TRUE", "FALSE"))),
            alpha = 0.8, size = 2
          ) +
          scale_color_manual(
            values = c("TRUE" = court_themes$dark$made, "FALSE" = court_themes$dark$missed),
            labels = c("Made", "Missed"),
            name = NULL
          )
      } else if (input$chart_type == "Hex") {
        league_averages <- shot_data() %>%
          group_by(shot_zone_range, shot_zone_area) %>%
          summarize(fgm = sum(shot_made_numeric), fga = n(), .groups = "drop")
        
        hex_data <- calculate_hexbins_from_shots(data, league_averages)
        generate_hex_chart(hex_data, base_court, metric = "bounded_fg_diff")
      } else {
        base_court
      }
    })
  })
  
})
