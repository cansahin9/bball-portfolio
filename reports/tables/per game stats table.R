setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/reports/tables")

gt_theme_f5 <- function(gt_object, ...) {
  gt_object %>%
    opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()
      ),
      weight = 400
    ) %>%
    tab_style(
      locations = cells_title("title"),
      style = cell_text(
        font = google_font("Roboto"),
        weight = 700
      )
    ) %>%
    tab_style(
      locations = cells_title("subtitle"),
      style = cell_text(
        font = google_font("Roboto"),
        color = "gray35",
        weight = 400
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(
          font = google_font("Roboto"),
          v_align = "bottom",
          size = px(14),
          weight = 'bold',
          color = "white"  # white text for contrast
        ),
        cell_fill(color = "black"),  # red background
        cell_borders(sides = c("top", "bottom"), color = "gray35", weight = px(1))
      ),
      locations = gt::cells_column_labels()
    ) %>%
    tab_options(
      column_labels.background.color = "black",  # red background
      heading.border.bottom.style = "none",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      column_labels.font.weight = "bold", 
      table.font.size = 14,
      heading.align = "left",
      table.background.color = "white",
      table_body.hlines.color = 'gray85',
      data_row.padding = px(6),
      ...
    )
}


create_eurocup_table <- function(first_name, last_name, 
                                 save_path = getwd(),
                                 verbose = TRUE) {
  require(dplyr)
  require(gt)
  require(purrr)
  require(stringr)
  require(euroleaguer)
  require(scales)
  library(gtExtras)
  library(gtUtils)
  
  player_name_pattern <- paste0("(?i)", first_name, ".*", last_name, "|", last_name, ".*", first_name)
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  all_seasons <- paste0("U", 2010:current_year)
  
  if (verbose) message("🔍 Searching for ", str_to_title(first_name), " ", str_to_title(last_name))
  
  # Get per-game data
  player_data <- map_df(all_seasons, function(season) {
    if (verbose) message("📅 Checking ", season)
    tryCatch({
      getPlayerStats(season_code = season, statistic_mode = "perGame") %>%
        filter(str_detect(PlayerName, player_name_pattern)) %>%
        mutate(SeasonCode = season)
    }, error = function(e) tibble())
  })
  
  # Manual patch for 2021
  if (!"U2021" %in% player_data$SeasonCode) {
    vnc_2021 <- tryCatch({
      getTeamStats("U2021", "VNC")$PlayerAveragePerGame %>%
        filter(str_detect(tolower(PlayerName), tolower(first_name))) %>%
        mutate(
          `2P%` = as.character(`2P%`),
          `3P%` = as.character(`3P%`),
          `FT%` = as.character(`FT%`),
          SeasonCode = "U2021"
        )
    }, error = function(e) tibble())
    
    if (nrow(vnc_2021) > 0) player_data <- bind_rows(player_data, vnc_2021)
  }
  
  if (nrow(player_data) == 0) stop("❌ No data found for this player.")
  
  # Clean and format per-season data
  cleaned <- player_data %>%
    mutate(across(c(`2P%`, `3P%`, `FT%`), ~as.numeric(str_replace(.x, "%", "")) / 100)) %>%
    transmute(
      Season = paste0(as.integer(str_replace(SeasonCode, "^U", "")), "-", 
                      substr(as.integer(str_replace(SeasonCode, "^U", "")) + 1, 3, 4)),
      G = floor(GP),
      MP = round(coalesce(MIN, TimePlayed / 60), 1),
      PTS = round(PTS, 1),
      FG = round(`2PM` + `3PM`, 1),
      FGA = round(`2PA` + `3PA`, 1),
      `FG%` = (`2PM` + `3PM`) / (`2PA` + `3PA`),
      `2P` = round(`2PM`, 1),
      `2PA` = round(`2PA`, 1),
      `2P%` = `2P%`,
      `3P` = round(`3PM`, 1),
      `3PA` = round(`3PA`, 1),
      `3P%` = `3P%`,
      FT = round(FTM, 1),
      FTA = round(FTA, 1),
      `FT%` = `FT%`,
      AST = round(AST, 1),
      TOV = round(TO, 1),
      ORB = round(OREB, 1),
      DRB = round(DREB, 1),
      STL = round(STL, 1),
      BLK = round(BLK, 1)
    ) %>%
    arrange(Season)
  
  # Get accumulated stats for accurate totals
  accumulated_data <- map_df(all_seasons, function(season) {
    tryCatch({
      getPlayerStats(season_code = season, statistic_mode = "accumulated") %>%
        filter(str_detect(PlayerName, player_name_pattern))
    }, error = function(e) tibble())
  })
  
  # Patch 2021 if needed
  if (!"U2021" %in% accumulated_data$SeasonCode) {
    vnc_patch <- tryCatch({
      getTeamStats("U2021", "VNC")$PlayerAveragePerGame %>%
        filter(str_detect(tolower(PlayerName), tolower(first_name))) %>%
        mutate(
          SeasonCode = "U2021",
          GP = as.numeric(GP),
          MIN = as.numeric(TimePlayed),
          PTS = PTS * GP,
          `2PM` = `2PM` * GP,
          `2PA` = `2PA` * GP,
          `3PM` = `3PM` * GP,
          `3PA` = `3PA` * GP,
          FTM = FTM * GP,
          FTA = FTA * GP,
          AST = AST * GP,
          TO = TO * GP,
          OREB = OREB * GP,
          DREB = DREB * GP,
          STL = STL * GP,
          BLK = BLK * GP
        )
    }, error = function(e) tibble())
    accumulated_data <- bind_rows(accumulated_data, vnc_patch)
  }
  
  # Compute totals separately
  totals <- accumulated_data %>%
    summarise(
      GP   = sum(GP, na.rm = TRUE),
      MIN  = sum(MIN, na.rm = TRUE),
      PTS  = sum(PTS, na.rm = TRUE),
      `2PM` = sum(`2PM`, na.rm = TRUE),
      `2PA` = sum(`2PA`, na.rm = TRUE),
      `3PM` = sum(`3PM`, na.rm = TRUE),
      `3PA` = sum(`3PA`, na.rm = TRUE),
      FTM   = sum(FTM, na.rm = TRUE),
      FTA   = sum(FTA, na.rm = TRUE),
      AST   = sum(AST, na.rm = TRUE),
      TO    = sum(TO, na.rm = TRUE),
      OREB  = sum(OREB, na.rm = TRUE),
      DREB  = sum(DREB, na.rm = TRUE),
      STL   = sum(STL, na.rm = TRUE),
      BLK   = sum(BLK, na.rm = TRUE)
    )
  
  career_stats <- totals %>%
    mutate(
      `FG%` = round((`2PM` + `3PM`) / (`2PA` + `3PA`), 3),
      `2P%` = round(`2PM` / `2PA`, 3),
      `3P%` = round(`3PM` / `3PA`, 3),
      `FT%` = round(FTM / FTA, 3)
    ) %>%
    transmute(
      Season = "Career",
      G = GP,
      MP = round(MIN / GP, 1),
      PTS = round(PTS / GP, 1),
      FG = round((`2PM` + `3PM`) / GP, 1),
      FGA = round((`2PA` + `3PA`) / GP, 1),
      `FG%`,  # already computed
      `2P` = round(`2PM` / GP, 1),
      `2PA` = round(`2PA` / GP, 1),
      `2P%`,  # already computed
      `3P` = round(`3PM` / GP, 1),
      `3PA` = round(`3PA` / GP, 1),
      `3P%`,  # already computed
      FT = round(FTM / GP, 1),
      FTA = round(FTA / GP, 1),
      `FT%`,  # already computed
      AST = round(AST / GP, 1),
      TOV = round(TO / GP, 1),
      ORB = round(OREB / GP, 1),
      DRB = round(DREB / GP, 1),
      STL = round(STL / GP, 1),
      BLK = round(BLK / GP, 1)
    )
  
  final_data <- bind_rows(cleaned, career_stats)
  
  gt_table <- final_data %>%
    gt() %>%
    gt_theme_f5() %>%
    tab_header(title = md(paste0("EuroCup Per-Game Stats"))) %>%
    fmt_integer(columns = G) %>%
    fmt_number(columns = setdiff(names(final_data), c("Season", "G", "FG%", "2P%", "3P%", "FT%")), decimals = 1) %>%
    fmt_percent(columns = c(`FG%`, `2P%`, `3P%`, `FT%`), decimals = 1) %>%
    data_color(
      columns = `FG%`,
      colors = scales::col_numeric(
        palette = c("#e63946", "#f1a85b" , "#f1faee", "#90be6d" , "#52b788"),
        domain = c(0.34, 0.65)
      )
    ) %>%
    data_color(
      columns = `2P%`,
      colors = scales::col_numeric(
        palette = c("#e63946", "#f1a85b" , "#f1faee", "#90be6d" , "#52b788"),
        domain = c(0.36, 0.65)
      )
    ) %>%
    data_color(
      columns = `3P%`,
      colors = scales::col_numeric(
        palette = c("#e63946", "#f1a85b" , "#f1faee", "#90be6d" , "#52b788"),
        domain = c(0.23, 0.45)
      )
    ) %>%
    data_color(
      columns = `FT%`,
      colors = scales::col_numeric(
        palette = c("#e63946", "#f1a85b" , "#f1faee", "#90be6d" , "#52b788"),
        domain = c(0.28, 1.20)
      )
    ) %>%
    
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = nrow(final_data))
    ) %>%
    
    tab_style(
      style = cell_text(
        color = "black",
        weight = "normal"  # slightly bold but not heavy
      ),
      locations = cells_body()
    )
    
  # Save as HTML (supports full font rendering)
  file_name <- paste0(first_name, "_", last_name, "_eurocup_per_game.html")
  gt_save_crop(gt_table, file = paste0(save_path, "/", first_name, "_", last_name, "_eurocup_table.png"), bg = "white")
  message("✅ HTML table saved to: ", file.path(save_path, file_name))
  
  return(gt_table)
}


# Example usage:
create_eurocup_table("Tarik", "Phillip")
