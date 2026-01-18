
generate_team_advanced_metrics <- function(season_code, game_code, team_code, save_path = NULL) {
  library(euroleaguer)
  library(dplyr)
  library(gt)
  library(scales)
  library(stringr)
  library(glue)
  library(tidyr)
  
  
  # GT Theme Function
  gt_theme_f5 <- function(gt_object, ...) {
    gt_object %>%
      opt_table_font(
        font = list(google_font("Roboto"), default_fonts()),
        weight = 400
      ) %>%
      tab_style(
        style = cell_text(font = google_font("Roboto"), weight = 700),
        locations = cells_title("title")
      ) %>%
      tab_style(
        style = list(
          cell_text(font = google_font("Roboto"), v_align = "bottom", size = px(12), weight = 'bold', color = "black"),
          cell_fill(color = "white")
        ),
        locations = cells_body()
      ) %>%
      tab_options(
        heading.align = "left",
        column_labels.font.weight = "bold",
        table.font.size = 12,
        table.border.top.style = "none",
        table.border.bottom.style = "none",
        data_row.padding = px(6),
        row_group.padding = px(6),
        column_labels.padding = px(6),
        ...
      )
  }
  
  # Compute Metrics
  get_team_advanced_metrics <- function(box, team_code) {
    team <- box$TeamStats %>% filter(TeamCode == team_code)
    opp  <- box$TeamStats %>% filter(TeamCode != team_code)
    
    pts_team <- team$PTS
    pts_opp <- opp$PTS
    
    fga_team <- team$`2PA` + team$`3PA`
    fta_team <- team$FTA
    to_team <- team$TO
    oreb_team <- team$OREB
    
    fga_opp <- opp$`2PA` + opp$`3PA`
    fta_opp <- opp$FTA
    to_opp <- opp$TO
    oreb_opp <- opp$OREB
    
    poss_team <- fga_team + 0.44 * fta_team + to_team - oreb_team
    poss_opp  <- fga_opp + 0.44 * fta_opp + to_opp - oreb_opp
    possessions <- 0.5 * (poss_team + poss_opp)
    pace <- round(possessions, 1)
    
    fgm <- team$`2PM` + team$`3PM`
    three_pm <- team$`3PM`
    ftm <- team$FTM
    
    efg <- (fgm + 0.5 * three_pm) / fga_team
    to_pct <- to_team / poss_team
    orb_pct <- oreb_team / (oreb_team + opp$DREB)
    ft_rate <- ftm / fga_team
    
    ortg <- round(100 * pts_team / possessions, 1)
    drtg <- round(100 * pts_opp / possessions, 1)
    netrtg <- round(ortg - drtg, 1)
    
    tibble(
      ORtg = ortg,
      DRtg = drtg,
      NetRtg = netrtg,
      Pace = pace,
      eFG = percent(efg, 0.1),
      TO = percent(to_pct, 0.1),
      ORB = percent(orb_pct, 0.1),
      FTR = percent(ft_rate, 0.1)
    )
  }
  
 
  # Scoring Opportunities
  get_scoring_opportunities <- function(pbp_df, player_stats_df, team_code) {
    bench_ids <- player_stats_df %>%
      filter(TeamCode == team_code, IsStarter == 0) %>%
      pull(Player_ID)
    
    list(
      FastbreakPoints = pbp_df %>%
        filter(TeamCode == team_code, Fastbreak == "1") %>%
        summarise(pts = sum(Points, na.rm = TRUE)) %>%
        pull(pts),
      
      PointsOffTurnovers = pbp_df %>%
        filter(TeamCode == team_code, PointsOffTurnover == "1") %>%
        summarise(pts = sum(Points, na.rm = TRUE)) %>%
        pull(pts),
      
      SecondChancePoints = pbp_df %>%
        filter(TeamCode == team_code, SecondChance == "1") %>%
        summarise(pts = sum(Points, na.rm = TRUE)) %>%
        pull(pts),
      
      BenchPoints = pbp_df %>%
        filter(TeamCode == team_code, Player_ID %in% bench_ids) %>%
        summarise(pts = sum(Points, na.rm = TRUE)) %>%
        pull(pts)
    )
  }
  
  # Load Data
  box <- getGameBoxScore(season_code, game_code)
  pbp <- getGamePoints(season_code, game_code)
  
  metrics <- get_team_advanced_metrics(box, team_code)
  scoring <- get_scoring_opportunities(pbp, box$PlayerStats, team_code)
  

  # Build GT Table
  stats_tbl <- tibble(
    Category = c(
      "Scoring Opportunities", "Scoring Opportunities", "Scoring Opportunities", "Scoring Opportunities",
      "Efficiency Metrics", "Efficiency Metrics", "Efficiency Metrics",
      "Pace & Four Factors", "Pace & Four Factors", "Pace & Four Factors", "Pace & Four Factors", "Pace & Four Factors"
    ),
    Metric = c(
      "Fastbreak Points", "Points off Turnovers", "Second Chance Points", "Bench Points",
      "Offensive Rating", "Defensive Rating", "Net Rating",
      "Pace", "Effective FG%", "Turnover %", "Offensive Rebound %", "Free Throw Rate"
    ),
    Value = c(
      scoring$FastbreakPoints,
      scoring$PointsOffTurnovers,
      scoring$SecondChancePoints,
      scoring$BenchPoints,
      metrics$ORtg,
      metrics$DRtg,
      metrics$NetRtg,
      metrics$Pace,
      metrics$eFG,
      metrics$TO,
      metrics$ORB,
      metrics$FTR
    )
  )
  
  gt_table <- stats_tbl %>%
    gt(groupname_col = "Category") %>%
    gt_theme_f5() %>%
    tab_header(title = md("**Advanced Team Metrics**")) %>%
    cols_label(Metric = "Metric", Value = "Value") %>%
    cols_align("center", columns = Value) %>%
    cols_align("left", columns = Metric) %>%
    tab_style(
      style = list(
        cell_fill(color = "gray80"),
        cell_text(weight = "bold", size = px(12), color = "black"),
        cell_borders(sides = c("top", "bottom"), color = "black", weight = px(2))
      ),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#49BED8"),
        cell_text(color = "white", size = px(12))
      ),
      locations = cells_column_labels()
    ) %>%
    cols_width(Metric ~ px(200), Value ~ px(80))
  

  # Export
  if (is.null(save_path)) {
    save_path <- glue("Advanced_Team_Metrics_{team_code}_{game_code}.png")
  }
  
  gtsave(gt_table, filename = save_path, vwidth = 3200, vheight = 2400, expand = 10)
  message(glue("✅ Metrics saved to: {save_path}"))
}

generate_team_advanced_metrics(season_code = "U2025", game_code = 46, team_code = "TTK")

