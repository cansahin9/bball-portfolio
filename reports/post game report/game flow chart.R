
setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/reports/post game report")


library(tidyverse)
library(euroleaguer)

theme_owen <- function () {
  theme_minimal(base_size = 12, base_family = "Roboto") %+replace%
    theme(
      panel.grid = element_line(color = "gray80", linewidth = 0.7),  # Add this line
      panel.grid.major.y = element_line(color = "gray80", linewidth = 0.7),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'white', color = NA),
      panel.background = element_rect(fill = 'white', color = NA)
    )
}


generate_game_flow_chart <- function(season_code, game_code, team_code) {
  # Get data
  pbp_raw <- getGamePlayByPlay(season_code = season_code, game_code = game_code)
  pbp_df <- pbp_raw$PlayByPlay
  summary <- pbp_raw$PlayByPlaySummary
  box <- getGameBoxScore(season_code = season_code, game_code = game_code)
  
  # Match team names to codes
  team_name <- if (summary$CodeTeamA == team_code) summary$TeamA else summary$TeamB
  opp_name  <- if (summary$CodeTeamA == team_code) summary$TeamB else summary$TeamA
  
  # Prepare play-by-play: game time in seconds, cumulative score differential
  pbp <- pbp_df %>%
    mutate(
      MinuteStr = str_sub(Markertime, 1, 2),
      SecondStr = str_sub(Markertime, 4, 5),
      Minute = suppressWarnings(as.numeric(MinuteStr)),
      Second = suppressWarnings(as.numeric(SecondStr)),
      game_seconds = (Quarter - 1) * 600 + (60 * (10 - Minute) + (60 - Second))
    ) %>%
    arrange(game_seconds) %>%
    fill(PointsA, PointsB, .direction = "down") %>%
    filter(!is.na(PointsA) & !is.na(PointsB)) %>%
    group_by(game_seconds) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(
      PointsA = as.numeric(PointsA),
      PointsB = as.numeric(PointsB),
      diff = if (team_code == summary$CodeTeamA) {
        PointsA - PointsB
      } else {
        PointsB - PointsA
      },
      diff_pos = ifelse(diff > 0, diff, 0),
      diff_neg = ifelse(diff < 0, diff, 0)
    )
  
  # Quarter-by-quarter scoring for differential labels
  quarter_scores <- box$ByQuarter %>%
    filter(str_to_upper(Team) == str_to_upper(team_name)) %>%
    pivot_longer(cols = starts_with("Quarter"), names_to = "Quarter", values_to = "Points") %>%
    mutate(Quarter = str_remove(Quarter, "Quarter"))
  
  opp_quarter_scores <- box$ByQuarter %>%
    filter(str_to_upper(Team) == str_to_upper(opp_name)) %>%
    pivot_longer(cols = starts_with("Quarter"), names_to = "Quarter", values_to = "Points") %>%
    mutate(Quarter = str_remove(Quarter, "Quarter"))
  
  net_quarters <- quarter_scores %>%
    mutate(
      OppPoints = opp_quarter_scores$Points,
      Quarter = as.numeric(Quarter)
    ) %>%
    arrange(Quarter) %>%
    mutate(
      CumPoints = cumsum(Points),
      CumOppPoints = cumsum(OppPoints),
      Net = CumPoints - CumOppPoints,
      NetSign = ifelse(Net > 0, paste0("+", Net), as.character(Net)),
      Label = paste0("Q", Quarter, ": ", NetSign),
      Time = (Quarter - 1) * 600 + 300  # midpoint of each quarter
    )
  
  x_min <- min(pbp$game_seconds, na.rm = TRUE)
  max_y <- max(pbp$diff, na.rm = TRUE)
  label_y <- min(max_y + 5, 18)  # cap label Y position so it doesn't go beyond 20
  print(net_quarters)
  
  # Plot
  ggplot(pbp, aes(x = game_seconds)) +
    # Positive (team leading) area
    geom_ribbon(aes(ymin = 0, ymax = diff_pos),
                fill = ifelse(team_code == summary$CodeTeamA, "black","#49BED8"),
                alpha = 0.3) +
    
    # Negative (team trailing) area
    geom_ribbon(aes(ymin = diff_neg, ymax = 0),
                fill = "lightgray", alpha = 0.3) +
    
    # Line for game flow
    geom_line(
      aes(y = diff),
      color = ifelse(team_code == summary$CodeTeamA, "black","#49BED8"),
      linewidth = 1
    ) +
    geom_vline(xintercept = c(600, 1200, 1800), linetype = "dashed", color = "gray60") +
    geom_hline(yintercept = 0, linewidth = 0.6, color = "gray35") +  # bold horizontal center line
    geom_vline(xintercept = x_min, linewidth = 1.1, color = "black") +   # thick vertical line at game start
    geom_label(
      data = net_quarters,
      aes(x = Time, y = label_y, label = Label),
      size = 4,
      fill = "gray90",
      color = "black",
      label.size = 0.6,
      label.r = unit(0.25, "lines"),
      label.padding = unit(0.35, "lines"),  # more breathing room
      fontface = "bold",
      family = "Roboto"
    ) +
    theme_owen() +
    scale_x_continuous(
      limits = c(x_min, max(pbp$game_seconds, na.rm = TRUE)),
      breaks = NULL,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(-20, 20),
      breaks = c(-20, -10, 0, 10, 20),
      expand = c(0, 0)
    ) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(
      breaks = NULL,
      expand = expansion(add = c(0, 0))
    ) +
    labs(
      title = "Game Flow"
    ) +
    theme(
      axis.title = element_blank(),  # Removes "diff" label
      axis.text.x = element_blank(),  # Hides x-axis numbers
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(face = "bold", size = 12, family = "Roboto", color = "black"),
      plot.title = element_text(face = "bold", size = 24, family = "Roboto", hjust = 0),
    )
}

generate_game_flow_chart(season_code = "U2025", game_code = 46, team_code = "LLI")  # London Lions
generate_game_flow_chart(season_code = "U2025", game_code = 46, team_code = "TTK")  

ggsave("game_flow_lions_telekom2.png", width = 12, height = 6, dpi = 300)


