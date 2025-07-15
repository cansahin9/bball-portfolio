library(dplyr)
library(purrr)
library(tibble)
library(euroleaguer)
library(ggplot2)
library(scales)
library(ggtext)  # for caption formatting
library(paletteer)

# Get EuroCup seasons
eurocup_seasons <- getCompetitionHistory("U")$SeasonCode

# Calculate league-wide ORB% by season
orb_trend_eurocup <- purrr::map_dfr(eurocup_seasons, function(season) {
  message("Processing EuroCup season: ", season)
  
  tryCatch({
    stats <- getTeamLeadStats(season_code = season, phase_type = "All")$TeamAccumulated
    
    oreb <- sum(stats$OREB, na.rm = TRUE)
    dreb <- sum(stats$DREB, na.rm = TRUE)
    orb_pct <- round(oreb / (oreb + dreb), 4)
    
    tibble(
      Season = season,
      Year = as.integer(substr(season, 2, 5)) + 1,  # U2023 → 2024
      ORB = orb_pct
    )
  }, error = function(e) {
    message("Skipping ", season, ": ", e$message)
    return(NULL)
  })
})

years <- orb_trend_eurocup$Year

p_eurocup <- orb_trend_eurocup %>%
  ggplot(aes(x = Year, y = ORB, group = 1)) +
  geom_line(color = "#2C77A7", linewidth = 1.25) +
  geom_point(shape = 21, fill = "#2C77A7", color = "black", stroke = 0.5, size = 2.5) +
  geom_smooth(method = "loess", se = FALSE, linetype = 5, color = "gray50", linewidth = 1) +
  scale_y_continuous(
    limits = c(min(orb_trend_eurocup$ORB) - 0.01, max(orb_trend_eurocup$ORB) + 0.005),
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = years,
    labels = sprintf("%02d", years %% 100)
  ) +
  theme_minimal(base_family = "Oswald") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(face = "italic", size = 10.5, color = "gray40"),
    axis.text.x = element_text(size = 8),
    plot.caption = element_markdown(size = 7, hjust = 0),
    plot.margin = margin(t = 25, b = 25, l = 50, r = 50)
  ) +
  labs(
    x = "Years",
    y = "ORB%",
    title = "EuroCup ORB% Trend",
    subtitle = "2003–2024 • Regular Season + Playoffs",
    caption = "**Data**: euroleagueR | **Graphic**: Can Sahin"
  )

ggsave("eurocup_orbpct_trend.png", p_eurocup, width = 10, height = 6, dpi = 320)
