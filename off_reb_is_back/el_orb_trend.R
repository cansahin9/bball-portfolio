library(euroleaguer)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(scales)
library(paletteer)
library(ggtext)

# Define EuroLeague seasons
seasons <- paste0("E", 2005:2025)

# Function to get league-wide ORB% from team stats
get_orbpct_all_games <- function(season_code) {
  message(paste("Processing:", season_code))
  
  stats <- tryCatch({
    getTeamLeadStats(season_code = season_code, phase_type = "All")$TeamAccumulated
  }, error = function(e) {
    message(paste("  Skipped:", season_code, "—", e$message))
    return(NULL)
  })
  
  if (is.null(stats) || !"OREB" %in% names(stats) || !"DREB" %in% names(stats)) {
    return(NULL)
  }
  
  oreb <- sum(stats$OREB, na.rm = TRUE)
  dreb <- sum(stats$DREB, na.rm = TRUE)
  orb_pct <- round(oreb / (oreb + dreb), 4)
  
  tibble(
    Season = season_code,
    ORB = orb_pct
  )
}

# Run loop over all seasons
orb_trend_euroleague <- map_dfr(seasons, get_orbpct_all_games)

# View result
print(orb_trend_euroleague)

# Format year to 06–25 format
orb_trend_euroleague <- orb_trend_euroleague %>%
  mutate(
    year = as.integer(gsub("E", "", Season)),
    label = sprintf("%02d", year %% 100),
    type = "EuroLeague" # just 1 line for now, but matches SLB structure
  )

years <- orb_trend_euroleague$year

# Caption
caption <- "**Data**: euroleagueR | **Graphic**: Can Sahin"

# Plot
p <- orb_trend_euroleague %>%
  ggplot(aes(x = year, y = ORB, group = type)) +
  geom_line(aes(color = type), linewidth = 1.25) +
  geom_point(aes(fill = type), shape = 21, show.legend = FALSE) +
  geom_smooth(method = "loess", se = FALSE, linetype = 5, color = "gray50", size = 0.8) +
  scale_x_continuous(
    breaks = years,
    labels = sprintf("%02d", (years) %% 100)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.285, 0.325),
    breaks = seq(0.29, 0.32, 0.01)
  ) +
  scale_color_paletteer_d("ggsci::nrc_npg", direction = 1, labels = "EuroLeague") +
  scale_fill_paletteer_d("ggsci::nrc_npg", direction = 1) +
  theme_minimal(base_family = "Oswald") +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption = element_markdown(size = 7, hjust = 0),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(family = "", face = "italic", size = 10.5),
    plot.margin = margin(b = 25, t = 25, r = 50, l = 50),
    axis.text = element_text(size = 7)
  ) +
  labs(
    color = "",
    x = "Years",
    y = "ORB%",
    title = "EuroLeague ORB% Trend",
    subtitle = "2006–2025 • Regular Season + Playoffs + F4",
    caption = caption
  )

# Save the plot
ggsave(
  "euroleague_orb_trend.png",
  width = 10, height = 6, dpi = 300, bg = "white"
)

eurocup_seasons <- getCompetitionHistory("U")
head(eurocup_seasons)

