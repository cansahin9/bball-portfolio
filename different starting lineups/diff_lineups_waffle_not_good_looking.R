# Load required packages
library(tidyverse)
library(janitor)
library(paletteer)
library(waffle)
library(forcats)
library(euroleaguer)
library(extrafont)
loadfonts(device = "win")

# Custom theme
theme_owen <- function () {
  theme_minimal(base_size = 10, base_family = "Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0.5),
      panel.spacing = unit(1, "lines"),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "none"
    )
}

# --- Data extraction ------------------------------------------------------------

# Helper: get starting lineup from box score
get_game_lineup <- function(season_code, game_code) {
  box <- euroleaguer::getGameBoxScore(season_code, game_code)
  if (is.null(box) || !"PlayerStats" %in% names(box) || nrow(box$PlayerStats) == 0) return(NULL)
  
  starters <- box$PlayerStats %>%
    filter(IsStarter == 1) %>%
    select(TeamCode, Player) %>%
    group_by(TeamCode) %>%
    summarise(
      StartingLineup = paste(sort(Player), collapse = " / "),
      .groups = "drop"
    )
  
  if (nrow(starters) == 0) return(NULL)
  
  tibble(
    GameCode = game_code,
    TeamCode = starters$TeamCode,
    StartingLineup = starters$StartingLineup
  )
}

# Pull games
rounds <- euroleaguer::getCompetitionRounds("U2024")
games <- map_dfr(rounds$Round, function(rnd) {
  euroleaguer::getCompetitionGames("U2024", rnd)
})

# Pull lineups
lineups_raw <- map_dfr(games$GameCode, function(gc) {
  tryCatch({
    get_game_lineup("U2024", gc)
  }, error = function(e) {
    message("Failed GameCode: ", gc)
    return(NULL)
  })
})

# Merge game dates
lineups_df <- lineups_raw %>%
  left_join(games %>% select(GameCode, GameDate = Date), by = "GameCode") %>%
  filter(!is.na(StartingLineup))

# --- Process for waffle chart --------------------------------------------------

df_waffle <- lineups_df %>%
  arrange(TeamCode, GameDate) %>%
  group_by(TeamCode) %>%
  mutate(
    game_no = row_number(),
    lineup_id = as.integer(factor(StartingLineup, levels = unique(StartingLineup))),
    team_label = paste0(TeamCode, " (", n_distinct(StartingLineup), ")")
  ) %>%
  count(team_label, lineup_id) %>%
  ungroup() %>%
  mutate(team_label = fct_reorder(team_label, desc(n), .fun = sum))

df_waffle <- df_waffle %>%
  mutate(team_label = fct_reorder(team_label, lineup_id, .fun = max, .desc = TRUE))
# --- Plot ----------------------------------------------------------------------

ggplot(df_waffle, aes(fill = factor(lineup_id), values = n)) +
  geom_waffle(n_rows = 5, size = 0.3, color = "floralwhite", flip = TRUE) +
  facet_wrap(~team_label, nrow = 5, strip.position = "bottom") +
  scale_fill_paletteer_d("ggsci::category20_d3") +
  coord_equal(clip = "off") +
  theme_owen() +
  labs(
    title = "Number Of Starting Lineups Used By Each EuroCup Team (2024)",
    subtitle = paste0("Each square represents one game. Each color = a unique starting lineup.\nData from euroleaguer | Updated ", format(Sys.Date(), "%B %d, %Y")),
    caption = ""
  )
ggsave("waffle_ec.png", width = 10, height = 7, dpi = 300)
