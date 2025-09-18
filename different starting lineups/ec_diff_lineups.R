# Set working directory
setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/different_lineups")

# Load packages
library(tidyverse)
library(extrafont)
library(janitor)

# Custom theme
theme_owen <- function () {
  theme_minimal(base_size=10, base_family="Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

# STEP 1: Add team color dictionary (your custom EuroCup colors)
eurocup_colors <- tribble(
  ~TeamCode, ~primary,
  "CAN", "#FFB718",    # Gran Canaria
  "HTA", "#D50A0A",    # Hapoel Tel Aviv
  "BAH", "#002F6C",    # Bahcesehir
  "JER", "#B30B0B",    # Hapoel Jerusalem
  "BOU", "#C8102E",    # JL Bourg
  "PAM", "#F47920",    # Valencia
  "CLU", "#000025",    # Cluj Napoca
  "LJU", "#007A33",    # Olimpija Ljubljana
  "BUD", "#0033A0",    # Buducnost
  "BES", "#000000",    # Besiktas
  "WOL", "#00205B",    # Wolves Vilnius
  "VNC", "#9E1B32",    # Venezia
  "TTK", "#005BAC",    # Turk Telekom
  "ULM", "#F39200",    # Ulm
  "JOV", "#007F3E",    # Joventut
  "LKB", "#56008C",    # Lietkabelis
  "TRN", "#A6A6A6",    # Trento
  "HAM", "#231F20",    # Hamburg
  "TSO", "#998000",    # Trefl Sopot
  "ARI", "#c9aa00"     # Aris
)

# Optional: Friendly team names (for facet titles later)
team_names <- tribble(
  ~TeamCode, ~TeamName,
  "CAN", "Gran Canaria",
  "HTA", "Hapoel TA",
  "BAH", "Bahcesehir",
  "JER", "Hapoel JLM",
  "BOU", "JL Bourg",
  "PAM", "Valencia",
  "CLU", "Cluj Napoca",
  "LJU", "Olimpija",
  "BUD", "Buducnost",
  "BES", "Besiktas",
  "WOL", "Wolves",
  "VNC", "Venezia",
  "TTK", "Turk Telekom",
  "ULM", "Ulm",
  "JOV", "Joventut",
  "LKB", "Lietkabelis",
  "TRN", "Trento",
  "HAM", "Hamburg",
  "TSO", "Trefl Sopot",
  "ARI", "Aris"
)

# Re-fetch lineup data if not already done
# Get games and teams
teams <- euroleaguer::getCompetitionTeams("U2024")
rounds <- euroleaguer::getCompetitionRounds("U2024")

games <- map_dfr(rounds$Round, function(rnd) {
  euroleaguer::getCompetitionGames(season_code = "U2024", round = rnd)
})

# Define helper to extract starting lineups
get_game_lineup <- function(season_code, game_code) {
  box <- euroleaguer::getGameBoxScore(season_code, game_code)
  if (is.null(box)) return(NULL)
  
  if (!"PlayerStats" %in% names(box) || nrow(box$PlayerStats) == 0) return(NULL)
  
  starters <- box$PlayerStats %>%
    filter(IsStarter == 1) %>%
    select(TeamCode, Player) %>%
    group_by(TeamCode) %>%
    summarise(StartingLineup = paste(sort(Player), collapse = " / "), .groups = "drop")
  
  if (nrow(starters) == 0) return(NULL)
  
  tibble(
    GameCode = game_code,
    TeamCode = starters$TeamCode,
    StartingLineup = starters$StartingLineup
  )
}

# Get lineups
lineups_raw <- map_dfr(games$GameCode, function(gc) {
  tryCatch({
    get_game_lineup("U2024", gc)
  }, error = function(e) {
    message("Failed GameCode: ", gc)
    return(NULL)
  })
})

# Final `lineups_df` with date joined
lineups_df <- lineups_raw %>%
  left_join(games %>% select(GameCode, GameDate = Date), by = "GameCode") %>%
  filter(!is.na(StartingLineup))

# STEP 2: Process the lineup data
df <- lineups_df %>%
  arrange(TeamCode, GameDate) %>%
  group_by(TeamCode) %>%
  mutate(
    g = row_number(),
    starting_lineup = StartingLineup,
    cum_unique_lineups = cumsum(!duplicated(starting_lineup)),
    total_lineups = n_distinct(starting_lineup),
    gp = max(g),
    pct = total_lineups / gp
  ) %>%
  ungroup()

# STEP 3: Merge team names and colors
df <- df %>%
  left_join(team_names, by = "TeamCode") %>%
  left_join(eurocup_colors, by = "TeamCode") %>%
  arrange(desc(pct), TeamCode) %>%
  mutate(team = fct_reorder(TeamCode, total_lineups, .desc = TRUE))

# STEP 4: Final game marker data
df_last <- df %>%
  group_by(team) %>%
  filter(g == max(g)) %>%
  select(team, TeamName, g, cum_unique_lineups, primary)

# STEP 5: Duplicate line column for background lines
df$teamDuplicate <- df$team

# === 🟢 PLOT ===
df %>%
  ggplot(aes(x = g, y = cum_unique_lineups)) +
  geom_line(data = mutate(df, team = NULL), aes(group = teamDuplicate),
            colour = 'grey80', size = .5, alpha = .5) +
  geom_line(aes(group = team, color = primary), size = .75, alpha = 1) +
  geom_point(data = df_last,
             aes(x = g, y = cum_unique_lineups, fill = primary),
             shape = 21, color = "black", size = 2.5, stroke = 0.4)+
  geom_text(data = df_last, aes(x = g, y = cum_unique_lineups + 1.5, label = TeamName, color = primary),
            family = "Consolas", alpha = 1, nudge_x = 3, size = 3, fontface = 'bold') +
  # scale_x_continuous(limits = c(-7, max(df$g) + 7), breaks = pretty(df$g))+
  # scale_y_continuous(limits = c(0, max(df$cum_unique_lineups) + 5), breaks = pretty(df$cum_unique_lineups)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 25, 5)) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_owen() +
  facet_wrap(~team, nrow = 4, strip.position = 'top') +
  coord_cartesian(clip = 'off') +
  theme(
    legend.position = 'none',
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    plot.title.position = 'plot',
    plot.title = element_text(face = 'bold', size = 10),
    plot.subtitle = element_text(size = 8),
    plot.margin = unit(c(.5, .5, 1, .5), "lines"),
    plot.background = element_rect(fill = 'floralwhite', color = NA),
    panel.background = element_rect(fill = "floralwhite", color = NA)
  ) +
  labs(
    x = "Games Played",
    y = "Cumulative Number Of Different Starting Lineups Used",
    title = "Cumulative Number of Different Starting Lineups Used By Games Played",
    subtitle = "2024–25 EuroCup Season",
    caption = "Data: euroleaguer | Plot: Can Sahin"
  )

# Save to file
ggsave("eurocup_different_starting_lineups.png", width = 10, height = 8, dpi = 300)
