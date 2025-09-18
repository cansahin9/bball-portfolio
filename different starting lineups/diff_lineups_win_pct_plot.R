# === 📚 LOAD LIBRARIES ===
library(tidyverse)
library(euroleaguer)
library(prismatic)
library(scales)
library(ggtext)
library(extrafont)

# === 🧭 SET PARAMETERS ===
season_code <- "U2024"

# === 1️⃣ GET ROUNDS + GAMES ===
rounds <- getCompetitionRounds(season_code = season_code)
round_codes <- rounds$Round

games_list <- purrr::map_dfr(round_codes, function(rnd) {
  tryCatch({
    getCompetitionGames(season_code = season_code, round = rnd)
  }, error = function(e) NULL)
})

game_codes <- games_list$GameCode %>% unique() %>% sort()

# === 2️⃣ GET STARTING LINEUPS ===
eurocup_lineups <- purrr::map_dfr(game_codes, function(code) {
  tryCatch({
    box <- getGameBoxScore(season_code = season_code, game_code = code)
    game_meta <- games_list %>% filter(GameCode == code) %>% select(PhaseTypeCode)
    
    box$PlayerStats %>%
      filter(IsStarter == 1) %>%
      mutate(PhaseTypeCode = game_meta$PhaseTypeCode[1]) %>%
      group_by(TeamCode, GameCode, PhaseTypeCode) %>%
      summarise(starting_lineup = paste(sort(Player_ID), collapse = "-"), .groups = "drop")
  }, error = function(e) NULL)
})

# === 3️⃣ CALCULATE LINEUP VOLATILITY ===
df_lineups <- eurocup_lineups %>%
  filter(PhaseTypeCode == "RS") %>%
  group_by(TeamCode) %>%
  summarise(
    games_played = n_distinct(GameCode),
    unique_lineups = n_distinct(starting_lineup),
    pct_lineup_volatility = unique_lineups / games_played
  )


# === 4️⃣ GET STANDINGS ===
latest_rs_round <- rounds %>%
  filter(PhaseTypeCode == "RS") %>%
  arrange(desc(Round)) %>%
  slice(1) %>%
  pull(Round)

standings <- getCompetitionStandings(season_code = season_code, round = latest_rs_round)

# Rebuild win % correctly from standings
df_standings <- standings %>%
  select(TeamCode, TeamName, GamesPlayed, GamesWon, GamesLost) %>%
  mutate(
    win_pct = GamesWon / GamesPlayed
  )
# === 🎨 TEAM COLORS ===
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

# === 📛 FRIENDLY NAMES ===
team_names <- tribble(
  ~TeamCode, ~FriendlyName,
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

short_codes <- tribble(
  ~TeamCode, ~ShortLabel,
  "CAN", "GCA",   # Gran Canaria
  "PAM", "VAL",   # Valencia
  "HTA", "HTA",
  "BAH", "BAH",
  "JER", "JER",
  "BOU", "BOU",
  "CLU", "CLU",
  "LJU", "LJU",
  "BUD", "BUD",
  "BES", "BES",
  "WOL", "WOL",
  "VNC", "VNC",
  "TTK", "TTK",
  "ULM", "ULM",
  "JOV", "JOV",
  "LKB", "LKB",
  "TRN", "TRN",
  "HAM", "HAM",
  "TSO", "TSO",
  "ARI", "ARI"
)

df_scatter <- df_lineups %>% 
  left_join(df_standings, by = "TeamCode") %>%
  mutate(win_pct = GamesWon / GamesPlayed) %>%
  left_join(eurocup_colors, by = "TeamCode") %>%
  left_join(team_names, by = "TeamCode") %>%
  left_join(short_codes, by = "TeamCode") %>%   # 👈 add short codes here
  mutate(
    primary = ifelse(is.na(primary), "#CCCCCC", primary),
    label   = ShortLabel   # 👈 use short label always
  ) %>%
  select(TeamCode, GamesPlayed, GamesWon, GamesLost,
         unique_lineups, pct_lineup_volatility, win_pct, primary, label)



# === CUSTOM THEME ===
theme_owen <- function () {
  theme_minimal(base_size = 10, base_family = "Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

# === FINAL PLOT ===
ggplot(df_scatter, aes(x = pct_lineup_volatility, y = win_pct)) +
  geom_point(
    aes(fill = primary, color = after_scale(clr_darken(fill, 0.3))),
    shape = 21, size = 11, stroke = 1.25, alpha = 0.9
  ) +
  geom_text(
    aes(label = label),
    color = "white",
    family = "Consolas", fontface = "bold", size = 3.25, hjust = 0.5
  ) +
  geom_hline(yintercept = mean(df_scatter$win_pct, na.rm = TRUE), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(df_scatter$pct_lineup_volatility, na.rm = TRUE), linetype = "dashed", alpha = 0.5) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = label_number(accuracy = 0.1)
  ) +
  theme_owen() +
  labs(
    title = "Stable Starters, Stronger Teams?",
    subtitle = "EuroCup 2024–25 Regular Season | Win % vs. Starting Lineup Volatility",
    x = "Number Of Different Starting Lineups Used / Games Played",
    y = "Win Percentage",
    caption = "Data: euroleaguer | Plot: Can Sahin"
  ) +
  theme(
    plot.title = element_text(face = 'bold', size = 14),
    plot.subtitle = element_text(size = 7.5),
    axis.title = element_text(size = 9),
    plot.title.position = 'plot',
    plot.margin = unit(c(.5, .5, .5, .5), "lines"),
    plot.caption = element_text(hjust = 1, size = 6)
  )

# === SAVE PLOT ===
ggsave("eurocup_lineup_continuity_vs_win_pct_2025.png", width = 6, height = 6, dpi = 300)
