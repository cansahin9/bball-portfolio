# ───── SETUP ─────
setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/reports/post game report")

# ───── Setup ─────
library(euroleaguer)
library(dplyr)
library(gt)
library(scales)
library(purrr)
library(tidyverse)
library(ggforce)
library(ggplot2)
library(colorspace)


# ───── Constants ─────
backboard_offset <- 4
neck_length <- 0.5
hoop_radius <- 0.75
hoop_center_y <- backboard_offset + neck_length + hoop_radius  # = 5.25

# ───── Game & Team Selection ─────
game_code <- 36
team_code <- "LLI"

# ───── Load Game & League Data ─────
game_data <- getGamePoints(season_code = "U2025", game_code = game_code)

# Team Shots
team_shots <- game_data %>%
  filter(TeamCode == team_code, Action_ID %in% c("2FGM", "2FGA", "3FGM", "3FGA")) %>%
  mutate(
    x = ((CoordX * 0.55)) * 0.0625,
    y = ((CoordY * 0.45) + 58) * 0.0783,
    isMade = Action_ID %in% c("2FGM", "3FGM"),
    shot_value = ifelse(Action_ID %in% c("2FGM", "2FGA"), 2, 3),
    distance = sqrt(x^2 + (y - hoop_center_y)^2),
    zone = case_when(
      distance <= 4 ~ "Rim",
      y <= 19 & abs(x) <= 8 & distance > 4 ~ "Paint (Non-RA)",
      y <= 10.5 & abs(x) > 20 ~ "Corner 3",
      distance > 22.15 ~ "Above Break 3",
      TRUE ~ "Midrange"
    )
  ) %>% filter(!is.na(zone))

# ✅ Pull all games from Rounds 1–4 (or any range)
games_metadata <- getCompetitionGames(season_code = "U2025", round = 1:4)
game_codes <- unique(games_metadata$GameCode)

# ✅ Pull shot data from those games
safe_get <- purrr::possibly(function(code) getGamePoints(season_code = "U2025", game_code = code), otherwise = NULL)
games <- purrr::map(game_codes, safe_get) %>% purrr::compact()
league_data <- bind_rows(games)

league_shots <- league_data %>%
  filter(Action_ID %in% c("2FGM", "2FGA", "3FGM", "3FGA")) %>%
  mutate(
    x = ((CoordX * 0.55)) * 0.0625,
    y = ((CoordY * 0.45) + 58) * 0.0783,
    isMade = Action_ID %in% c("2FGM", "3FGM"),
    shot_value = ifelse(Action_ID %in% c("2FGM", "2FGA"), 2, 3),
    distance = sqrt(x^2 + (y - hoop_center_y)^2),
    zone = case_when(
      distance <= 4 ~ "Rim",
      y <= 19 & abs(x) <= 8 & distance > 4 ~ "Paint (Non-RA)",
      y <= 10.5 & abs(x) > 20 ~ "Corner 3",
      distance > 22.15 ~ "Above Break 3",
      TRUE ~ "Midrange"
    )
  ) %>% filter(!is.na(zone))

# ───── Calculate PPS + FG% ─────
team_stats <- team_shots %>%
  group_by(zone) %>%
  summarise(
    ATT = n(),
    MD = sum(isMade),
    FG_PCT_Team = round(100 * MD / ATT, 1),
    PPS_Team = round(sum(isMade * shot_value) / ATT, 3),
    .groups = "drop"
  )

league_stats <- league_shots %>%
  group_by(zone) %>%
  summarise(
    FG_PCT_League = round(100 * mean(isMade), 1),
    PPS_League = round(mean(isMade * shot_value), 3),
    .groups = "drop"
  )

# ───── Combine + Diff ─────
zone_comp <- team_stats %>%
  left_join(league_stats, by = "zone") %>%
  mutate(
    Diff_PPS = round(PPS_Team - PPS_League, 3),
    Diff_FG = round(FG_PCT_Team - FG_PCT_League, 1)
  ) %>%
  select(
    Zone = zone,
    MD, ATT, 
    FG_PCT_Team, FG_PCT_League, Diff_FG,
    PPS_Team, PPS_League, Diff_PPS
    
  ) %>%
  arrange(factor(Zone, levels = c("Rim", "Paint (Non-RA)", "Midrange", "Corner 3", "Above Break 3")))

# ───── GT Theme ─────
gt_theme_f5 <- function(gt_object) {
  gt_object %>%
    opt_table_font(font = list(google_font("Roboto"), default_fonts()), weight = 400) %>%
    tab_style(
      style = list(cell_text(font = google_font("Roboto"), weight = 'bold', color = "white"),
                   cell_fill(color = "black")),
      locations = cells_column_labels()
    ) %>%
    tab_options(
      column_labels.background.color = "black",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      table.font.size = 12,
      heading.align = "left",
      table.background.color = "white",
      table_body.hlines.color = 'gray85',
      data_row.padding = px(4)
    )
}

gt_table_clean <- zone_comp %>%
  select(Zone, MD, ATT, FG_PCT_Team, PPS_Team) %>%   # 👈 Swapped columns
  gt() %>%
  gt_theme_f5() %>%
  fmt_number(columns = PPS_Team, decimals = 2) %>%
  fmt_number(columns = FG_PCT_Team, decimals = 1, pattern = "{x}%") %>%
  # subtitle style, font, color and size
  tab_style(
    style = cell_text(size = px(10), color = "gray45"),
    locations = cells_title(groups = "subtitle")
  ) %>%
  # Color PPS
  data_color(
    columns = PPS_Team,
    colors = col_numeric(
      palette = c("#e63946", "#f1faee", "#52b788"),
      domain = range(zone_comp$PPS_Team, league_stats$PPS_League, na.rm = TRUE)  # ✅ Includes both team + league
    )
  ) %>%
  
  # Color FG%
  # data_color(
  #   columns = FG_PCT_Team,
  #   colors = col_numeric(
  #     palette = c("#e63946", "#f1faee", "#52b788"),
  #     domain = range(zone_comp$FG_PCT_Team, league_stats$FG_PCT_League, na.rm = TRUE)
  #   )
  # ) %>%
  
  cols_label(
    MD = "Made",
    ATT = "Attempted",
    PPS_Team = "Points Per Shot",
    FG_PCT_Team = "Field Goal%"
  ) %>%
  tab_header(
    title = md("**Team Shot Efficiency by Zone**")
  ) %>%
  
  # source note 
  tab_source_note(
    source_note = md("Coloring based on League Averages")
  ) %>%
  tab_style(
    style = list(
      cell_text(
        color = "gray45",     # Text color
        size = px(11),        # Font size
        font = google_font("Roboto")
      )
    ),
    locations = cells_source_notes()
  ) %>%
  
  cols_align("center", columns = everything()) %>%
  # column widths
  cols_width(everything() ~ px(90))

gtsave(gt_table_clean, "lions Team_Efficiency_by_Zone_vs_League.png", vwidth = 3800, vheight = 3600, expand = 10)



# -----------------------------------------------
# Percentile table

# Pull all games from Rounds 1–4 (adjust as needed)
games_metadata <- getCompetitionGames(season_code = "U2025", round = 1:4)
game_codes <- unique(games_metadata$GameCode)

# STEP 2: Pull the shot data from those games
safe_get <- purrr::possibly(function(code) getGamePoints(season_code = "U2025", game_code = code), otherwise = NULL)
games <- purrr::map(game_codes, safe_get) %>% purrr::compact()
league_data <- bind_rows(games)

# Create league_shots with zone info
league_shots <- league_data %>%
  filter(Action_ID %in% c("2FGM", "2FGA", "3FGM", "3FGA")) %>%
  mutate(
    x = ((CoordX * 0.55)) * 0.0625,
    y = ((CoordY * 0.45) + 58) * 0.0783,
    isMade = Action_ID %in% c("2FGM", "3FGM"),
    shot_value = ifelse(Action_ID %in% c("2FGM", "2FGA"), 2, 3),
    distance = sqrt(x^2 + (y - hoop_center_y)^2),
    zone = case_when(
      distance <= 4 ~ "Rim",
      y <= 19 & abs(x) <= 8 & distance > 4 ~ "Paint (Non-RA)",
      y <= 10.5 & abs(x) > 20 ~ "Corner 3",
      distance > 22.15 ~ "Above Break 3",
      TRUE ~ "Midrange"
    )
  ) %>%
  filter(!is.na(zone))

# ───── STEP 1: League-wide team-zone PPS & FG% ─────
league_team_zone_stats <- league_shots %>%
  group_by(GameCode, TeamCode, zone) %>%
  summarise(
    ATT = n(),
    Made = sum(isMade),
    PPS = sum(isMade * shot_value) / ATT,
    FG_PCT = Made / ATT,
    .groups = "drop"
  )

# ───── STEP 2: Add percentile ranks per zone ─────
league_percentiles <- league_team_zone_stats %>%
  group_by(zone) %>%
  mutate(
    PPS_percentile = percent_rank(PPS),
    FG_percentile = percent_rank(FG_PCT)
  ) %>%
  ungroup()

# ───── STEP 3: Filter for this game's team ─────
team_percentiles <- league_percentiles %>%
  filter(GameCode == game_code, TeamCode == team_code) %>%
  mutate(
    PPS_percentile = round(PPS_percentile * 100),
    FG_percentile = round(FG_percentile * 100),
    FG_PCT = round(FG_PCT * 100, 1),
    PPS = round(PPS, 2)
  ) %>%
  rename(
    Zone = zone,
    `FG%` = FG_PCT,
    `PPS` = PPS,
    `FG%ile` = FG_percentile,
    `PPS%ile` = PPS_percentile
  ) %>%
  arrange(factor(Zone, levels = c("Rim", "Paint (Non-RA)", "Midrange", "Corner 3", "Above Break 3")))


team_percentiles <- team_percentiles %>%
  select(
    Zone, Made, ATT, `FG%`, PPS, `FG%ile`, `PPS%ile`
  )

team_percentiles <- team_percentiles %>%
  mutate(
    `PPS%ile` = as.numeric(`PPS%ile`),
    `FG%ile` = as.numeric(`FG%ile`)
  )

# ───── STEP 4: Create GT table ─────
gt_percentile_table <- team_percentiles %>%
  gt() %>%
  gt_theme_f5() %>%
  tab_header(
    title = md("**Team Zone Efficiency Percentile Rank**"),
    subtitle = "Percentile vs All Teams Across Rounds 1–4"
  ) %>%
  data_color(
    columns = c(`FG%ile`, `PPS%ile`),
    colors = col_numeric(
      palette = c("#e63946", "#f1faee", "#52b788"),
      domain = c(0, 100)
    )
  ) %>%
  fmt_number(columns = `PPS`, decimals = 2) %>%
  fmt_number(columns = `FG%`, decimals = 1, pattern = "{x}%") %>%
  cols_label(
    Made = "Made",
    ATT = "Att",
    `FG%` = "FG%",
    PPS = "PPS",
    `FG%ile` = "FG% Percentile",
    `PPS%ile` = "PPS Percentile"
  ) %>%
  cols_align("center", columns = everything()) %>%
  tab_source_note(md("*League-wide comparison per shot zone*"))

gtsave(gt_percentile_table, "niners Team_Zone_Efficiency_Percentile_Rank.png", vwidth = 4000, vheight = 3600, expand = 10)

# -----------------------------------------------
# Percentile Wheel Chart
# # ---- Step 1: Pull PPS Percentiles ----
zone_percentiles <- team_percentiles %>%
  select(Zone, PPS_percentile = `PPS%ile`)  # Use dynamically calculated percentiles

zone_data <- tibble(Zone = zone_levels) %>%
  left_join(zone_percentiles, by = "Zone") %>%
  left_join(zone_groups, by = "Zone") %>%
  mutate(
    fill_fg = "black",  # team color
    fill_bg = lighten(fill_fg, amount = 0.8),  # ⚠️ creates a shaded version
    r = PPS_percentile,
    r_draw = ifelse(PPS_percentile == 0, 2, PPS_percentile),
    angle = pi / length(zone_levels),
    start = seq(0, length.out = length(zone_levels)) * angle - pi/2,
    end = start + angle,
    base_mid = (start + end) / 2 - pi/2
  )

zone_data %>% filter(is.na(fill_fg))


ggplot(zone_data) +
  # Background arc — now uses shaded fill
  geom_arc_bar(
    aes(
      start = start, end = end, r0 = 0, r = 100,
      x0 = 0, y0 = 0, fill = fill_bg
    ),
    color = "white", linewidth = 1
  ) +
  
  # Foreground arc
  geom_arc_bar(
    aes(
      start = start, end = end, r0 = 0, r = r_draw,
      x0 = 0, y0 = 0, fill = fill_fg
    ),
    color = "white", linewidth = 1
  ) +
  
  # Percentile labels (use 'th' except for 0)
  geom_label(aes(
    x = 60 * cos(base_mid),
    y = -60 * sin(base_mid),
    label = ifelse(PPS_percentile == 0, "0", paste0(PPS_percentile, "th"))
  ),
  fill = "gray20", color = "white", fontface = "bold", size = 4.5,
  label.size = 0, label.r = unit(0.25, "lines")) +
  
  # Zone labels (use display_label if custom naming needed)
  geom_text(aes(
    x = 113 * cos(base_mid),
    y = -113 * sin(base_mid),
    label = Zone
  ), fontface = "bold", size = 3.5, color = "black") +
  
  scale_fill_identity() +
  coord_fixed(clip = "off") +
  theme_void() +
  labs(
    title = "Points Per Shot Percentile by Zone",
    subtitle = "Compared to All Teams (Rounds 1–4)",
    caption = "100 = Best, 0 = Worst\nPercentile rank based on Points Per Shot"
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 30, 10),
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray30"),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "gray40")
  )

ggsave("lions Zone_PPS_Percentile_SemiCircular_Auto.png", width = 10, height = 6, dpi = 300, bg = "white")
