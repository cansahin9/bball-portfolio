# ───────────────────────────────────────────────
# 📁 SETUP
# ───────────────────────────────────────────────
setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/reports/post game report")

library(euroleaguer)
library(dplyr)
library(tidyr)
library(gt)
library(scales)
library(gtExtras)
library(purrr)

# ───────────────────────────────────────────────
# 📊 STEP 1: Get Game Boxscore
# ───────────────────────────────────────────────
game_data <- getGameBoxScore(season_code = "U2025", game_code = 36)

# London Lions (TeamCode = "LLI")
team_stats <- game_data$TeamStats %>%
  filter(TeamCode == "NIN") %>%
  mutate(
    `2P%` = round(`2P%`, 1),
    `3P%` = round(`3P%`, 1),
    `FT%` = round(`FT%`, 1)
  ) %>%
  select(
    PTS, `2PM`, `2PA`, `2P%`, 
    `3PM`, `3PA`, `3P%`, 
    FTM, FTA, `FT%`, 
    AST, TO, OREB, DREB, STL, BLK
  )

# ───────────────────────────────────────────────
# 📈 STEP 2: Get Season Averages for Team
# ───────────────────────────────────────────────
season_avg <- getTeamStats(
  season_code = "U2025",
  team_code = "NIN",
  phase_type = "RS"
)$TeamAveragePerGame %>%
  select(
    PTS, `2PM`, `2PA`, `2P%`,
    `3PM`, `3PA`, `3P%`,
    FTM, FTA, `FT%`,
    AST, TO, OREB, DREB, STL, BLK
  )


# ───────────────────────────────────────────────
# 🧮 STEP 3: Create Comparison (Game vs Season Average)
# ───────────────────────────────────────────────
comparison_df <- team_stats %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Value") %>%
  left_join(
    season_avg %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Average"),
    by = "Metric"
  ) %>%
  mutate(
    Diff = Value - Average,
    Direction = case_when(
      is.na(Average) ~ NA_character_,
      Diff > 0 ~ "above",
      Diff < 0 ~ "below",
      TRUE ~ "even"
    )
  )

# 🧮 STEP 3.5: Reshape Comparison to Wide Format (for coloring)
team_wide <- comparison_df %>%
  select(Metric, Value, Diff) %>%
  pivot_wider(
    names_from = Metric,
    values_from = c(Value, Diff),
    names_glue = "{.value}_{Metric}"
  )

# ───────────────────────────────────────────────
# 🎨 STEP 4: GT THEME (f5 style)
# ───────────────────────────────────────────────
gt_theme_f5 <- function(gt_object, ...) {
  gt_object %>%
    opt_table_font(
      font = list(google_font("Roboto"), default_fonts()),
      weight = 400
    ) %>%
    tab_style(
      locations = cells_title("title"),
      style = cell_text(font = google_font("Roboto"), weight = 700)
    ) %>%
    tab_style(
      locations = cells_title("subtitle"),
      style = cell_text(font = google_font("Roboto"), color = "gray35", weight = 400)
    ) %>%
    tab_style(
      style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = list(
        cell_text(font = google_font("Roboto"), v_align = "bottom", size = px(12), weight = 'bold', color = "gray90"),
        cell_fill(color = "#DC7A2F"),
        cell_borders(sides = c("top", "bottom"), color = "black", weight = px(1))
      ),
      locations = gt::cells_column_labels()
    ) %>%
    tab_options(
      column_labels.background.color = "#DC7A2F",
      heading.border.bottom.style = "none",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      column_labels.font.weight = "bold",
      table.font.size = 12,
      heading.align = "left",
      table.background.color = "white",
      table_body.hlines.color = 'gray85',
      data_row.padding = px(6),
      ...
    )
}

value_cols <- names(team_wide)[grepl("^Value_", names(team_wide))]

# ───────────────────────────────────────────────
# 🧾 STEP 5: Build Final Table
# ───────────────────────────────────────────────
# Build final gt table directly from raw team_wide
team_table <- team_wide %>%
  select(all_of(value_cols)) %>%
  gt() %>%
  gt_theme_f5() %>%
  tab_header(title = md("**Team Boxscore**")) %>%
  fmt_number(
    columns = c("Value_2P%", "Value_3P%", "Value_FT%"),
    decimals = 1,
    pattern = "{x}%"
  ) %>%
  cols_label(
    `Value_PTS` = "PTS", 
    `Value_2PM` = "2P", `Value_2PA` = "2PA", `Value_2P%` = "2P%",
    `Value_3PM` = "3P", `Value_3PA` = "3PA", `Value_3P%` = "3P%",
    `Value_FTM` = "FT", `Value_FTA` = "FTA", `Value_FT%` = "FT%",
    `Value_AST` = "AST", `Value_TO` = "TO", `Value_OREB` = "ORB",
    `Value_DREB` = "DRB", `Value_STL` = "STL", `Value_BLK` = "BLK"
  ) %>%
  tab_spanner(
    label = md("**Shooting**"),
    columns = c("Value_2PM", "Value_2PA", "Value_2P%", 
                "Value_3PM", "Value_3PA", "Value_3P%", 
                "Value_FTM", "Value_FTA", "Value_FT%")
  ) %>%
  tab_spanner(
    label = md("**Impact**"),
    columns = c("Value_AST", "Value_TO", "Value_OREB", 
                "Value_DREB", "Value_STL", "Value_BLK")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(everything() ~ px(50)) %>%
  tab_source_note(
    md('<div style="text-align:right; color:gray; font-size:11px;"><em>Color shading indicates performance relative to team’s season average</em></div>')
  )


metrics_to_color <- c("2P%", "3P%", "FT%", "AST", "TO", "OREB", "DREB", "STL", "BLK")

for (metric in metrics_to_color) {
  value_col <- paste0("Value_", metric)
  diff_col  <- paste0("Diff_", metric)
  
  if (!all(c(value_col, diff_col) %in% names(team_wide))) next
  
  diff_val <- team_wide[[diff_col]]
  value_val <- team_wide[[value_col]]
  
  if (is.na(diff_val) || is.na(value_val)) next
  
  # Flip sign for "bad-when-high" metrics
  if (metric %in% c("TO", "FGA", "Fouls", "MissedFT")) {
    diff_val <- -diff_val
  }
  
  # Fixed symmetric scale around 0
  domain <- c(-20, 0, 20)
  
  # Define color function
  color_fn <- col_numeric(
    palette = c("#e63946", "#f0f0f0", "#52b788"), # red-gray-green
    domain = domain,
    na.color = "#f1faee"
  )
  
  # Apply coloring
  team_table <- team_table %>%
    tab_style(
      style = cell_fill(color = color_fn(diff_val)),
      locations = cells_body(columns = all_of(value_col))
    )
  
  team_table <- team_table %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body()
    )
}

# ───────────────────────────────────────────────
# 💾 SAVE OUTPUT
# ───────────────────────────────────────────────
gtsave(team_table, "niners_Boxscore.png", expand = 10)
