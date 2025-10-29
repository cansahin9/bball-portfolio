# ───────────────────────────────────────────────
# 📁 SETUP
# ───────────────────────────────────────────────
setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/reports/post game report")

library(euroleaguer)
library(dplyr)
library(gt)
library(scales)
library(stringr)
library(glue)
library(tidyr)  

# ───────────────────────────────────────────────
# 🎨 REUSABLE THEME FUNCTION (for padding + style)
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
      style = list(
        cell_text(font = google_font("Roboto"), v_align = "bottom", size = px(12), weight = 'bold', color = "black"),  # 👈 add color = "black"
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

# Load game data
game_data <- getGameBoxScore(season_code = "U2025", game_code = 36)

# Process player stats
player_df <- game_data$PlayerStats %>%
  filter(TeamCode == "NIN") %>%
  mutate(
    MP = floor(Seconds / 60),
    Group = ifelse(IsStarter == 1, "Starters", "Bench"),
    FGA = `2PA` + `3PA`,
    AdjFGA = FGA + 0.44 * FTA,
    PPS = if_else(AdjFGA == 0, NA_real_, PTS / AdjFGA),
    TS = if_else(AdjFGA == 0, NA_real_, PTS / (2 * AdjFGA))
  ) %>%
  select(
    Group, Player, MP, PTS,
    `2PM`, `2PA`, `2P%`,
    `3PM`, `3PA`, `3P%`,
    FTM, FTA, `FT%`,
    PPS, TS
  ) %>%
  arrange(desc(Group), desc(MP))

player_df <- player_df %>%
  mutate(
    Number = str_extract(Player, "#\\d+"),
    Player = str_trim(str_to_title(gsub("#\\d+", "", Player))),
    Player = str_trim(glue("{Player} {Number}"))
  ) %>%
  select(-Number)  # 👈 THIS removes the extra column

player_df <- player_df %>%
  mutate(
    `2P%` = replace_na(`2P%`, 0),
    `3P%` = replace_na(`3P%`, 0),
    `FT%` = replace_na(`FT%`, 0),
    TS = replace_na(TS, 0)  # For TS%
  )

# Build GT table
player_table <- player_df %>%
  gt(groupname_col = "Group") %>%
  gt_theme_f5() %>%
  tab_header(title = md("**Player Boxscore**")) %>%
  cols_label(
    MP = "MP",
    PTS = "PTS",
    `2PM` = "2P", `2PA` = "2PA", `2P%` = "2P%",
    `3PM` = "3P", `3PA` = "3PA", `3P%` = "3P%",
    FTM = "FT", FTA = "FTA", `FT%` = "FT%",
    PPS = "PPS", TS = "TS%"
  ) %>%
  tab_spanner(
    label = "Shooting",
    columns = c(`2PM`, `2PA`, `2P%`, `3PM`, `3PA`, `3P%`, FTM, FTA, `FT%`)
  ) %>%
  tab_spanner(
    label = "Efficiency",
    columns = c(PPS, TS)
  ) %>%
  data_color(
    columns = c(MP),
    colors = col_numeric(
      palette = c("#f0f0f0", "#B34735"),
      domain = c(0, 40)
    )
  ) %>%
  data_color(
    columns = c(PTS),
    colors = col_numeric(
      palette = c("#f0f0f0", "#52b788"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(TS),
    colors = col_numeric(
      palette = c("#f0f0f0", "#52b788"),
      domain = NULL
    )
  ) %>%
  fmt_number(
    columns = c(`2P%`, `3P%`, `FT%`),
    decimals = 1, pattern = "{x}%"
  ) %>%
  # Format TS% as percentage (multiply by 100 and add %)
  fmt_percent(
    columns = TS,
    decimals = 1
  ) %>%
  # Keep PPS as 2-decimal float
  fmt_number(
    columns = PPS,
    decimals = 2
  )%>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left",
    data_row.padding = px(6),        # ← key change
    row_group.padding = px(2),
    column_labels.padding = px(2),
    table_body.hlines.color = "gray90"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  cols_width(
    Player ~ px(180),
    everything() ~ px(50)
  ) %>%
  cols_align(
    align = "center",
    columns = -Player     # ← aligns all but Player to center
  )

# Add group row styling (gray background for "Starters" and "Bench")
player_table <- player_table %>%
  tab_style(
    style = list(
      cell_fill(color = "gray90"),
      cell_text(color = "black"),
      cell_borders(sides = c("top", "bottom"), color = "black", weight = px(2)),
      cell_text(weight = "bold", size = px(12))
    ),
    locations = cells_row_groups()
  )

# Add black background to column headers and spanners
player_table <- player_table %>%
  tab_style(
    style = list(
      cell_fill(color = "#DC7A2F"),
      cell_text(color = "white", weight = "bold", size = px(12))
    ),
    locations = list(
      cells_column_labels(),
      cells_column_spanners()
    )
  ) 

# Force black text inside colored cells
player_table <- player_table %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body(columns = c(MP, PTS, PPS, TS))
  )

# Export to PNG
gtsave(player_table, "niners_Player_Efficiency_Boxscore.png", expand = 10)
