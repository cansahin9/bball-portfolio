setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Eurocup/25-26/vs Besiktas 07.10.2025")

# Load required libraries
library(gt)
library(dplyr)
library(readr)

# Read the data
df <- read_csv("player stats.csv")

# Detect whether % columns are decimals or already percentages
is_decimal <- max(df$`FG%`, na.rm = TRUE) <= 1

# Build per-game stats
per_game_stats <- df %>%
  transmute(
    Season,
    G = as.integer(GP),  # integer
    MP = round(MIN, 1),
    PTS = round(PTS, 1),
    FG = round(FGM, 1),
    FGA = round(FGA, 1),
    `FG%` = round(`FG%` * if_else(is_decimal, 100, 1), 1),
    `3P` = round(`3PM`, 1),
    `3PA` = round(`3PA`, 1),
    `3P%` = round(`3P%` * if_else(is_decimal, 100, 1), 1),
    FT = round(FTM, 1),
    FTA = round(FTA, 1),
    `FT%` = round(`FT%` * if_else(is_decimal, 100, 1), 1),
    AST = round(AST, 1),
    TOV = round(TOV, 1),
    ORB = round(OFF, 1),
    DRB = round(DEF, 1),
    STL = round(STL, 1),
    BLK = round(BLK, 1)
  )

# Build styled gt table
gt_table <- per_game_stats %>%
  gt() %>%
  tab_header(
    title = "Per Game Stats by Season"
  ) %>%
  # ✅ Keep integers for G only
  fmt_number(
    columns = G,
    decimals = 0
  ) %>%
  # ✅ Keep one decimal for the rest
  fmt_number(
    columns = c(MP, PTS, FG, FGA, `3P`, `3PA`, FT, FTA, AST, TOV, ORB, DRB, STL, BLK),
    decimals = 1
  ) %>%
  # ✅ Format % columns with % sign
  fmt_number(
    columns = c(`FG%`, `3P%`, `FT%`),
    decimals = 1,
    pattern = "{x}%"
  ) %>%
  cols_label(
    `3P` = "3P",
    `3PA` = "3PA",
    `3P%` = "3P%",
    `FG%` = "FG%",
    `FT%` = "FT%"
  ) %>%
  # 🔥 Black header styling
  tab_style(
    style = list(
      cell_fill(color = "black"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.names = "Calibri",
    table.font.size = 12,
    heading.title.font.size = 12,
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    data_row.padding = px(2)
  )

# ✅ Save final table
gtsave(gt_table, "per_game_stats_by_season.png")
