setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/reports/tables")

# Load required libraries
library(gt)
library(dplyr)
library(readr)
library(scales)

# ───────────────────────────────────────────────
# 🔧 Step 1: Load CSV
# ───────────────────────────────────────────────
file_path <- "C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/reports/tables/Tarik PHILLIP.csv"
df <- read_csv(file_path)

# ───────────────────────────────────────────────
# 🎨 Step 3: Custom GT Theme
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
      style = list(
        cell_text(
          font = google_font("Roboto"),
          v_align = "bottom",
          size = px(12),
          weight = 'bold',
          color = "white"
        ),
        cell_fill(color = "black"),
        cell_borders(sides = c("top", "bottom"), color = "black", weight = px(1))
      ),
      locations = gt::cells_column_labels()
    ) %>%
    tab_options(
      column_labels.background.color = "black",
      heading.border.bottom.style = "none",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      column_labels.font.weight = "bold",
      table.font.size = 11,
      heading.align = "left",
      table.background.color = "white",
      table_body.hlines.color = 'gray85',
      data_row.padding = px(2),
      ...
    )
}

combined_data <- tibble::tibble(
  Category = c(
    rep("Playmaking Profile", 2),
    rep("Scoring Efficiency", 5),
    rep("Defensive Impact", 4),
    rep("Impact Summary", 4)
  ),
  Metric = c(
    "Assist Rate", "Turnover Rate",                           # Playmaking (2)
    "True Shooting Rate", "Effective FG Rate", "Free Throw Rate", "Points Per Possession", "Usage Rate",      # Scoring (5)
    "Steal Rate", "Block Rate", "Defensive Rating", "Defensive Rebounding Rate", # Defense (5)
    "Player Efficency Rating", "Win Share", "Box Plus-Minus", "Value Over Replacement Player"                # Impact (4)
  ),
  Value = df %>% select(
    `AST%`, `TOV%`,
    `TS%`, `EFG%`, `FT%`, `PPP`, `USG%`,
    `STL%`, `BLK%`, `IND DEF RTG`, `DRB%`,
    PER, WS, BPM, VORP
  ) %>%
    unlist() %>%
    parse_number(),
  
  PCTL = c(
    pull(df, `PERCENTILE...85`), pull(df, `PERCENTILE...77`),  # AST%, TOV%
    pull(df, `PERCENTILE...51`), pull(df, `PERCENTILE...43`), pull(df, `PERCENTILE...73`), pull(df, `PERCENTILE...23`), pull(df, `PERCENTILE...19`),  # TS%, EFG%, FT%, PPP, USG%
    pull(df, `PERCENTILE...89`), pull(df, `PERCENTILE...93`), pull(df, `PERCENTILE...13`), pull(df, `PERCENTILE...103`),  # STL%, BLK%, DEF RTG, DRB%
    pull(df, `PERCENTILE...105`), pull(df, `PERCENTILE...107`), pull(df, `PERCENTILE...111`), pull(df, `PERCENTILE...113`)  # PER, WS, BPM, VORP
  )
)

# Rename for gt formatting
combined_data <- combined_data %>%
  rename(`PCTL¹` = PCTL)

combined_table <- combined_data %>%
  gt(groupname_col = "Category") %>%
  gt_theme_f5() %>%
  tab_header(title = md("**Performance Profile (2024–25)**")) %>%
  
  # Format percentage values with % symbol
  fmt_number(
    columns = Value,
    rows = Metric %in% c(
      "Assist Rate", "Turnover Rate", "True Shooting Rate", "Effective FG Rate", 
      "Free Throw Rate", "Usage Rate", "Steal Rate", "Block Rate", 
      "Defensive Rebounding Rate"
    ),
    decimals = 1,
    pattern = "{x}%"
  ) %>%
  
  # Format PPP (or any non-% metric) with 2 decimals
  fmt_number(
    columns = Value,
    rows = Metric == "Points Per Possession",
    decimals = 2
  ) %>%
  
  data_color(
    columns = `PCTL¹`,
    colors = col_numeric(
      palette = c("#e63946", "#f1a85b", "#f1faee", "#90be6d", "#52b788"),
      domain = c(0, 100)
    )
  ) %>%
  # 🟩 This line below ensures correct label for PCTL¹ column
  cols_label(`PCTL¹` = md("**PCTL¹**")) %>%
  # only category left aligned
  cols_align(align = "left", columns = Category) %>%
  cols_align(align = "center", columns = c(Value, `PCTL¹`)) %>%
  
  # 🆕 Style: Gray background & bold for sub-category titles
  tab_style(
    style = list(
      cell_fill(color = "gray95"),
      cell_text(weight = "bold", size = px(12)),
      cell_borders(sides = "top", color = "black", weight = px(2)),
      cell_borders(sides = "bottom", color = "black", weight = px(2))
    ),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    row_group.padding = px(2)  # Reduce padding height of section headers
  )%>%
  
  tab_source_note(md("*¹ Percentile ranks compared to all qualifying guards — 100 = Best, 0 = Worst*"))

# Save the table as a PNG file
gtsave(combined_table, "Tarik PHILLIP Advanced Profile.png")

