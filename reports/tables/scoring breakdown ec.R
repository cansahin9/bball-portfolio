# ───────────────────────────────────────────────
# ⚙️ SETUP
# ───────────────────────────────────────────────
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
# 🎯 Step 2: Extract Scoring Breakdown Data
# ───────────────────────────────────────────────

# Clean USG% by parsing and dividing by 100 for percent format
# Clean values
usg_clean <- readr::parse_number(df$`USG%`) / 100
ppg <- df$PTS
ppp <- df$PPP

# Build table with cleaned values
scoring_data <- tibble::tibble(
  Metric = c("USG%", "PPG", "PPP"),
  Value = c(usg_clean, ppg, ppp),
  PCTL1 = c(df$`PERCENTILE...19`, df$`PERCENTILE...21`, df$`PERCENTILE...23`)
)

colnames(scoring_data)[3] <- "PCTL¹"



# ───────────────────────────────────────────────
# 🎨 Step 3: Custom GT Theme (Reusable)
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
        cell_borders(sides = c("top", "bottom"), color = "gray35", weight = px(1))
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

scoring_table_gt <- scoring_data %>%
  gt() %>%
  gt_theme_f5() %>%
  tab_header(title = md("**Scoring Breakdown (2024–25)**")) %>%
  
  # Correctly formatted!
  fmt_percent(columns = Value, rows = Metric == "USG%", decimals = 1) %>%
  fmt_number(columns = Value, rows = Metric == "PPG", decimals = 1) %>%
  fmt_number(columns = Value, rows = Metric == "PPP", decimals = 2) %>%
  
  data_color(
    columns = `PCTL¹`,
    colors = col_numeric(
      palette = c("#e63946", "#f1a85b", "#f1faee", "#90be6d", "#52b788"),
      domain = c(0, 100)
    )
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_source_note(
    source_note = md("*¹ Percentile ranks compared to all qualifying guards — 100 = Best, 0 = Worst*")
  )


# ───────────────────────────────────────────────
# 💾 Step 5: Save Table (Optional)
# ───────────────────────────────────────────────
gt::gtsave(scoring_table_gt, "scoring_breakdown_compact.png")

