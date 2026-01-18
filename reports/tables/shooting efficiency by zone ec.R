
# Load required libraries
library(gt)
library(dplyr)
library(readr)
library(scales)

# Step 1: Load CSV
file_path <- "Tarik PHILLIP.csv"
df <- read_csv(file_path)

# Step 2: Extract shooting zones
zone_data <- tibble::tibble(
  Zone = c("Rim", "Paint", "Mid", "Corner 3", "Above 3"),
  `Freq %` = df %>%
    select(`RIM FREQ`, `PAINT FREQ`, `MID FREQ`, `C3 FREQ`, `L3 FREQ`) %>%
    unlist() %>%
    as.character() %>%
    readr::parse_number(),
  `PPS` = as.numeric(df %>% select(`RIM PPS`, `PAINT PPS`, `MID PPS`, `C3 PPS`, `L3 PPS`) %>% unlist()),
  `F %ile` = as.numeric(df %>% select(`PERCENTILE...63`, `PERCENTILE...65`, `PERCENTILE...67`, `PERCENTILE...69`, `PERCENTILE...71`) %>% unlist()),
  `P %ile` = as.numeric(df %>% select(`PERCENTILE...53`, `PERCENTILE...55`, `PERCENTILE...57`, `PERCENTILE...59`, `PERCENTILE...61`) %>% unlist())
)

zone_data <- zone_data %>%
  rename(
    `PCTL¹` = `F %ile`,
    `PCTL²` = `P %ile`
  )


# Add footer as an extra row
zone_data_with_footer <- bind_rows(
  zone_data,
  tibble(
    Zone = "*Percentile ranks compared to all qualifying guards — 100 = Best, 0 = Worst*",
    `Freq %` = NA_real_,
    `PCTL¹` = NA_real_,
    `PPS` = NA_real_,
    `PCTL²` = NA_real_
  )
)


# Step 3: Custom GT Theme
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

zone_table_gt <- zone_data %>%
  gt() %>%
  gt_theme_f5() %>%
  tab_header(title = md("**Shooting Efficiency by Zone (2024–25)**")) %>%
  
  tab_spanner(label = "Volume", columns = c(`Freq %`, `PCTL¹`)) %>%
  tab_spanner(label = "Efficiency", columns = c(`PPS`, `PCTL²`)) %>%
  
  # Format numbers
  fmt_number(columns = `PPS`, decimals = 2) %>%
  fmt_number(columns = `Freq %`, decimals = 1, suffixing = FALSE) %>%
  
  data_color(
    columns = `PCTL²`,
    colors = col_numeric(
      palette = c("#e63946", "#f1a85b", "#f1faee", "#90be6d", "#52b788"),
      domain = c(0, 100)
    )
  ) %>%
  data_color(
    columns = `PCTL¹`,
    colors = col_numeric(
      palette = c("#e63946", "#f1a85b", "#f1faee", "#90be6d", "#52b788"),
      domain = c(0, 100)
    )
  ) %>%
  
  # Source note instead of final row
  tab_source_note(
    source_note = md("*¹ Percentile ranks of shot frequency compared to all qualifying guards (0–100)*<br>*² Percentile ranks of points per shot compared to all qualifying guards (0–100)*")
  )%>%
  
  # Center align all
  cols_align(align = "center", columns = everything())


# Save table
gt::gtsave(zone_table_gt, "shooting_efficiency_zone_compact.png")

