# Set working directory to where your data is located
setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/2 man lineup net rtg")

# Load required packages
library(tidyverse)
library(readr)
library(janitor)
library(gt)
library(gtExtras)
library(gtUtils)
library(euroleaguer)

# =======================
# 1. Load & Prepare Data
# =======================

# Load your EuroCup double-big 2-man lineup data (already cleaned & filtered)
df <- read_csv("2man.csv") %>%
  clean_names()  # Ensures consistent snake_case column names

# Expected column names: player1, player2, team_abbreviation, min, net_rtg, off_rtg, def_rtg

# =====================================
# 2. Get Team Logos via euroleaguer API
# =====================================

# Get unique team codes from your dataset
eurocup_teams <- unique(df$team_abbreviation)

# Pull logos from euroleaguer
team_logos <- map_dfr(eurocup_teams, function(team_code) {
  tryCatch({
    getTeam(team_code = team_code, season_code = "U2024") %>%
      select(team_code = TeamCode, logo_url = ImagesCrest)
  }, error = function(e) {
    tibble(team_code = team_code, logo_url = NA)
  })
})

# Join logo URLs into your main dataset
df <- df %>%
  left_join(team_logos, by = c("team_abbreviation" = "team_code"))

# ==========================
# 3. Define Custom Theme (F5)
# ==========================

gt_theme_f5 <- function(gt_object, ...) {
  gt_object %>%
    opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()
      ),
      weight = 400
    ) %>%
    tab_style(
      locations = cells_title("title"),
      style = cell_text(
        font = google_font("Roboto"),
        weight = 700
      )
    ) %>%
    tab_style(
      locations = cells_title("subtitle"),
      style = cell_text(
        font = google_font("Roboto"),
        color = "gray65",
        weight = 400
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "top", color = "black", weight = px(0)
        ),
        cell_text(
          font = google_font("Roboto"),
          v_align = "bottom",
          size = px(14),
          weight = 'bold'
        )
      ),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_stubhead()
      )
    ) %>%
    tab_options(
      column_labels.background.color = "floralwhite",
      data_row.padding = px(7.5),
      heading.border.bottom.style = "none",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      column_labels.font.weight = "bold", 
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "floralwhite",
      stub.border.color = "floralwhite",
      stub.border.width = px(0),
      source_notes.font.size = 12,
      source_notes.border.lr.style = "none",
      table.font.size = 16,
      heading.align = "left",
      table.background.color = "floralwhite",
      table_body.hlines.color = 'gray90',
      ...
    )
}

# Flip names: "Surname Initial." → "Initial. Surname"
flip_name <- function(name) {
  suffixes <- c("Jr.", "Sr.", "II", "III", "IV", "V")
  parts <- str_split(name, " ", simplify = TRUE)
  n <- ncol(parts)
  
  if (n == 3 && parts[2] %in% suffixes) {
    # Format: Surname Suffix Initial → Initial Surname Suffix
    initial <- str_remove(parts[3], "\\.")  # remove dot if exists
    return(paste0(initial, ". ", parts[1], " ", parts[2]))
  } else if (n == 2) {
    # Format: Surname Initial → Initial Surname
    initial <- str_remove(parts[2], "\\.")
    return(paste0(initial, ". ", parts[1]))
  } else {
    return(name)  # If unexpected format, keep original
  }
}


df <- df %>%
  mutate(
    player1 = map_chr(player1, flip_name),
    player2 = map_chr(player2, flip_name)
  )

# ================================
# 4. Build & Save the GT Table
# ================================

df %>%
  mutate(team_abbreviation = logo_url) %>%
  arrange(desc(net_rtg)) %>%  # Sort by Net Rating DESC
  gt() %>%
  gt_theme_f5() %>%
  gt_img_rows(columns = team_abbreviation, height = 30, img_source = "web") %>%
  cols_hide(columns = c(logo_url)) %>%
  gt_merge_stack(
    col1 = player1,
    col2 = player2,
    font_size = c("14px", "14px"),
    font_weight = c("bold", "normal"),
    small_cap = FALSE,
    palette = c("black", "gray")
  ) %>%
  tab_header(
    title = "Doubling Up – EuroCup Regular Season 2024-25",
    subtitle = "Net Rating of commonly used double big looks (Min 50 MP)"
  ) %>%
  cols_label(
    team_abbreviation = "", 
    player1 = "Big Combo",
    min = "Min",
    off_rtg = md("Off.<br>Rating"),
    def_rtg = md("Def.<br>Rating"),
    net_rtg = md("Net<br>Rating")
  ) %>%
  cols_move_to_start(columns = team_abbreviation) %>%
  cols_move_to_end(columns = net_rtg) %>%
  fmt_number(columns = net_rtg, force_sign = TRUE, decimals = 1) %>%
  data_color(
    columns = net_rtg,
    domain = c(-25, 30),
    palette = "Redmonder::dPBIRdGy",
    alpha = 0.75
  ) %>%
  tab_options(
    data_row.padding = px(0)
  ) %>%
  tab_source_note(
    source_note = md("<div style='text-align:right; color:gray;'>Table: Can Sahin</div>")
  ) %>%
  gt_save_crop(file = "eurocup_double_big.png", bg = "floralwhite")





