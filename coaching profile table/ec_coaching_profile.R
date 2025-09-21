setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/coaching profile tables")

Sys.setenv(CHROMOTE_CHROME = "D:/Program Files/Google/Chrome/Application/chrome.exe")

# Load packages
library(tidyverse)
library(paletteer)
library(gt)
library(gtExtras)
library(gtUtils)

# Theme
gt_theme_f5 <- function(gt_object, ...) {
  gt_object %>%
    opt_table_font(font = list(google_font("Roboto"), default_fonts()), weight = 400) %>%
    tab_style(locations = cells_title("title"), style = cell_text(font = google_font("Roboto"), weight = 700)) %>%
    tab_style(locations = cells_title("subtitle"), style = cell_text(font = google_font("Roboto"), color = "gray65", weight = 400)) %>%
    tab_style(
      style = list(cell_borders(sides = "top", color = "black", weight = px(0)),
                   cell_text(font = google_font("Roboto"), v_align = "bottom", size = px(11), weight = 'bold')),
      locations = list(gt::cells_column_labels(), gt::cells_stubhead())
    ) %>%
    tab_options(
      column_labels.background.color = "floralwhite",
      data_row.padding = px(7.5),
      heading.border.bottom.style = "none",
      table.border.top.style = "none", # transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "bold", 
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
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

# Suffix helper
append_suffix <- function(num){
  suff <- case_when(
    num %in% c(11,12,13) ~ "th",
    num %% 10 == 1 ~ "st",
    num %% 10 == 2 ~ "nd",
    num %% 10 == 3 ~ "rd",
    TRUE ~ "th"
  )
  paste0(num, suff)
}

# Palette
custom_palette <- as.character(paletteer::paletteer_d("Redmonder::dPBIRdGn"))[3:9]

# Load and wrangle data
df <- read_csv("ec_coach_ranks.csv") %>%
  mutate(
    coach_name = sub("\\s*\\(.*", "", coach_name),
    record = paste0(wins, "-", losses),
    record = paste0(record, " (", tolower(append_suffix(w_pct_rank)), ")"),
    team_abbreviation = paste0("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/coaching profile tables/ec_team_logos_svg/", team_abbreviation, ".png"),
    pace = round(pace, 1),
    net_rating = sprintf("%+3.1f", net_rating),
    across(ends_with("_pct"), ~ scales::percent(., .1)),
    across(ends_with("_rate"), ~ scales::percent(., .1)),
    across(matches("^pct_fga_(atb|cr)_3pt$"), ~ scales::percent(., .1))
  ) %>%
  select(-wins, -losses, -w_pct_rank) %>%
  relocate(record, .before = net_rating)

# Filter for Žakelj (16 metric columns only)
zakelj <- df %>%
  filter(str_detect(coach_name, "Žakelj")) %>%
  arrange(desc(season)) %>%
  select(
    season, record, team_abbreviation,
    net_rating_rank, off_rating_rank, def_rating_rank, pace_rank,
    efg_pct_rank, fta_rate_rank, tm_tov_pct_rank, oreb_pct_rank, pct_fga_atb_3pt_rank, pct_fga_cr_3pt_rank,
    opp_efg_pct_rank, opp_fta_rate_rank, opp_tov_pct_rank, opp_oreb_pct_rank, opp_pct_fga_atb_3pt_rank, opp_pct_fga_cr_3pt_rank,
    net_rating, off_rating, def_rating, pace,
    efg_pct, fta_rate, tm_tov_pct, oreb_pct, pct_fga_atb_3pt, pct_fga_cr_3pt,
    opp_efg_pct, opp_fta_rate, opp_tov_pct, opp_oreb_pct, opp_pct_fga_atb_3pt, opp_pct_fga_cr_3pt
  )

# Custom column label
szn_rec_col <- '<link href="https://cdnjs.cloudflare.com/ajax/libs/roboto-fontface/0.10.0/css/roboto/roboto-fontface.css" rel="stylesheet">
<div style="display: flex; flex-direction: column; align-items: center;">
  <span style="font-weight: bold; font-size: 14px; color: black;">Season</span>
  <span style="font-size: 8px; color: #BEBEBE;">Record</span>
</div>'


# Final table
zakelj %>%
  gt() %>%
  tab_header(
    title = "Key Rankings of Andrej Žakelj Coached Teams (EuroCup)",
    subtitle = "As of October 1, 2025"
  ) %>%
  cols_label(
    team_abbreviation = "",
    season ~ md(szn_rec_col),
    net_rating_rank = "NRtg",
    off_rating_rank = "ORtg",
    def_rating_rank = "DRtg",
    pace_rank = "Pace",
    efg_pct_rank = "eFG%",
    fta_rate_rank = "FTr%",
    tm_tov_pct_rank = "TOV%",
    oreb_pct_rank = "ORB%",
    pct_fga_atb_3pt_rank = "3PAr (ATB)",
    pct_fga_cr_3pt_rank = "3PAr (Corner)",
    opp_efg_pct_rank = "eFG%",
    opp_fta_rate_rank = "FTr%",
    opp_tov_pct_rank = "TOV%",
    opp_oreb_pct_rank = "DRB%",
    opp_pct_fga_atb_3pt_rank = "3PAr (ATB)",
    opp_pct_fga_cr_3pt_rank = "3PAr (Corner)"
  ) %>%
  tab_spanner(label = "Advanced", columns = net_rating_rank:pace_rank) %>%
  tab_spanner(label = "Offense", columns = efg_pct_rank:pct_fga_cr_3pt_rank) %>%
  tab_spanner(label = "Defense", columns = opp_efg_pct_rank:opp_pct_fga_cr_3pt_rank) %>%
  tab_style(style = cell_borders(sides = "left", weight = px(2)),
            locations = cells_body(columns = c(efg_pct_rank, opp_efg_pct_rank))) %>%
  tab_style(style = cell_borders(sides = "left", color = "black", weight = px(2)),
            locations = cells_column_spanners(spanners = c("Offense", "Defense"))) %>%
  tab_style(style = cell_borders(sides = "left", color = "black", weight = px(2)),
            locations = cells_column_labels(columns = c(efg_pct_rank, opp_efg_pct_rank))) %>%
  # Merge Season + Record
  gt_merge_stack(col1 = season, col2 = record,
                 font_weight = c("bold", "normal"),
                 font_size = c("14px", "8px")) %>%
  cols_align(
    align = "center",
    columns = season
  ) %>%
  # Merge and display stat rank + value
  gt_merge_stack(col1 = net_rating_rank, col2 = net_rating, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = off_rating_rank, col2 = off_rating, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = def_rating_rank, col2 = def_rating, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = pace_rank, col2 = pace, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = efg_pct_rank, col2 = efg_pct, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = fta_rate_rank, col2 = fta_rate, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = tm_tov_pct_rank, col2 = tm_tov_pct, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = oreb_pct_rank, col2 = oreb_pct, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = pct_fga_atb_3pt_rank, col2 = pct_fga_atb_3pt, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = pct_fga_cr_3pt_rank, col2 = pct_fga_cr_3pt, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = opp_efg_pct_rank, col2 = opp_efg_pct, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = opp_fta_rate_rank, col2 = opp_fta_rate, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = opp_tov_pct_rank, col2 = opp_tov_pct, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = opp_oreb_pct_rank, col2 = opp_oreb_pct, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = opp_pct_fga_atb_3pt_rank, col2 = opp_pct_fga_atb_3pt, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  gt_merge_stack(col1 = opp_pct_fga_cr_3pt_rank, col2 = opp_pct_fga_cr_3pt, small_cap = F, palette = c("black", "black"), font_weight = c("normal", "normal"), font_size = c("13px", "7px")) %>%
  # Logos
  gt_img_rows(team_abbreviation, img_source = "local") %>%
  # Lowercase rank suffixes
  fmt(columns = ends_with("_rank"), fns = function(x) tolower(append_suffix(x))) %>%
  # Color raw rank columns
  gt_color_rows(columns = ends_with("_rank"), palette = custom_palette, direction = -1, domain = c(1, 20)) %>%
  # Column widths
  cols_width(team_abbreviation ~ px(40),
             season ~ px(70),
             everything() ~ px(50)) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  gt_theme_f5() %>%
  tab_options(
    table_body.border.bottom.width = '1px',
    table_body.border.bottom.color = "gray90",
    data_row.padding = '1px',
    stub.background.color = 'floralwhite'
  ) %>%
  tab_source_note(
    md('<div style="text-align: right; width: 100%;">
         <span style="color:#b4b4b4;"><b>Table:</b></span>
         <span style="color:#b4b4b4;"> Can Sahin</span>
       </div>')
  ) %>%
  gt_save_crop(file = "Zakelj.png", whitespace = 20, bg = "floralwhite")
