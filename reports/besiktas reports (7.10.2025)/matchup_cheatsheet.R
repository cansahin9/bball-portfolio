
# =========================================
# EuroCup Matchup Tables (Owen-style theme)
# =========================================

library(tidyverse)
library(janitor)
library(gt)
library(gtExtras)
library(euroleaguer)
library(purrr)
library(dplyr)
library(stringr)

# ---- Load Data ----
team <- read_csv("ec_team_matchup_data_25_26.csv") %>% clean_names()
opp  <- read_csv("ec_opponent_matchup_data_25_26.csv") %>% clean_names()

# ---- Fix %/freq/pps columns ----
team <- team %>%
  mutate(across(where(is.character) & matches("percent$|_pps$|_freq$|ast_percent"),
                ~ readr::parse_number(.)))

opp <- opp %>%
  mutate(across(where(is.character) & matches("percent$|_pps$|_freq$|ast_percent"),
                ~ readr::parse_number(.)))


# ---- Standardize ----
team <- team %>% rename(team_abb = team_abb, team_name = team)
opp  <- opp  %>% rename(team_abb = team_abb, team_name = team)

# ---- Join offense + defense ----
df <- team %>%
  left_join(opp %>% select(-team_name), by = "team_abb")

logo_df <- map_dfr(unique(df$team_abb), function(code) {
  message("Fetching: ", code)
  res <- euroleaguer::getTeam(team_code = code, season_code = "U2025")
  if (is.null(res)) {
    tibble(team_abb = code, TeamName = NA, ImagesCrest = NA)
  } else {
    res %>%
      select(team_abb = TeamCode, TeamName, ImagesCrest)
  }
})

df <- df %>%
  left_join(logo_df, by = "team_abb") %>%
  mutate(
    team_display = coalesce(TeamName, team_name),
    logo = ImagesCrest
  )

# ---- Choose Teams ----
TM1 <- "LLI"   # Offense
TM2 <- "BES"   # Defense

# ---- KPI Function ----
kpi <- function(code) {
  df %>% filter(team_abb == code) %>%
    transmute(
      wl  = paste0(w.x, "-", l.x),
      net = sprintf("%+.1f", net_rtg.x)
    ) %>% slice(1)
}
k1 <- kpi(TM1); k2 <- kpi(TM2)

# ---- Title + subtitle ----
gt_title <- gt::md(
  paste0(
    "<img src='", df$logo[df$team_abb==TM1][1], "' style='height:28px;'> ",
    df$team_display[df$team_abb==TM1][1],
    " vs. ",
    df$team_display[df$team_abb==TM2][1],
    " <img src='", df$logo[df$team_abb==TM2][1], "' style='height:28px;'>"
  )
)
gt_subtitle <- gt::md(
  paste0(k1$wl, " (", k1$net, " Net) · ", k2$wl, " (", k2$net, " Net)")
)

row_map_off <- tribble(
  ~name, ~stat, ~group,
  
  # Advanced
  "Offensive Rating", "off_rtg.x", "Advanced",
  "eFG%",             "efg_percent.x", "Advanced",
  "FT Rate",          "ft_freq.x", "Advanced",
  "TOV Rate",         "tov_percent.x", "Advanced",
  "OREB Rate",        "orb_percent.x", "Advanced",
  
  # Shooting Efficiency
  "Rim PPS",          "rim_pps.x", "Shooting Efficiency",
  "Paint PPS",        "paint_pps.x", "Shooting Efficiency",
  "Mid PPS",          "mid_pps.x", "Shooting Efficiency",
  "Corner 3 PPS",     "c3_pps.x", "Shooting Efficiency",
  "Above Break 3 PPS","l3_pps.x", "Shooting Efficiency",
  "3P%",              "x3p_percent.x", "Shooting Efficiency",
  "FT%",              "ft_percent.x", "Shooting Efficiency",
  
  # Shooting Frequency
  "Rim Rate",         "rim_freq.x", "Shooting Frequency",
  "Paint Rate",       "paint_freq.x", "Shooting Frequency",
  "Mid Rate",         "mid_freq.x", "Shooting Frequency",
  "Corner 3 Rate",    "c3_freq.x", "Shooting Frequency",
  "Above Break Rate", "l3_freq.x", "Shooting Frequency",
  "FT Rate",          "ft_freq.x", "Shooting Frequency",
  
  # Playmaking & Defense
  "Ast%",             "ast_percent.x","Playmaking & Defense",
  "Ast% (2P)",        "ast_percent_2p.x","Playmaking & Defense",
  "Ast% (3P)",        "ast_percent_3p.x","Playmaking & Defense",
  "Stl%",              "st_percent.x","Playmaking & Defense",
  "Blk%",             "blk_percent.x","Playmaking & Defense"
)

row_map_def <- tribble(
  ~name, ~stat, ~group,
  
  # Advanced
  "Defensive Rating", "def_rtg.x", "Advanced",
  "eFG% Allowed",     "efg_percent.y", "Advanced",
  "FT Rate Allowed",  "ft_freq.y", "Advanced",
  "TOV Forced Rate",  "tov_percent.y", "Advanced",
  "DREB Rate",        "drb_percent.x", "Advanced",
  
  # Shooting Efficiency Allowed
  "Rim PPS Allowed",       "rim_pps.y", "Shooting Efficiency",
  "Paint PPS Allowed",     "paint_pps.y", "Shooting Efficiency",
  "Mid PPS Allowed",       "mid_pps.y", "Shooting Efficiency",
  "Corner 3 PPS Allowed",  "c3_pps.y", "Shooting Efficiency",
  "Above Break 3 PPS Allowed","l3_pps.y", "Shooting Efficiency",
  "3P%",           "x3p_percent.y", "Shooting Efficiency",
  "FT%",           "ft_percent.y", "Shooting Efficiency",
  
  # Shooting Frequency Allowed
  "Rim Rate Allowed",      "rim_freq.y", "Shooting Frequency",
  "Paint Rate Allowed",    "paint_freq.y", "Shooting Frequency",
  "Mid Rate Allowed",      "mid_freq.y", "Shooting Frequency",
  "Corner 3 Rate Allowed", "c3_freq.y", "Shooting Frequency",
  "Above Break Rate Allowed","l3_freq.y", "Shooting Frequency",
  "FT Rate Allowed",       "ft_freq.y", "Shooting Frequency",
  
  # Playmaking & Defense
  "Ast%",             "ast_percent.y","Playmaking & Defense",
  "Ast% (2P)",        "ast_percent_2p.y","Playmaking & Defense",
  "Ast% (3P)",        "ast_percent_3p.y","Playmaking & Defense",
  "Stl%",              "st_percent.y","Playmaking & Defense",
  "Blk%",             "blk_percent.y","Playmaking & Defense"
)


# ---- Lower better stats ----
lower_better_stats <- c("def_rtg.x","def_rtg.y","ft_freq.y","tov_percent.x","tov_percent.y",
                        "rim_pps.y","paint_pps.y","mid_pps.y","c3_pps.y",
                        "l3_pps.y","x3p_percent.y","ft_percent.y",
                        "rim_freq.y","paint_freq.y","mid_freq.y",
                        "c3_freq.y","l3_freq.y","ft_freq.y", 
                        "ast_percent.y","ast_percent_2p.y","ast_percent_3p.y",
                        "st_percent.y","blk_percent.y")

scale_cols <- c(
  # Shooting % and Freq
  "efg_percent.x","efg_percent.y",
  "tov_percent.x","tov_percent.y",
  "orb_percent.x","orb_percent.y",
  "drb_percent.x","drb_percent.y",
  "x3p_percent.x","x3p_percent.y",
  "ft_percent.x","ft_percent.y",
  "rim_freq.x","rim_freq.y",
  "paint_freq.x","paint_freq.y",
  "mid_freq.x","mid_freq.y",
  "c3_freq.x","c3_freq.y",
  "l3_freq.x","l3_freq.y",
  "ft_freq.x","ft_freq.y",
  
  # Playmaking & Defense percentages
  "ast_percent.x","ast_percent.y",
  "ast_percent_2p.x","ast_percent_2p.y",
  "ast_percent_3p.x","ast_percent_3p.y",
  "st_percent.x","st_percent.y",
  "blk_percent.x","blk_percent.y"
)


df <- df %>%
  mutate(across(any_of(scale_cols), 
                ~ ifelse(is.na(.) | !is.numeric(.), ., ifelse(. > 1, . / 100, .))))

# ---- Long DF with ranks ----
df_long <- df %>%
  select(team_abb, team_display, where(is.numeric)) %>%  # only numeric stats
  pivot_longer(-(team_abb:team_display), 
               names_to = "stat", values_to = "value") %>%
  group_by(stat) %>%
  mutate(rank = ifelse(
    stat %in% lower_better_stats,
    rank(value, ties.method = "first"),
    rank(-value, ties.method = "first")
  )) %>%
  ungroup()

row_map <- bind_cols(
  row_map_off %>% rename(stat_x = stat),
  row_map_def %>% select(stat_y = stat)
)

# ---- Attach matchup values + ranks ----
gt_df <- row_map %>%
  left_join(df_long %>% filter(team_abb == TM1), by = c("stat_x"="stat")) %>%
  rename(team1_val = value, team1_rank = rank) %>%
  left_join(df_long %>% filter(team_abb == TM2), by = c("stat_y"="stat")) %>%
  rename(team2_val = value, team2_rank = rank)

# ---- Lowercase suffix for ranks ----
append_suffix <- function(num) {
  suff <- case_when(
    num %% 100 %in% c(11,12,13) ~ "th",
    num %% 10 == 1 ~ "st",
    num %% 10 == 2 ~ "nd",
    num %% 10 == 3 ~ "rd",
    TRUE ~ "th"
  )
  paste0(num, suff) |> tolower()
}

gt_df <- gt_df %>%
  mutate(
    team1_rank_num = team1_rank,
    team2_rank_num = team2_rank
  )

gt_df <- gt_df %>%
  mutate(
    team1_rank = append_suffix(team1_rank),
    team2_rank = append_suffix(team2_rank)
  )


# ---- Title with logos ----
gt_title <- gt::md(
  paste0(
    "<img src='", df$logo[df$team_abb==TM1][1], "' style='height:28px;'> ",
    df$team_display[df$team_abb==TM1][1],
    " vs. ",
    df$team_display[df$team_abb==TM2][1],
    " <img src='", df$logo[df$team_abb==TM2][1], "' style='height:28px;'>"
  )
)

# ---- Subtitle ----
gt_subtitle <- gt::md(
  paste0(k1$wl, " (", k1$net, " Net) · ", k2$wl, " (", k2$net, " Net)")
)

# ---- Spanners without logos ----
spanner1 <- gt::md(paste0(TM1, " OFFENSE"))
spanner2 <- gt::md(paste0(TM2, " DEFENSE"))

# ---- Neutral stats (gray ranks) ----
neutral_stats <- c()

# # ---- Neutral stats (gray ranks) ----
# neutral_stats <- c("Ast%", "Ast% (2P)", "Ast% (3P)", 
#                    "Rim Rate", "Paint Rate", "Mid Rate", 
#                    "Corner 3 Rate", "Above Break Rate", "FT Rate")

# ---- Keep numeric ranks for coloring ----
gt_df <- row_map %>%
  left_join(df_long %>% filter(team_abb == TM1), by = c("stat_x"="stat")) %>%
  rename(team1_val = value, team1_rank_num = rank) %>%
  left_join(df_long %>% filter(team_abb == TM2), by = c("stat_y"="stat")) %>%
  rename(team2_val = value, team2_rank_num = rank)

gt_display <- gt_df %>%
  select(name, group, team1_val, team1_rank_num, team2_val, team2_rank_num)

# ---- Add row index to preserve row order ----
gt_off <- row_map_off %>%
  mutate(row_id = row_number()) %>%
  left_join(df_long %>% filter(team_abb == TM1),
            by = c("stat" = "stat")) %>%
  rename(value_1 = value, rank_1_num = rank)

gt_def <- row_map_def %>%
  mutate(row_id = row_number()) %>%
  left_join(df_long %>% filter(team_abb == TM2),
            by = c("stat" = "stat")) %>%
  rename(value_2 = value, rank_2_num = rank)

# ---- Merge offense + defense safely ----
gt_split <- gt_off %>%
  left_join(gt_def, by = c("group", "row_id")) %>%   # join also by row_id
  select(group, name_off = name.x, value_1, rank_1_num,
         name_def = name.y, value_2, rank_2_num) %>%
  mutate(
    rank_1 = append_suffix(rank_1_num),
    rank_2 = append_suffix(rank_2_num)
  )

# ---- Create GT Table ----
matchup_gt <- gt_split %>%
  gt(groupname_col = "group") %>%
  gt_theme_savant() %>%
  
  # ---- Header ----
tab_header(title = gt_title, subtitle = gt_subtitle) %>%
  
  # ---- Spanners ----
tab_spanner(label = paste0(TM1, " OFFENSE"), columns = c(value_1, rank_1_num)) %>%
  tab_spanner(label = paste0(TM2, " DEFENSE"), columns = c(value_2, rank_2_num)) %>%
  tab_style(
    style = cell_text(align = "left", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%
  
  # ---- Hide column labels ----
cols_label(
  name_off   = "",
  value_1    = "",
  rank_1_num = "",
  name_def   = "",
  value_2    = "",
  rank_2_num = ""
) %>%
  
  # ---- Column widths + alignment ----
cols_width(
  name_off ~ px(170),
  value_1 ~ px(70),
  name_def ~ px(170),
  value_2 ~ px(70),
) %>%
  cols_align(columns = c(name_off, name_def), align = "left") %>%
  cols_align(columns = c(rank_1_num, rank_2_num), align = "right") %>%
  
  # ---- Formatting ----
fmt_percent(columns = c(value_1, value_2),
            rows = str_detect(name_off, "%|Rate"),
            scale_values = TRUE, decimals = 1) %>%
  fmt_number(columns = c(value_1, value_2),
             rows = str_detect(name_off, "PPS|Rating|PPP"),
             decimals = 2) %>%
  
  # ---- Rank coloring (performance stats) ----
data_color(
  columns = rank_1_num,
  #rows = !(group %in% c("Shooting Frequency","Playmaking & Defense")),
  colors = scales::col_bin(
    palette = c("#3FA34D","#B4D98C","#EFD78A","#F4A259","#D64550"),
    bins = c(1,4,8,12,16,Inf), pretty = FALSE
  )
) %>%
  data_color(
    columns = rank_2_num,
    #rows = !(group %in% c("Shooting Frequency","Playmaking & Defense")),
    colors = scales::col_bin(
      palette = c("#3FA34D","#B4D98C","#EFD78A","#F4A259","#D64550"),
      bins = c(1,4,8,12,16,Inf), pretty = FALSE
    )
  ) %>%
  
  # ---- Neutral stats (gray ranks) ----
# data_color(
#   columns = c(rank_1_num, rank_2_num),
#   rows = group %in% c("Shooting Frequency","Playmaking & Defense"),
#   colors = scales::col_bin(
#     palette = c("gray95","gray70","gray50","gray30"),
#     bins = c(1,4,8,12,16,Inf), pretty = FALSE
#   )
# ) %>%

# ---- Overlay rank text ----
text_transform(
  locations = cells_body(columns = rank_1_num),
  fn = function(x) gt_split$rank_1
) %>%
  text_transform(
    locations = cells_body(columns = rank_2_num),
    fn = function(x) gt_split$rank_2
  ) %>%
  
  # ---- Category title styling ----
tab_style(
  style = list(
    cell_fill(color = "white"),
    cell_text(color = "black", weight = "bold", size = px(13), align = "left")
  ),
  locations = cells_row_groups()) %>%
  
  tab_options(
    row_group.border.top.width = px(2),
    row_group.padding = ".5px",
    row_group.border.top.style = "solid"  
  ) %>%
  
  cols_hide(columns = c(rank_1, rank_2))

# helpers for framing
first_grp <- unique(gt_split$group)[1]
last_row  <- nrow(gt_split)

matchup_gt <- matchup_gt %>%
  tab_options(
    table.border.top.style    = "none",
    table.border.bottom.style = "none",
    table.border.left.style   = "none",
    table.border.right.style  = "none",
    table_body.hlines.style   = "solid",  #breath
    table_body.vlines.style   = "none",
    heading.border.bottom.style = "none"
  ) %>%
  
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(2)),
    locations = cells_body(columns = rank_2_num)
  ) %>%
  tab_style(
    style = cell_borders(sides = "left", color = "black", weight = px(2)),
    locations = cells_body(columns = name_off)
  ) %>%
  
  # 4) thicker middle and right left top divider
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(2)),
    locations = list(
      cells_column_spanners(spanners = spanner1),
      cells_column_labels(columns = rank_1_num),
      cells_body(columns = rank_1_num)
    )
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(2)),
    locations = list(
      cells_column_spanners(spanners = spanner2),
      cells_column_labels(columns = rank_2_num),
      cells_body(columns = rank_2_num)
    )
  ) %>%
  tab_style(
    style = cell_borders(sides = "left", color = "black", weight = px(2)),
    locations = cells_column_labels(columns = name_off)
  ) %>%
  tab_style(
    style = cell_borders(sides = "top", color = "black", weight = px(2)),
    locations = list(
      cells_column_spanners(spanners = spanner1),
      cells_column_labels(columns = rank_1_num),
      cells_column_spanners(spanners = spanner2),
      cells_column_labels(columns = rank_2_num),
      cells_column_labels(columns = name_off),
      cells_column_labels(columns = name_def)
    )
  ) %>%
  
  # 5) bottom border for last row
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_body(
      columns = c(name_off, value_1, rank_1_num, name_def, value_2, rank_2_num),
      rows = last_row
    )
  ) %>%
  
  # 6) group separators
  tab_style(
    style = cell_borders(sides = "top", color = "black", weight = px(2)),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_borders(sides = "left", color = "black", weight = px(2)),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(2)),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_row_groups()
  ) %>%
  
  # 7) caption OUTSIDE frame
  tab_source_note(
    source_note = md(
      "<div style='text-align:right; color:gray; font-size:11px;'>Table: Can Sahin </div>" #Remove "Table: Can Sahin" from first table when creating combined tables
    )
  )

# ---- Save Table ----
gt_save_crop(
  matchup_gt,
  file = paste0("matchup_", TM1, "_vs_", TM2, ".png"),
  whitespace = 20,
  bg = "white"
)

