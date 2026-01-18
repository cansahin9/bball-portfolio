
Sys.setenv(CHROMOTE_CHROME = "D:/Program Files/Google/Chrome/Application/chrome.exe")

library(tidyverse)
library(euroleaguer)
library(gt)
library(gtExtras)
library(stringr)

# Step 1: Get player season totals
df <- getPlayerStats(season_code = "U2024", statistic_mode = "accumulated")

# Step 2: Compute league average 3P%
league_avg_3p <- sum(df$`3PM`) / sum(df$`3PA`)

# Step 3: Expected 3P% (padding formula)
df <- df %>%
  mutate(
    fg3Pct = `3PM` / `3PA`,
    xFg3Pct = (`3PM` + 50 * league_avg_3p) / (`3PA` + 50),
    diff = xFg3Pct - fg3Pct
  )

# Step 4: Rank top and bottom 10
df_ranked <- df %>%
  filter(`3PA` > 50) %>% # filter low volume shooters
  arrange(desc(xFg3Pct)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10 | rank >= n() - 9) %>%
  mutate(rankGroup = ifelse(rank <= 10, "Top 10", "Bottom 10"))

df_ranked <- df_ranked %>%
  mutate(
    PlayerName = str_to_title(str_trim(str_replace(PlayerName, "(.*),\\s*(.*)", "\\2 \\1")))
  )

# Step 5: Build gt table
gt_table <- df_ranked %>%
  select(ImageUrl, PlayerName, `3PA`, fg3Pct, xFg3Pct, rankGroup) %>%
  group_by(rankGroup) %>%
  gt() %>%
  gt_img_rows(columns = ImageUrl, height = 25) %>%   
  fmt_percent(columns = c(fg3Pct, xFg3Pct), decimals = 1) %>%
  tab_header(
    title = md("**EuroCup Expected 3P% Leaderboards**"),
    subtitle = md("**2024-25 Season**")
  ) %>%
  cols_label(
    ImageUrl = "",
    PlayerName = "Player",
    `3PA` = "3PA",
    fg3Pct = "Actual 3P%",
    xFg3Pct = "Expected 3P%"
  ) %>%
  cols_align(align = "left", columns = PlayerName) %>%
  cols_align(align = "center", columns = c(`3PA`, fg3Pct, xFg3Pct)) %>%
  tab_footnote(
    footnote = "Expected 3P% = (3PM + 50 × League Avg 3P%) / (3PA + 50)",
    locations = cells_column_labels(columns = c(xFg3Pct))
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.background.color = "floralwhite",
    column_labels.font.size = 12,
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    row_group.background.color = "#E5E1D8",
    table.font.size = 10,
    table.align = "center", 
    heading.title.font.size = 20,
    heading.subtitle.font.size = 12.5,
    table.font.names = "Consolas",
    data_row.padding = px(1),
    footnotes.padding = px(0.5)
  ) %>%
  tab_source_note(
    source_note = md("**Source**: 'Stabilization Rates and the Padding Approach' by Kostya Medvedovsky <br>**Table**: Can Sahin")
  )


# Save the table as an image
gtsave(
  gt_table,
  "ec_padded_leaderboard.png",
  vwidth = 1200,   
  vheight = 1600
)

