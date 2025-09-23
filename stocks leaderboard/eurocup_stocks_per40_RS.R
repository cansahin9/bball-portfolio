# --- Setup ---
setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/leaderboard plot")

library(tidyverse)
library(euroleaguer)
library(ggimage)
library(janitor)

# 1. Custom theme
theme_f5 <- function (font_size = 9) { 
  theme_minimal(base_size = font_size, base_family = "Roboto") %+replace% 
    theme(
      plot.background = element_rect(fill = "floralwhite", color = "floralwhite"), 
      panel.grid.minor = element_blank(), 
      plot.title = element_text(hjust = 0, size = 14, face = 'bold'), 
      plot.subtitle = element_text(color = 'gray65', hjust = 0, margin = margin(2.5, 0, 10, 0), size = 11), 
      plot.caption = element_text(color = 'gray65', hjust = 1, size = 6, margin = margin(t = 5))
    )
}

# 2. Season code
season_code <- "U2024"

# 3. Get competition games and filter regular season only
rounds <- getCompetitionRounds(season_code = season_code)
games <- map_dfr(rounds$Round, ~getCompetitionGames(season_code = season_code, round = .x))
games_rs <- games %>% filter(PhaseTypeCode == "RS")

# 4. Pull all RS boxscores
boxscores <- map(games_rs$GameCode, ~{
  bs <- getGameBoxScore(season_code = season_code, game_code = .x)
  tibble(bs$PlayerStats) %>% mutate(GameCode = .x)
}) %>% bind_rows()

# 5. Aggregate player RS totals
stats_rs <- boxscores %>%
  clean_names() %>%
  mutate(
    stl = as.numeric(stl),
    blk = as.numeric(blk),
    seconds = as.numeric(seconds)
  ) %>%
  replace_na(list(stl = 0, blk = 0, seconds = 0)) %>%
  group_by(player_id, player, team_code) %>%
  summarise(
    STL = sum(stl, na.rm = TRUE),
    BLK = sum(blk, na.rm = TRUE),
    MIN = sum(seconds, na.rm = TRUE) / 60,   # convert total seconds → minutes
    .groups = "drop"
  )

# --- Helper: Find last team for a player ---
get_last_team <- function(team_codes) {
  codes <- str_split(team_codes, ";")[[1]]
  if (length(codes) == 1) return(codes)
  
  last_team <- games_rs %>%
    filter(HomeCode %in% codes | AwayCode %in% codes) %>%
    arrange(desc(Date)) %>%
    slice(1) %>%
    {
      if (.$HomeCode %in% codes) .$HomeCode else .$AwayCode
    }
  
  return(last_team)
}

# 6. Clean stats and fix names/teams
df <- stats_rs %>%
  filter(MIN > 300) %>%
  rowwise() %>%
  mutate(TeamCode = get_last_team(team_code)) %>%
  ungroup() %>%
  mutate(
    # remove jersey numbers like " #23"
    PlayerName = str_remove(player, "\\s*#\\d+$"),
    PlayerName = str_to_lower(PlayerName),
    PlayerName = str_trim(PlayerName),
    PlayerName = str_replace(PlayerName, "^(.*?),\\s*(.*)$", "\\2 \\1"),
    PlayerName = str_to_title(PlayerName, locale = "en"),
    
    # restore initials
    PlayerName = str_replace_all(PlayerName, "\\b([A-Z])([A-Z])\\b", "\\1\\2"),
    PlayerName = str_replace(PlayerName, "\\bDj\\b", "DJ"),
    PlayerName = str_replace(PlayerName, "\\bCj\\b", "CJ"),
    
    # suffixes
    PlayerName = str_replace(PlayerName, "\\bJr\\b", "Jr."),
    PlayerName = str_replace(PlayerName, "\\bSr\\b", "Sr."),
    PlayerName = str_replace(PlayerName, "\\bIi\\b", "II"),
    PlayerName = str_replace(PlayerName, "\\bIii\\b", "III"),
    PlayerName = str_replace(PlayerName, "\\bIv\\b", "IV"),
    
    # fix DeShawn
    PlayerName = str_replace(PlayerName, "^De'shawn", "DeShawn"),
    
    # clean plot formatting
    PlayerNamePlot = str_replace(PlayerName, "^(.*) (\\S+)$", "\\1\n\\2"),
    
    # fix Mc / Mac capitalization
    PlayerName = str_replace_all(PlayerName, "\\bMc([a-z])", ~paste0("Mc", str_to_upper(str_sub(.x, 3, 3)), str_sub(.x, 4))),
    PlayerName = str_replace_all(PlayerName, "\\bMac([a-z])", ~paste0("Mac", str_to_upper(str_sub(.x, 4, 4)), str_sub(.x, 5))),
    
    # add defensive stocks
    stops = STL + BLK,
    stop_per40 = 40 * stops / MIN
  ) %>%
  arrange(desc(stop_per40)) %>%
  slice_head(n = 50)


# 7. Team logos + colors
team_lookup <- getCompetitionTeams(season_code = season_code) %>%
  transmute(
    TeamCode,
    ec_logo_svg = ImagesCrest,
    color = if_else(str_detect(PrimaryColor, "^#[0-9A-Fa-f]{6}$"), PrimaryColor, "#999999"),
    alternate_color = if_else(str_detect(SecondaryColor, "^#[0-9A-Fa-f]{6}$"), SecondaryColor, "#cccccc")
  )

df <- df %>%
  left_join(team_lookup, by = "TeamCode")

# 8. Layout for leaderboard
df <- df %>%
  mutate(cn = (row_number() - 1) %/% 10 + 1) %>%
  group_by(cn) %>%
  mutate(rn = row_number()) %>%
  ungroup() %>%
  mutate(rank = paste0(row_number(), "."))

# 9. Plot
p <- df %>%
  ggplot(aes(x = 1, y = 1)) +
  scale_x_continuous(limits = c(.9, 1.1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(.9, 1.05)) +
  facet_wrap(rn ~ cn, ncol = 5) +
  coord_cartesian(clip = 'off') +
  theme_f5() +
  theme(
    axis.text = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  ) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, color = color), 
            fill = 'transparent', linewidth = 1) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, color = alternate_color), 
            fill = 'transparent', linewidth = .35) +
  scale_color_identity() +
  geom_text(aes(x = .925, y = .975, label = rank), size = 3, hjust = 1, family = "Roboto") +
  geom_text(aes(x = .955, y = .975, label = PlayerNamePlot),
            size = 2.5, hjust = 0, family = "Roboto", lineheight = 0.9)+
  geom_text(aes(x = 1.090, y = .975, label = sprintf("%.2f", stop_per40)), size = 3, hjust = 1, family = "Roboto") +
  geom_image(aes(x = .9375, y = .975, image = ec_logo_svg), size = .75) +
  labs(
    title = "Top 50 Eurocup Players in Defensive Stocks Per 40",
    subtitle = "A stock is a steal + block. 2024–25 Regular Season (min. 300 minutes played).",
    caption = "Data: euroleaguer | Plot: Can Sahin"
  )

# 10. Save
ggsave("eurocup_stocks_per40_RS.png", p, w = 8, h = 4, dpi = 600)
