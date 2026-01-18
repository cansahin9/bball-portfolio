Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)

# Packages
library(tidyverse)
library(euroleaguer)
library(janitor)
library(ggtext)
library(cowplot)
library(MASS)
library(conflicted)

# Resolve common conflicts
conflict_prefer("filter","dplyr"); conflict_prefer("select","dplyr")
conflict_prefer("rename","dplyr"); conflict_prefer("lag","dplyr")

# CHOOSE SEASON & TEAMS
SEASON <- "U2024"               

teamA_query <- "Aris"       
teamB_query <- "Turk Telekom"      

# colors for the plot (low = TeamB, high = TeamA)
col_teamA <- "#937C08"
col_teamB <- "#208297"

# 1) FIBA HALF COURT (METERS)
width  <- 15.00     # baseline-to-baseline width
height <- 14.00     # baseline to halfcourt line
key_height <- 5.80
inner_key_width <- 3.60
outer_key_width <- 4.90
backboard_width <- 1.80
backboard_offset <- 1.20
neck_length <- 0.151
hoop_radius <- 0.225
hoop_center_y <- backboard_offset + neck_length + hoop_radius  # ≈ 1.576 m
three_point_radius <- 6.75
three_point_side_radius <- 6.60
three_point_side_height <- 2.99

circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) {
  ang <- seq(0, 2*pi, length.out = npoints)
  tibble(x = center[1] + radius*cos(ang),
         y = center[2] + radius*sin(ang))
}

build_fiba_court_m <- function() {
  base <- tibble(
    x = c( width/2, -width/2,
           outer_key_width/2,  outer_key_width/2, -outer_key_width/2, -outer_key_width/2,
           -backboard_width/2,  backboard_width/2,
           0, 0),
    y = c( 0, 0,
           0, key_height, key_height, 0,
           backboard_offset, backboard_offset,
           backboard_offset, backboard_offset + neck_length),
    desc = c(rep("perimeter", 2), rep("outer_key", 4), rep("backboard", 2), rep("neck", 2))
  )
  
  foul <- circle_points(c(0, key_height), inner_key_width/2)
  foul_top <- foul %>% filter(y >  key_height) %>% mutate(desc = "foul_circle_top")
  foul_bot <- foul %>% filter(y <= key_height) %>% mutate(desc = "foul_circle_bottom")
  
  hoop <- circle_points(c(0, hoop_center_y), hoop_radius) %>% mutate(desc = "hoop")
  restricted <- circle_points(c(0, hoop_center_y), 1.25) %>% filter(y >= hoop_center_y) %>% mutate(desc = "restricted")
  
  arc <- circle_points(c(0, hoop_center_y), three_point_radius) %>% filter(y >= three_point_side_height)
  three_line <- tibble(
    x = c(three_point_side_radius, three_point_side_radius, arc$x,
          -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, arc$y,
          three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  bind_rows(base, foul_top, foul_bot, hoop, restricted, three_line) |>
    mutate(dash = if_else(desc == "foul_circle_bottom", "longdash", "solid"))
}

court_points <- build_fiba_court_m()

# 2) GAMES, LOOKUP & SHOT DATA
rounds <- getCompetitionRounds(SEASON)
games  <- map_dfr(rounds$Round, ~ tryCatch(getCompetitionGames(SEASON, round = .x), error = function(e) NULL))

# Build name<->code lookup from schedule
team_lookup <- bind_rows(
  games %>% transmute(code = HomeCode, name = HomeName),
  games %>% transmute(code = AwayCode, name = AwayName)
) %>% distinct() %>% arrange(code, name)

# Resolve a user query (name or 3-letter code) to list(code, name)
resolve_team <- function(query) {
  q <- toupper(query)
  # direct code match
  by_code <- team_lookup %>% filter(toupper(code) == q)
  if (nrow(by_code) > 0) return(list(code = by_code$code[1], name = by_code$name[1]))
  # fuzzy name match
  by_name <- team_lookup %>% filter(str_detect(name, regex(query, ignore_case = TRUE)))
  stopifnot(nrow(by_name) > 0)
  list(code = by_name$code[1], name = by_name$name[1])
}

tA <- resolve_team(teamA_query)
tB <- resolve_team(teamB_query)

games_for_code <- function(code) {
  games %>% filter(HomeCode == code | AwayCode == code) %>% pull(GameCode)
}

teamA_games <- games_for_code(tA$code)
teamB_games <- games_for_code(tB$code)

get_team_shots <- function(game_codes, team_code_str) {
  map_dfr(game_codes, function(gc) {
    tryCatch({
      df <- getGamePoints(SEASON, gc) %>% janitor::clean_names()
      df |>
        dplyr::filter(.data$team_code == team_code_str) |>
        dplyr::transmute(
          x = as.numeric(.data$coord_x),
          y = as.numeric(.data$coord_y),
          game_code = gc
        )
    }, error = function(e) NULL)
  })
}

teamA_shots <- get_team_shots(teamA_games, tA$code) %>% mutate(name_team = tA$name)
teamB_shots <- get_team_shots(teamB_games, tB$code) %>% mutate(name_team = tB$name)

# 3) ENGINE UNITS -> METERS (+ rim alignment)
# EuroCup engine ranges observed: X ~ [-700,+700] (≈1400), Y ~ [0,1300]
x_scale <- width  / 1400
y_scale <- height / 1300

shots_raw_m <- bind_rows(teamA_shots, teamB_shots) %>%
  mutate(x_m = x * x_scale,
         y_m = y * y_scale)

# Align the median near-rim cluster to the true hoop center
rim_med_y <- shots_raw_m %>%
  filter(abs(x_m) < 0.5, y_m < 3) %>%
  summarize(med = median(y_m, na.rm = TRUE)) %>% pull(med)

delta_y <- ifelse(is.finite(rim_med_y), hoop_center_y - rim_med_y, 0)

shots_all <- shots_raw_m %>%
  mutate(
    locationX = x_m,
    locationY = pmin(height, pmax(0, y_m + delta_y))
  ) %>%
  select(locationX, locationY, name_team)

# 4) KDE DIFFERENCE (meters)
tm_density_compare <- function(df, team1, team2, n = 200) {
  df1 <- df %>% filter(name_team == team1)
  df2 <- df %>% filter(name_team == team2)
  if (nrow(df1) < 3 || nrow(df2) < 3) stop("Not enough shots for one or both teams.")
  
  bw_x <- MASS::bandwidth.nrd(c(df1$locationX, df2$locationX))
  bw_y <- MASS::bandwidth.nrd(c(df1$locationY, df2$locationY))
  lims <- c(-width, width, 0, height)
  
  d1 <- MASS::kde2d(df1$locationX, df1$locationY, h = c(bw_x, bw_y), n = n, lims = lims)
  d2 <- MASS::kde2d(df2$locationX, df2$locationY, h = c(bw_x, bw_y), n = n, lims = lims)
  
  z <- d1$z - d2$z
  colnames(z) <- d1$y
  
  as_tibble(z) %>%
    mutate(x_coord = d1$x) %>%
    pivot_longer(-x_coord, names_to = "y_coord", values_to = "z") %>%
    mutate(y_coord = as.numeric(y_coord))
}

density_data <- tm_density_compare(shots_all, tA$name, tB$name, n = 200)

# Prefilter grid to avoid warnings and plot only what's visible
xlim_use <- c(-width/1.5, width/1.5)
ylim_use <- c(0, height)
density_plot <- density_data %>%
  filter(between(x_coord, xlim_use[1], xlim_use[2]),
         between(y_coord, ylim_use[1], ylim_use[2]),
         is.finite(z)) %>%
  distinct(x_coord, y_coord, .keep_all = TRUE)

# 5) PLOT (Owen-style, FIBA meters)
p <- ggplot() +
  geom_raster(data = density_plot, aes(x = x_coord, y = y_coord, fill = z), na.rm = TRUE) +
  stat_contour(data = density_plot, aes(x = x_coord, y = y_coord, z = z, color = after_stat(level)),
               bins = 10, na.rm = TRUE) +
  scale_fill_gradient2(low = col_teamB, mid = "floralwhite", high = col_teamA, midpoint = 0) +
  scale_color_gradient2(low = col_teamB, mid = "floralwhite", high = col_teamA, midpoint = 0) +
  guides(fill = "none", color = "none") +
  geom_path(data = court_points, aes(x = x, y = y, group = desc, linetype = dash),
            color = "black", linewidth = 0.9) +
  scale_linetype_identity() +
  scale_x_continuous(limits = xlim_use, expand = expansion(mult = 0)) +
  scale_y_continuous(limits = ylim_use, expand = expansion(mult = 0)) +
  coord_fixed(clip = "off") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = NA),
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = ggtext::element_markdown(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 11, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.caption = element_text(hjust = .5)
  ) +
  labs(
    title = glue::glue("<span style='color:{col_teamA}'>Aris</span> vs. <span style='color:{col_teamB}'>**Turk Telekom**</span>"),
    subtitle = glue::glue("Color indicates areas with more shot attempts by the <span style='color:{col_teamA}'>**Aris**</span> or by the <span style='color:{col_teamB}'>**Turk Telekom**</span>"),
    caption  = "EuroCup 2024–25"
  )

# Draw with background and save (use 15:14 aspect for fuller court)
p <- cowplot::ggdraw(p) + theme(plot.background = element_rect(fill = "floralwhite", color = NA))
ggsave(glue::glue("{tA$code}_{tB$code}_density_plot.png"), p, width = 6, height = 6, units = "in", dpi = 600)



