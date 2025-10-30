setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/reports/post game report")

# ───── SETUP ─────
library(euroleaguer)
library(ggplot2)
library(tidyverse)
library(janitor)
library(cowplot)
library(extrafont)
library(dplyr)
library(purrr)

# ───── Court Constants ─────
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  tibble(x = center[1] + radius * cos(angles),
         y = center[2] + radius * sin(angles))
}

width = 55
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 22.84567
three_point_side_radius = 22.3535
three_point_side_height = 10.009711

court_themes = list(
  ppt = list(
    court = "white",
    lines = 'black',
    text = 'black',
    made = 'green3',
    missed = 'red2'
  )
)

# ───── Court Plot Function ─────
plot_court = function(court_theme = court_themes$ppt) {
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  foul_circle_top = filter(foul_circle, y > key_height) %>% mutate(desc = "foul_circle_top")
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(angle = atan((y - key_height) / x) * 180 / pi,
           angle_group = floor((angle - 5.625) / 11.25),
           desc = paste0("foul_circle_bottom_", angle_group)) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>% mutate(desc = "hoop")
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>% filter(y >= hoop_center_y) %>% mutate(desc = "restricted")
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>% filter(y >= three_point_side_height, y >= hoop_center_y)
  
  court_points = tibble(x = c(-width / 2, width / 2), y = c(0, 0), desc = "baseline") %>%
    bind_rows(tibble(x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
                     y = c(0, key_height, key_height, 0), desc = "outer_key")) %>%
    bind_rows(tibble(x = c(-backboard_width / 2, backboard_width / 2), y = c(backboard_offset, backboard_offset), desc = "backboard")) %>%
    bind_rows(tibble(x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck")) %>%
    bind_rows(foul_circle_top, foul_circle_bottom, hoop, restricted) %>%
    bind_rows(tibble(
      x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x,
            -three_point_side_radius, -three_point_side_radius),
      y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
      desc = "three_point_line"
    ))
  
  ggplot() +
    geom_path(data = court_points, aes(x = x, y = y, group = desc), color = court_theme$lines) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = 'white', color = 'white'),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

# ───── LOAD SHOT DATA ─────
game_code <- 46   # Replace with your game code
team_code <- "TTK"

game_data <- getGamePoints(season_code = "U2025", game_code = game_code)

shots <- game_data %>%
  filter(TeamCode == team_code) %>%
  filter(Action_ID %in% c("2FGA", "2FGM", "3FGA", "3FGM")) %>%
  mutate(
    x = ((CoordX * 0.55)) * 0.0625,
    y = ((CoordY * 0.45) + 58) * 0.0783,
    isShotMade = Action_ID %in% c("2FGM", "3FGM")
  )

# ───── PLOT SHOT CHART ─────
p <- plot_court() +
  geom_point(data = shots,
             aes(x = x, y = y, fill = isShotMade),
             color = "black", shape = 21, size = 3, stroke = 0.3) +
  scale_fill_manual(
    values = c("TRUE" = "green3", "FALSE" = "red2"),
    breaks = c("TRUE", "FALSE"),  # this forces the order
    labels = c("Made", "Missed")
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.margin = margin(t = -20, b = 20)
  )

# ───── SAVE ─────
ggsave("lions vs ttk _All_Shots_Simple 2.png", p, width = 6, height = 6, dpi = 300)
