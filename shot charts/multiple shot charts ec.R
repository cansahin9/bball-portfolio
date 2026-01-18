
# ───── Load Libraries ─────
library(euroleaguer)
library(ggplot2)
library(tidyverse)
library(janitor)
library(cowplot)
library(extrafont)
library(dplyr)
library(purrr)
library(ggrepel)

# ───── Court Construction ─────
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  tibble(x = center[1] + radius * cos(angles),
         y = center[2] + radius * sin(angles))
}

width = 55
height = 94 / 2
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
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "white"
  )
)

plot_court = function(court_theme = court_themes$ppt, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 22
    three_point_side_height = 0
  }
  
  court_points = tibble(x = c(-width / 2, width / 2), y = c(0, 0), desc = "baseline") %>%
    bind_rows(tibble(
      x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
      y = c(0, key_height, key_height, 0), desc = "outer_key"
    )) %>%
    bind_rows(tibble(x = c(-backboard_width / 2, backboard_width / 2), y = c(backboard_offset, backboard_offset), desc = "backboard")) %>%
    bind_rows(tibble(x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"))
  
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
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = tibble(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
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
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

# ───── LOAD GAME DATA ─────
safe_getGamePoints <- possibly(
  function(code) getGamePoints(season_code = "U2025", game_code = code),
  otherwise = NULL
)

rounds <- 6:16  
games <- map(rounds, safe_getGamePoints)
games <- compact(games)
game_data <- bind_rows(games)

# ───── FILTER PLAYER SHOTS ─────
player_name <- "PHILLIP, TARIK"

# Fix shot made logic and zone classification
shots <- game_data %>%
  filter(Player == player_name) %>%
  filter(Action_ID %in% c("2FGM", "2FGA", "3FGM", "3FGA")) %>%
  mutate(
    x = ((CoordX * 0.55)) * 0.0625,
    y = ((CoordY * 0.45) + 58 ) * 0.0783,
    isShotMade = Action_ID %in% c("2FGM", "3FGM"),
    distance = sqrt(x^2 + (y - hoop_center_y)^2),
    zone = case_when(
      distance <= 4 ~ "Restricted Area",
      y <= 17 & abs(x) <= 8 ~ "Paint (Non-RA)",
      y <= 22.3 ~ "Midrange",
      y > 22.3 ~ "Three Point",
      TRUE ~ "Other"
    )
  )


# # ───── CHART 1: SHOT CHART (MADE vs MISSED) ─────
# p1 <- plot_court() +
#   geom_point(data = shots,
#              aes(x = x, y = y, color = isShotMade, fill = isShotMade),
#              size = 3, shape = 21, stroke = 0.5) +
#   scale_color_manual(values = c("TRUE" = "green4", "FALSE" = "red4"), labels = c("TRUE" = "Made", "FALSE" = "Missed")) +
#   scale_fill_manual(values = c("TRUE" = "green2", "FALSE" = "red2"), labels = c("TRUE" = "Made", "FALSE" = "Missed")) +
#   ggtitle(paste(player_name, "- Shot Chart"),
#           subtitle = paste("EuroCup 2025/26 – As of", format(Sys.Date(), "%d %b %Y"))) +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 20, family = "Roboto", face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Roboto", face = "bold"),
#     legend.position = "bottom",
#     legend.title = element_blank()
#   )
# 
# ggsave(paste0(player_name, " - Shot Chart.png"), p1, height = 6, width = 6, dpi = 300)

# # ───── CHART 2: HEATMAP ─────
# p2 <- plot_court() +
#   stat_density_2d(
#     data = shots,
#     aes(x = x, y = y, fill = ..level..),
#     geom = "polygon", alpha = 0.6
#   ) +
#   scale_fill_viridis_c(option = "plasma") +
#   ggtitle(paste(player_name, "- Shot Density Heatmap"),
#           subtitle = paste("EuroCup 2025/26 – As of", format(Sys.Date(), "%d %b %Y"))) +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold")
#   )
# 
# ggsave(paste0(player_name, " - Heatmap.png"), p2, height = 6, width = 6, dpi = 300)

shots <- shots %>%
  mutate(
    x = ((CoordX * 0.55)) * 0.0625,
    y = ((CoordY * 0.45) + 58 ) * 0.0783,
    isShotMade = Action_ID %in% c("2FGM", "3FGM"),
    zone = case_when(
      Action_ID %in% c("3FGM", "3FGA") ~ "Three Point",
      sqrt(x^2 + (y - hoop_center_y)^2) <= 4 ~ "Restricted Area",
      y <= 17 & abs(x) <= 8 ~ "Paint (Non-RA)",
      TRUE ~ "Midrange"
    ),
    zone = factor(zone, levels = c("Restricted Area", "Paint (Non-RA)", "Midrange", "Three Point"))
  )

zone_summary <- shots %>%
  group_by(zone) %>%
  summarise(
    Attempts = n(),
    Made = sum(isShotMade),
    FG_Pct = round(100 * Made / Attempts, 1),
    label = paste0(FG_Pct, "% (", Made, "/", Attempts, ")"),
    x = median(x),
    y = median(y)
  ) %>%
  mutate(
    x = if_else(zone == "Three Point", 0, x),      # center 3PT label
    y = if_else(zone == "Three Point", 32, y)      # place above arc
  ) %>%
  filter(!is.na(FG_Pct))

# Format name first
formatted_name <- player_name %>%
  str_split(", ") %>%
  unlist() %>%
  rev() %>%
  str_to_title() %>%
  paste(collapse = " ")


# Plot: FG% + Attempts per Zone
p_fg_by_zone <- plot_court() +
  geom_point(data = shots, aes(x = x, y = y, color = zone), alpha = 0.5, size = 3) +
  geom_label_repel(
    data = zone_summary,
    aes(x = x, y = y, label = label),
    size = 4,
    box.padding = 0.3,
    label.size = 0.5,
    fill = "white",
    color = "black",
    fontface = "bold",
    alpha = 0.60  # Transparency
  ) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle(
    paste("FG% by Zone"),
    subtitle = paste("EuroCup 2025/26 – As of", format(Sys.Date(), "%d %b %Y"))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, family = "Roboto", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Roboto", face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.6, "lines"),
    legend.spacing.x = unit(0.3, 'cm')
  )


ggsave(paste0(player_name, " - FG by Zone (Attempts).png"), p_fg_by_zone, height = 6, width = 6, dpi = 300)

