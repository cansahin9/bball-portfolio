library(sportyR)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(stringr)

# Load data
basketball_shots <- read_csv("8.4.2025-23.58-practice.csv")

basketball_shots <- basketball_shots %>%
  mutate(Outcome = str_to_title(Outcome))  # "made" → "Made"


p1 <- geom_basketball(
  league = "ncaa",
  color_updates = list(
    court_background = "#e8e0d7",
    center_circle_color = "#CA0020",
    division_line_color = "#CA0020",
    endline_color = "#CA0020",
    sideline_color = "#CA0020",
    team_bench_area_color = "#0571B0",
    substitution_line_color = "#0571B0",
    court_apron_color = "#0571B0",
    three_point_line_color = "#0571B0",
    two_point_area_color = "#e8e0d7",
    key_color = "#CA0020",
    key_border_color = "#0571B0",
    free_throw_circle_color = "#0571B0",
    free_throw_circle_fill = "#e8e0d7",
    lower_defensive_box_color = "#e8e0d7",
    restricted_area_arc_color = "#0571B0",
    backboard_color = "#0571B0",
    basket_ring_color = "#0571B0",
    net_color = "#e8e0d7"
  )
) +
  geom_point(data = basketball_shots, aes(x = X, y = Y, color = Outcome), size = 3) +
  geom_text(data = basketball_shots, aes(x = X, y = Y, label = Player),
            color = "white", size = 2, fontface = "bold", family = "sans") +
  scale_color_manual(values = c("Make" = "green3", "Miss" = "red2"))+
  labs(title = "Practice Shot Chart - 05/08/25") +
  theme(
    panel.background = element_rect(fill = "gray15", color = "gray15"),
    plot.background = element_rect(fill = "gray15", color = "gray15"),
    text = element_text(family = "sans"),
    plot.title = element_text(colour = "white", hjust = 0.5, face = "bold", size = 14),
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.075),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill = "transparent", colour = "transparent"),
    legend.text = element_text(size = 8, face = "bold", color = "white")
  )

# Draw
cowplot::ggdraw(p1)

# Save
ggsave("basketball_shot_chart.png", plot = p1, height = 5, width = 8, dpi = "retina")

# Basketball data / court aesthetics
# shot plotter link
#https://shot-plotter.netlify.app/basketball-ncaa
