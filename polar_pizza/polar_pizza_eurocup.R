setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/polar_pizza")

library(tidyverse)
library(ggplot2)
library(devtools)
library(geomtextpath)
library(cowplot)   
library(magick) 
library(patchwork)

# ---- Data ----
index <- 1:9
stat <- c("PPG", "eFG%", "TS%", "AST%", "TOV%", "USG%", "RPG", "ORB%", "DRB%")
value <- c(91, 35, 44, 61, 76, 92, 79, 46, 90)

data <- data.frame(index, stat, value) %>% 
  mutate(type = case_when(
    index %in% 1:5 ~ "Offense",
    index %in% 6   ~ "General",
    index %in% 7:9 ~ "Rebounding"
  ))

data$type <- factor(data$type, levels = c("Offense", "General", "Rebounding"))

# ---- Colors ----
color1 <- "#006400"   # Badalona green
color2 <- "grey"
color3 <- "#333333"   # light black

# ---- Polar Pizza Chart ----
p <- ggplot(data = data, aes(x = reorder(stat, index), y = value, label= value, fill = type)) +
  geom_bar(width = 1, color = "oldlace", stat = "identity") +
  coord_curvedpolar() +  
  geom_bar(aes(y=100, fill=type), stat="identity", width=1, alpha=0.5) +
  geom_hline(yintercept = seq(0, 100, by = 100), color = "oldlace", size = 1) +
  geom_vline(xintercept = seq(.5, 9.5, by = 1), color = "oldlace", size = .5) +
  geom_label(color = "gray20", fill = "oldlace", size=2.5, fontface="bold", family = "Sans", show.legend = FALSE) +
  scale_fill_manual(values=c(color1, color2, color3)) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "oldlace", color="oldlace"),
    legend.title = element_blank(),
    legend.text = element_text(colour = "gray20", family = "Sans", face = "bold"),
    legend.key.size = unit(.5, "cm"),
    legend.box.spacing = unit(0, "mm"),
    plot.background = element_rect(fill = "oldlace", color="oldlace"),
    panel.background = element_rect(fill = "oldlace", color="oldlace"),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(face = "bold", size = 6.8, colour = "gray20"),
    axis.title = element_blank(),
    axis.text.x = element_text(face = "bold", size = 7, family = "Sans")
  ) +
  labs(
    title = NULL, 
    subtitle = NULL, 
    caption = NULL,
    x = NULL, y = NULL
  )

# Remove title/subtitle from chart for patchwork
p_no_title <- p + labs(title = NULL, subtitle = NULL, caption = NULL)

# ---- Image Panel ----
img_panel <- ggdraw() +
  draw_image("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/polar_pizza/sam_dribble.png",
             x = 0, y = 0, width = 2, height = 1, hjust = 0, vjust = 0) +
  theme(
    plot.margin = margin(0,0,0,0),
    plot.background = element_rect(fill = "oldlace", colour = "oldlace")
  )

# ---- Combine Photo + Chart + Title ----
final <- (img_panel | p_no_title) +
  plot_layout(widths = c(0.9, 1.6)) +
  plot_annotation(
    title    = "Sam Dekker Eurocup Percentiles 24-25",
    subtitle = "Data: 3stepsbasket | viz: Can Sahin",
    caption  = "Percentiles show how a player compares to the league: e.g., 91 means better than 91% of EuroCup players."
  ) &
  theme(
    plot.background = element_rect(fill = "oldlace", colour = "oldlace"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, colour = "gray20"),
    plot.subtitle = element_text(hjust = 0.5, size = 8, colour = "gray20"),
    plot.caption = element_text(size = 7, colour = "gray20")
  )

# ---- Save ----
ggsave("dekker_polar_dribble.png", final, width = 10, height = 6, dpi = 300, bg = "oldlace")
