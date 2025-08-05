setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/off_def_rating_scatter")

# Libraries
library(tidyverse)
library(ggrepel)
library(prismatic)

# Load EuroCup CSV
eurocup <- read_csv("24_25_teams_adv_stats.csv")  # Update path if needed

# Clean column names
eurocup <- eurocup %>% janitor::clean_names()

# Optional: Clean team names if needed
eurocup$team <- str_replace_all(eurocup$team, "\\s+", " ")

# Add custom colors (partial — expand this as needed)
eurocup_colors <- tibble::tibble(
  team = c(
    "Valencia Basket",
    "Hapoel Tel Aviv",
    "Bahcesehir Koleji",
    "Hapoel Bank Yahav Jerusalem",
    "JL Bourg-en-Bresse",
    "Dreamland Gran Canaria",
    "U-Banca Transilvania Cluj Napoca",
    "KK Cedevita Olimpija Ljubljana",
    "Buducnost Voli Podgorica",
    "Besiktas Icrypex",
    "Wolves Twinsbet Vilnius",
    "Umana Venezia",
    "Turk Telekom",
    "Ratiopharm Ulm",
    "Joventut Badalona",
    "7Bet-Lietkabelis Panevezys",
    "Dolomiti Energia Trento",
    "Veolia Towers Hamburg",
    "Trefl Sopot",
    "Aris Midea Thessaloniki"
  ),
  color = c(
    "#F47920",  # Valencia - Orange
    "#D50A0A",  # Hapoel Tel Aviv - Red
    "#002F6C",  # Bahcesehir - Dark Blue
    "#B30B0B",  # Hapoel Jerusalem - Crimson
    "#C8102E",  # JL Bourg - Red
    "#FFD700",  # Gran Canaria - Yellow/Gold
    "#000025",  # Cluj Napoca - Yellow
    "#007A33",  # Olimpija Ljubljana - Green
    "#0033A0",  # Buducnost - Blue
    "#000000",  # Besiktas - Black
    "#00205B",  # Wolves Vilnius - Navy Blue
    "#9E1B32",  # Venezia - Maroon
    "#005BAC",  # Turk Telekom - Blue
    "#F39200",  # Ulm - Orange
    "#007F3E",  # Joventut - Green
    "#56008C",  # Lietkabelis - Purple
    "#A6A6A6",  # Trento - Gray
    "#231F20",  # Hamburg - Black
    "#FFD700",  # Trefl Sopot - Yellow
    "#FDB927"   # Aris - Yellow/Gold
  )
)

# Merge colors
eurocup <- eurocup %>% left_join(eurocup_colors, by = "team")

# Build the plot
eurocup_plot <- eurocup %>%
  ggplot(aes(x = o_rtg, y = d_rtg)) +  # FIXED HERE
  geom_smooth(method = "lm", color = "gray85", fill = "gray80") +
  geom_point(
    aes(fill = color, color = after_scale(clr_darken(fill, 0.3))),
    shape = 21, alpha = .85, size = 3
  ) +
  geom_text_repel(
    aes(label = team),
    size = 2, color = "black", min.segment.length = unit(0.1, "lines")
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    plot.title = element_text(colour = "black", hjust = .5, face = "bold", size = 15),
    plot.subtitle = element_text(colour = "black", hjust = .5, face = "bold", size = 8),
    plot.caption = element_text(size = 6, hjust = 1, color = "black", face = "italic")
  ) +
  labs(
    title = "Offensive and Defensive Ratings of EuroCup Teams",
    subtitle = "2024–25 Season",
    x = "Offensive Rating",
    y = "Defensive Rating",
    caption = "Data: RealGM | Viz: Can Sahin"
  ) +
  scale_y_reverse()

# Save plot
ggsave("eurocup_team_ratings_25.png", eurocup_plot, height = 6, width = 6, dpi = 300)

# View plot
eurocup_plot
