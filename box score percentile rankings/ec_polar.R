setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/player profile dashboards")

# 📦 Load required libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(geomtextpath)

# 📥 Load data
raw <- read_csv("RASHEED SULAIMON.csv")

# 🔁 Rename percentile columns based on previous stat column (safe and unique)
col_names <- colnames(raw)
for (i in seq_along(col_names)) {
  if (grepl("^PERCENTILE", col_names[i])) {
    prev_col <- col_names[i - 1]
    clean_name <- prev_col %>%
      gsub("%", "P", .) %>%
      gsub("[^A-Za-z0-9]", "", .)  # Remove special characters
    col_names[i] <- paste0(clean_name, "_PCTL")
  }
}
colnames(raw) <- col_names

# ✅ Select 2024-25 player row
player_row <- raw %>%
  filter(SEASON == "2024-2025")

# 🎯 Define the 12 key metrics
metrics <- tribble(
  ~stat,                ~column,                ~group,
  "Points Per Poss.",    "PPP_PCTL",             "Scoring",
  "2 Point %",          "2PP_PCTL",             "Scoring",
  "3 Point %",          "3PP_PCTL",             "Scoring",
  "Free Throw Rate",    "FTFREQ_PCTL",          "Scoring",
  
  "Usage %",            "USGP_PCTL",            "Possession",
  "Assist %",           "ASTP_PCTL",            "Possession",
  "Ball Security",      "TOVP_PCTL",            "Possession",  # Will invert below
  "Off. Rebounding %",  "ORBP_PCTL",            "Possession",
  
  "Def. Rebounding %",  "DRBP_PCTL",            "Defending",
  "Block %",            "BLKP_PCTL",            "Defending",
  "Steal %",            "STP_PCTL",             "Defending"
)

# 🔢 Build the polar plot data
plot_data <- metrics %>%
  mutate(
    value = as.numeric(player_row[1, column]),
    index = row_number()
  )


color_map <- c(
  "Scoring" = "#003C71",       # Dark blue
  "Possession" = "gray",    # Red
  "Defending" = "#b95d00"      # Olive brown
)


ggplot(plot_data, aes(x = fct_inorder(stat), y = value, fill = group)) +
  # Outer background layer
  geom_bar(aes(y = 100), stat = "identity", width = 1, alpha = 0.5, colour = "white") +
  # Actual values (with spacing)
  geom_bar(stat = "identity", width = 1, colour = "white", linewidth = 0.5) +
  geom_label(aes(label = round(value)), fill = "gray25", colour = "white",
             size = 4, family = "Sans", fontface = "bold", show.legend = FALSE) +
  coord_curvedpolar() +   # ✅ keep only ONE coord system
  scale_fill_manual(values = color_map) +
  theme_void(base_family = "sans") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold", colour = "gray20"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, colour = "gray20"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", size = 10, colour = "gray20"),
    axis.text.x = element_text(color = "gray20", face = "bold", size = 10,
                               margin = margin(t = -5)),
    # Backgrounds
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.caption = element_text(hjust = 1, size = 6, colour = "gray20", face = "bold")
  ) +
  labs(
    title = "Rasheed Sulaimon",
    subtitle = "EuroCup 24–25 Season Box Score Percentile Rankings",
    caption = "100 = Best  0 = Worst\nAmongst all qualifying guards\n"
  )

# 💾 Save output
ggsave("Rasheed_Sulaimon_2024_25.png", width = 7, height = 7, dpi = 300, bg = "white")

