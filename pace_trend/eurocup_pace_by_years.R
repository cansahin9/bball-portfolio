setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/pace_by_season_eurocup")

# ---------------- Packages ----------------
library(tidyverse)
library(janitor)
library(readr)
library(ggtext)
library(paletteer)
library(scales)
library(cowplot)

# ---------------- Paths ----------------
out_csv     <- "eurocup_pace_by_season_manual.csv"
out_plot    <- "eurocup_pace_by_season_with_logo.png"
plot_title  <- "EuroCup Pace by Season"
logo_path_ec <- "eurocup_logo.png"   # ensure this PNG exists in the working dir

# ---------------- Manually embedded EuroCup pace ----------------
# 19-20: 72.2 | 21: 71.8 | 22: 72.3 | 23: 73.9 | 24: 74.8 | 25: 74.6
pace_df <- tribble(
  ~season,   ~pace,
  "2019-20", 72.2,
  "2020-21", 71.8,
  "2021-22", 72.3,
  "2022-23", 73.9,
  "2023-24", 74.8,
  "2024-25", 74.6
) %>%
  mutate(
    start_year = as.integer(substr(season, 1, 4))
  ) %>%
  arrange(start_year)

# Save tidy CSV
write_csv(pace_df %>% select(season, pace), out_csv)

# ---------------- Plot data formatting ----------------
plot_df <- pace_df %>%
  mutate(
    idx  = row_number(),
    xlab = sprintf("%02d", (start_year + 1) %% 100)  # end-year labels: 20,21,...,25
  )

# y-axis padding
yr  <- range(plot_df$pace, na.rm = TRUE)
pad <- diff(yr) * 0.06
ylims <- if (is.finite(pad) && pad > 0) c(yr[1] - pad, yr[2] + pad) else yr

caption <- "**Data:** hackastat.eu | **Chart:** Can Sahin"

# ---------------- Plot ----------------
p <- ggplot(plot_df, aes(x = idx, y = pace, group = 1)) +
  geom_line(color = "#17408B", linewidth = 1.25) +
  geom_point(fill = "#17408B", color = "#17408B",
             shape = 21, stroke = 0.4, size = 2, show.legend = FALSE) +
  geom_smooth(method = "loess", se = FALSE, linetype = 5,
              color = "gray50", linewidth = 0.8) +
  scale_x_continuous(
    breaks = plot_df$idx, labels = plot_df$xlab,
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(limits = ylims) +
  labs(
    title = plot_title,
    subtitle = paste0(min(plot_df$start_year + 1), " – ",
                      max(plot_df$start_year + 1),
                      " • Average possessions per 40 minutes"),
    x = "Years", y = "Pace", color = "", caption = caption
  ) +
  theme_minimal(base_family = "Oswald") +
  theme(
    plot.background   = element_rect(fill = "oldlace", color = "oldlace"),
    legend.position   = "none",
    plot.title.position = "plot",
    plot.caption      = ggtext::element_markdown(size = 7, hjust = 0),
    plot.title        = element_text(face = "bold", size = 20),
    plot.subtitle     = element_text(face = "italic", size = 10.5),
    plot.margin       = margin(b = 25, t = 25, r = 50, l = 50),
    axis.text         = element_text(size = 7),
    axis.text.x       = element_text(angle = 45, hjust = 1)
  )

# ---------------- Add top-right logo & save ----------------
p_with_logo <- ggdraw(p) +
  draw_image(logo_path_ec,
             x = 0.99, y = 0.98,   # canvas coords (0–1), top-right
             hjust = 1, vjust = 1, # anchor at top-right
             width = 0.14, height = 0.14)

ggsave(out_plot, p_with_logo, width = 10, height = 6, dpi = 300, bg = "oldlace")

print(p_with_logo)
