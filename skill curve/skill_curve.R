# --- Working dir (optional) ---
setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/skill curve")

# --- Packages ---
library(dplyr)
library(janitor)
library(ggplot2)
library(scales)
library(stringr)
library(cowplot)
library(readr)

# --- Simple theme (unchanged) ---
theme_owen <- function () {
  theme_minimal(base_size = 11) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "floralwhite", color = "floralwhite")
    )
}

# --- Load EuroCup CSV for Yogi Ferrell ---
# Expecting a file like: /mnt/data/yogi_ferrell_24_25_ec.csv
# with columns that include TS% (or ts_pct/ts_percent/TS), USG% (or usg/usg_pct), Team name, etc.
raw <- read_csv("yogi_ferrell_24_25_ec.csv", show_col_types = FALSE) |> clean_names()

# --- Flexible column finding helpers ---
pick_col <- function(nm, choices) {
  x <- intersect(choices, nm)
  if (length(x) == 0) NA_character_ else x[1]
}

nm   <- names(raw)

col_team <- pick_col(nm, c("team","tm","tm_tag","tm_name","club","squad"))
col_ts   <- pick_col(nm, c("ts_percent","ts_pct","ts","ts_"))
col_usg  <- pick_col(nm, c("usg_percent","usg_pct","usg"))
col_min  <- pick_col(nm, c("mp","min","minutes","mins"))  # not used for filtering, but can be shown

if (is.na(col_team) || is.na(col_ts) || is.na(col_usg)) {
  stop("Couldn't find the necessary columns. Make sure your CSV has team + TS% + USG% columns.")
}

# --- Standardize & clean ---
df <- raw |>
  transmute(
    Team = .data[[col_team]],
    TS   = suppressWarnings(as.numeric(str_replace(as.character(.data[[col_ts]]), "%", ""))),
    USG  = suppressWarnings(as.numeric(str_replace(as.character(.data[[col_usg]]), "%", ""))),
    MP   = if (!is.na(col_min)) suppressWarnings(as.numeric(.data[[col_min]])) else NA_real_
  ) |>
  mutate(
    # TS may be 0–1 or 0–100; normalize to proportion
    TS  = ifelse(TS > 1, TS/100, TS),
    # USG is typically in percent already; keep as % numerically
    USG = USG
  ) |>
  filter(!is.na(TS), !is.na(USG)) |>
  distinct()

# Optional light sanity trims (comment out if you want everything)
df <- df |> filter(USG >= 0)               # keep all usage (no lower bound)
# df <- df |> filter(USG >= 5)            # <- uncomment if you want a 5% floor like the NBA version

# --- Team colors: Budućnost VOLI ---
team_fill_cols <- c(BUD = "#d76f2c")   # main fill color
team_line_cols <- c(BUD = "#255297")   # outline color

# --- Plot (no MP filter) ---
# --- Plot (fixed Budućnost palette) ---
p <- df |>
  ggplot(aes(x = USG/100, y = TS)) +
  geom_point(aes(fill = Team, color = Team),
             shape = 21, size = 4, alpha = 0.85) +
  geom_smooth(method = "loess", se = TRUE,
              color = "black", linetype = "dashed") +
  theme_owen() +
  theme(legend.position = "top") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0.00, 0.50, 0.05)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0.20, 1.5, 0.1)) +
  scale_fill_manual(name = " ", values = team_fill_cols) +
  scale_color_manual(name = " ", values = team_line_cols) +
  labs(
    x = "Usage Rate",
    y = "True Shooting %",
    title = "Yogi Ferrell — Skill Curve",
    subtitle = "EuroCup games | 2024–2025 (Budućnost VOLI)"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 8),
  )

p <- p + theme(plot.margin = margin(10, 12, 18, 10))

p_final <- ggdraw(p) +
  draw_label("Data: hackastat.eu", x = 0.01, y = 0.01,
             hjust = 0, vjust = 0, size = 8, color = "grey30") +
  draw_label("Plot: Can Sahin", x = 0.99, y = 0.01,
             hjust = 1, vjust = 0, size = 8, color = "grey30")

# --- Save ---
ggsave("Yogi Ferrell Skill Curve — EuroCup 2024-25.png", p_final, width = 6, height = 6, dpi = 300)

