setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/skill curve")

# --- Packages ---
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(scales)
library(stringr)
library(cowplot)
library(purrr)

# --- Simple theme ---
theme_owen <- function () {
  theme_minimal(base_size = 11) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "floralwhite", color = "floralwhite")
    )
}

# ---------- Helpers ----------
pick_col <- function(nm, choices) {
  x <- intersect(choices, nm)
  if (length(x) == 0) NA_character_ else x[1]
}

clean_one_ec <- function(path, season_label) {
  raw <- read_csv(path, show_col_types = FALSE) |> clean_names()
  nm  <- names(raw)
  
  col_team <- pick_col(nm, c("team","tm","tm_tag","tm_name","club","squad"))
  col_ts   <- pick_col(nm, c("ts_percent","ts_pct","ts","ts_"))
  col_usg  <- pick_col(nm, c("usg_percent","usg_pct","usg"))
  col_min  <- pick_col(nm, c("mp","min","minutes","mins"))
  
  if (is.na(col_team) || is.na(col_ts) || is.na(col_usg)) {
    stop(paste0("Missing columns in ", basename(path),
                " (need team + TS% + USG%)"))
  }
  
  df <- raw |>
    transmute(
      Season = season_label,
      Team   = .data[[col_team]],
      TS     = suppressWarnings(as.numeric(str_replace(as.character(.data[[col_ts]]), "%", ""))),
      USG    = suppressWarnings(as.numeric(str_replace(as.character(.data[[col_usg]]), "%", ""))),
      MP     = if (!is.na(col_min)) suppressWarnings(as.numeric(.data[[col_min]])) else NA_real_
    ) |>
    mutate(
      TS  = ifelse(TS > 1, TS/100, TS),   # normalize to proportion
      USG = USG                           # keep as percent
    ) |>
    filter(!is.na(TS), !is.na(USG))
  df
}

# ---------- Load multiple seasons (name = season, value = file path) ----------
files <- list(
  "2021–22" = "yogi_ferrell_21_22_ec.csv",
  "2022–23" = "yogi_ferrell_22_23_ec.csv",
  "2023–24" = "yogi_ferrell_23_24_ec.csv",
  "2024–25" = "yogi_ferrell_24_25_ec.csv" 
)

# Now .x = path, .y = season label
df <- imap_dfr(files, ~ clean_one_ec(.x, .y))

# --- 1) Standardize team names (so legend & color mapping align) ---
df <- df %>%
  mutate(
    Team = str_trim(Team),
    Team = case_when(
      # Budućnost variants (EuroCup short code "BUD", full names, diacritics, etc.)
      str_detect(Team, regex("^BUD$|budu|buduc", ignore_case = TRUE)) ~ "Budućnost VOLI",
      # Cedevita variants
      str_detect(Team, regex("cedevita", ignore_case = TRUE)) ~ "Cedevita Olimpija",
      TRUE ~ Team
    )
  )
# --- 2) Palette with exact hex codes for the standardized names ---
bud_fill <- "#255297"; bud_line <- "#d76f2c"   # Budućnost VOLI
ced_fill <- "#008000"; ced_line <- "#F26E21"   # Cedevita Olimpija

teams <- sort(unique(df$Team))

# Start with defaults for all teams
pal_fill <- setNames(scales::hue_pal(l = 80)(length(teams)), teams)
pal_line <- setNames(scales::hue_pal(l = 30)(length(teams)), teams)

# Overwrite for our two clubs (names must match the standardized labels above)
if ("Budućnost VOLI" %in% teams) {
  pal_fill["Budućnost VOLI"] <- bud_fill
  pal_line["Budućnost VOLI"] <- bud_line
}
if ("Cedevita Olimpija" %in% teams) {
  pal_fill["Cedevita Olimpija"] <- ced_fill
  pal_line["Cedevita Olimpija"] <- ced_line
}

# ---------- Plot (no minute filter) ----------
p <- df |>
  ggplot(aes(x = USG/100, y = TS)) +
  geom_point(aes(fill = Team, color = Team), shape = 21, size = 4, alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  theme_owen() +
  theme(legend.position = "top") +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0.00, 0.50, 0.05),
    limits = c(
      max(0, (min(df$USG, na.rm=TRUE)/100) - 0.02),
      min(0.50, (max(df$USG, na.rm=TRUE)/100) + 0.02)
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0.20, 1.5, 0.1)) +
  scale_fill_manual(name = "Team", values = pal_fill) +
  scale_color_manual(name = "Team", values = pal_line) +
  labs(
    x = "Usage Rate",
    y = "True Shooting %",
    title = "Yogi Ferrell — Skill Curve",
    subtitle = "All EuroCup Games 2021-24 "
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 8)
  )

p <- p + theme(plot.margin = margin(10, 12, 18, 10))

p_final <- ggdraw(p) +
  draw_label("Data: hackastat.eu", x = 0.01, y = 0.01,
             hjust = 0, vjust = 0, size = 8, color = "grey30") +
  draw_label("Plot: Can Sahin", x = 0.99, y = 0.01,
             hjust = 1, vjust = 0, size = 8, color = "grey30")

ggsave("Yogi Ferrell Skill Curve — EuroCup 2021-24.png",
       p_final, width = 6, height = 6, dpi = 300)
