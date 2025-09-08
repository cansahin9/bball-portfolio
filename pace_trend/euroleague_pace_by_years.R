setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/pace_by_season_eurocup")

# ---------------- Packages ----------------
library(tidyverse)
library(janitor)
library(readr)
library(ggtext)
library(paletteer)
library(scales)
library(rlang)
library(stringr)
library(purrr)
library(cowplot)

# ---------------- Paths ----------------
input_file <- "euroleague_league_avg_all_time.csv"   # <- change if needed
out_csv    <- "euroleague_pace_by_season_clean.csv"
out_plot   <- "euroleague_pace_by_season.png"
plot_title <- "EuroLeague Pace by Season" 
logo_path_el <- "euroleague_logo.png"
logo_path_ec <- "eurocup_logo.png" # change to "EuroCup Pace by Season" if EuroCup file

# ---------------- Read and promote header robustly ----------------
# read with no names
raw0 <- read_csv(input_file, col_names = FALSE, show_col_types = FALSE, na = c("", "NA", "NaN"))

# find the first row that looks like a header (contains "SEASON" and/or "PACE")
header_row_idx <- which(apply(raw0, 1, function(r) {
  any(str_detect(toupper(as.character(r)), "\\bSEASON\\b")) ||
    any(str_detect(toupper(as.character(r)), "\\bPACE\\b"))
}))[1]
if (is.na(header_row_idx)) header_row_idx <- 1

header <- raw0 %>% slice(header_row_idx) %>% as.list() %>% unlist(use.names = FALSE)
df     <- raw0 %>% slice(-(1:header_row_idx))
names(df) <- make_clean_names(header)  # lower snake_case, spaces -> underscores

# ---------------- Find season and pace columns (name OR content) ----------------
# Try by name first
season_col <- names(df)[str_detect(names(df), regex("^season\\b", ignore_case = TRUE))][1]
pace_col   <- names(df)[str_detect(names(df), regex("^pace\\b",   ignore_case = TRUE))][1]

# Fallback by content:
if (is.na(season_col) || is.null(season_col)) {
  # Column where most rows look like "2000-2001" or "2000–01"
  season_pattern <- "^\\s*20\\d{2}\\s*[-–]\\s*(?:\\d{2}|20\\d{2})\\s*$"
  season_col <- names(df)[map_lgl(df, function(x) {
    v <- as.character(x)
    mean(grepl(season_pattern, v)) > 0.5
  })][1]
}

if (is.na(pace_col) || is.null(pace_col)) {
  # Column with numeric values in a plausible pace range (55–90)
  pace_col <- names(df)[map_lgl(df, function(x) {
    nums <- suppressWarnings(parse_number(as.character(x)))
    rng  <- range(nums, na.rm = TRUE)
    is.finite(rng[1]) && is.finite(rng[2]) && rng[1] >= 55 && rng[2] <= 90
  })][1]
}

if (is.na(season_col) || is.na(pace_col)) {
  cat("Available column names:\n"); print(names(df))
  stop("Could not detect SEASON and/or PACE columns automatically. See names printed above.")
}

# ---------------- Clean & order ----------------
pace_df <- df %>%
  transmute(
    season = as.character(.data[[season_col]]),
    pace   = parse_number(.data[[pace_col]])
  ) %>%
  filter(!is.na(season), !is.na(pace), season != "") %>%
  mutate(
    start_year = suppressWarnings(as.integer(substr(season, 1, 4)))
  ) %>%
  arrange(start_year) %>%
  distinct(season, .keep_all = TRUE) %>%   # if a totals row repeats, keep first
  select(season, pace, start_year)

# Save tidy CSV
write_csv(pace_df %>% select(season, pace), out_csv)

# ---------------- Plot ----------------
plot_df <- pace_df %>%
  mutate(idx = row_number(),
         xlab = sprintf("%02d", (start_year + 1) %% 100))

yr <- range(plot_df$pace, na.rm = TRUE)
pad <- diff(yr) * 0.06
ylims <- if (is.finite(pad)) c(yr[1] - pad, yr[2] + pad) else yr

caption <- "**Data:** hackastat.eu | **Chart:** Can Sahin"

p <- ggplot(plot_df, aes(x = idx, y = pace, group = 1)) +
  geom_line(aes(color = "League"), linewidth = 1.25) +
  geom_point(aes(fill = "League"), shape = 21, stroke = 0.4, size = 2, show.legend = FALSE) +
  geom_smooth(method = "loess", se = FALSE, linetype = 5, color = "gray50", linewidth = 0.8) +
  scale_x_continuous(breaks = plot_df$idx, labels = plot_df$xlab,
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = ylims) +
  scale_color_paletteer_d("ggsci::nrc_npg", direction = 1, labels = "League") +
  scale_fill_paletteer_d("ggsci::nrc_npg", direction = 1) +
  labs(
    title = plot_title,
    subtitle = paste0(min(plot_df$start_year + 1, na.rm = TRUE), " – ",
                      max(plot_df$start_year + 1, na.rm = TRUE),
                      " • Average possessions per 40 minutes"),
    x = "Years", y = "Pace", color = "", caption = caption
  ) +
  theme_minimal(base_family = "Oswald") +
  theme(
    plot.background   = element_rect(fill = "oldlace", color = "oldlace"),
    legend.position   = "none",
    plot.title.position = "plot",
    plot.caption      = element_markdown(size = 7, hjust = 0),
    plot.title        = element_text(face = "bold", size = 20),
    plot.subtitle     = element_text(face = "italic", size = 10.5),
    plot.margin       = margin(b = 25, t = 25, r = 50, l = 50),
    axis.text         = element_text(size = 7),
    axis.text.x       = element_text(angle = 45, hjust = 1)
  )
p_with_logo <- ggdraw(p) +
  draw_image(logo_path_el,
             x = 0.99,  y = 0.98,   # position (0–1), top-right
             hjust = 1, vjust = 1,  # anchor at top-right corner
             width = 0.14, height = 0.14)  # adjust size

ggsave("euroleague_pace_by_season_with_logo.png",
       p_with_logo, width = 10, height = 6, dpi = 300, bg = "oldlace")

print(p)






