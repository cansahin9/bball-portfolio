setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/player profile dashboards/skill curve")

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
      plot.background  = element_rect(fill = "white", color = "white")
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
  "2024–25" = "Ante ZIZIC.csv"
)

# Now .x = path, .y = season label
df <- imap_dfr(files, ~ clean_one_ec(.x, .y))

df_filtered <- df %>%
  filter(
    is.na(MP) | MP >= 10,
    TS >= 0.20,
    TS <= 1.10  # <- Add this to clip high-TS games
  )

# --- 1) Standardize team names (so legend & color mapping align) ---
df <- df_filtered %>%
  mutate(
    Team = str_trim(Team),
    Team = case_when(
      # Budućnost variants (EuroCup short code "BUD", full names, diacritics, etc.)
      str_detect(Team, regex("^BUD$|budu|buduc", ignore_case = TRUE)) ~ "Budućnost VOLI",
      # Cedevita variants
      str_detect(Team, regex("cedevita", ignore_case = TRUE)) ~ "Cedevita Olimpija",
      # Turk Telekom Ankara variants
      str_detect(Team, regex("turk|telekom|ankara", ignore_case = TRUE)) ~
        "Türk Telekom Ankara",
      # Bahçeşehir Koleji Istanbul variants
      str_detect(Team, regex("bahc|koleji", ignore_case = TRUE)) ~
        "Bahçeşehir Koleji Istanbul",
      # JL Bourg-en-Bresse variants
      str_detect(Team, regex("bourg|jl b", ignore_case = TRUE)) ~
        "JL Bourg-en-Bresse",
      # Wolves Vilnius variants
      str_detect(Team, regex("wolves|vilnius", ignore_case = TRUE)) ~
        "Wolves Vilnius",
      # Joventut Badalona variants
      str_detect(Team, regex("joventut|badalona|penya", ignore_case = TRUE)) ~
        "Joventut Badalona",
      # Bursaspor variants
      str_detect(Team, regex("bursa|bursaspor", ignore_case = TRUE)) ~
        "Bursaspor",
      # London Lions variants
      str_detect(Team, regex("london|lions", ignore_case = TRUE)) ~
        "London Lions",
      # Andorra variants
      str_detect(Team, regex("andorra|mora", ignore_case = TRUE)) ~
        "MoraBanc Andorra",
      # Slask Wroclaw variants
      str_detect(Team, regex("slask|wroclaw", ignore_case = TRUE)) ~
        "Śląsk Wrocław",
      # AS Monaco variants
      str_detect(Team, regex("monaco", ignore_case = TRUE)) ~
        "AS Monaco",
      # Olimpia Milano variants
      str_detect(Team, regex("milano|olimpia", ignore_case = TRUE)) ~
        "Olimpia Milano",
      # Paris Basketball variants
      str_detect(Team, regex("paris|pb", ignore_case = TRUE)) ~
        "Paris Basketball",
      # Macabi Tel Aviv variants
      str_detect(Team, regex("maccabi|macabi|tel aviv", ignore_case = TRUE)) ~
        "Maccabi Tel Aviv",
      # Virtus Bologna variants
      str_detect(Team, regex("virtus|bologna|vbo", ignore_case = TRUE)) ~
        "Virtus Bologna",
      # Anadolu Efes variants
      str_detect(Team, regex("anadolu|efes|ae", ignore_case = TRUE)) ~
        "Anadolu Efes Istanbul",
      # Besiktaş variants
      str_detect(Team, regex("besiktas|besi|bjk", ignore_case = TRUE)) ~
        "Beşiktaş",
      TRUE ~ Team
    )
  )
# --- 2) Palette with exact hex codes for the standardized names ---
bud_fill <- "#003C71"; bud_line <- "#FFFFFF"   # Budućnost VOLI
ced_fill <- "#007941"; ced_line <- "#F26739"   # Cedevita Olimpija
bou_fill <- "#DA291C"; bou_line <- "#FFFFFF"   # JL Bourg-en-Bresse
wol_fill <- "#52d1bc"; wol_line <- "#000000"   # Wolves Vilnius
ttk_fill <- "#49BED8"; ttk_line <- "#FFFFFF"   # Türk Telekom Ankara
bah_fill <- "#000A6E"; bah_line <- "#ED1C24"   # Bah
bes_fill <- "#000000"; bes_line <- "red"   # Beşiktaş
jov_fill <- "#005440"; jov_line <- "#1D1D1B"   # Joventut Badalona
bbu_fill <- "#00904A"; bbu_line <- "#F2E205"   # Bursaspor
lli_fill <- "#FFFFFF"; lli_line <- "#000000"   # London Lions
anr_fill <- "#003882"; anr_line <- "#9ECDE8"   # MoraBanc Andorra
wro_fill <- "#538A59"; wro_line <- "#D2AA53"   # Śląsk Wrocław
mco_fill <- "#E2211C"; mco_line <- "#84754D"   # AS Monaco
mil_fill <- "#E1261D"; mil_line <- "#1D1D1B"   # Olimpia Milano
prs_fill <- "#8074f7"; prs_line <- "#000000"   # Paris Basketball
tel_fill <- "#FFF100"; mta_line <- "#2371B5"   # Maccabi Tel Aviv
vir_fill <- "#1D1D1B"; vir_line <- "gray" # Virtus Bologna
ist_fill <- "#002D74"; ist_line <- "#00A7E1"   # Anadolu Efes Istanbul

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
if ("JL Bourg-en-Bresse" %in% teams) {
  pal_fill["JL Bourg-en-Bresse"] <- bou_fill
  pal_line["JL Bourg-en-Bresse"] <- bou_line
}
if ("Wolves Vilnius" %in% teams) {
  pal_fill["Wolves Vilnius"] <- wol_fill
  pal_line["Wolves Vilnius"] <- wol_line
}
if ("Türk Telekom Ankara" %in% teams) {
  pal_fill["Türk Telekom Ankara"] <- ttk_fill
  pal_line["Türk Telekom Ankara"] <- ttk_line
}
if ("Bahçeşehir Koleji Istanbul" %in% teams) {
  pal_fill["Bahçeşehir Koleji Istanbul"] <- bah_fill
  pal_line["Bahçeşehir Koleji Istanbul"] <- bah_line
}
if ("Beşiktaş" %in% teams) {
  pal_fill["Beşiktaş"] <- bes_fill
  pal_line["Beşiktaş"] <- bes_line
}
if ("Joventut Badalona" %in% teams) {
  pal_fill["Joventut Badalona"] <- jov_fill
  pal_line["Joventut Badalona"] <- jov_line
}
if ("Bursaspor" %in% teams) {
  pal_fill["Bursaspor"] <- bbu_fill
  pal_line["Bursaspor"] <- bbu_line
}
if ("London Lions" %in% teams) {
  pal_fill["London Lions"] <- lli_fill
  pal_line["London Lions"] <- lli_line
}
if ("MoraBanc Andorra" %in% teams) {
  pal_fill["MoraBanc Andorra"] <- anr_fill
  pal_line["MoraBanc Andorra"] <- anr_line
}
if ("Śląsk Wrocław" %in% teams) {
  pal_fill["Śląsk Wrocław"] <- wro_fill
  pal_line["Śląsk Wrocław"] <- wro_line
}
if ("AS Monaco" %in% teams) {
  pal_fill["AS Monaco"] <- mco_fill
  pal_line["AS Monaco"] <- mco_line
}
if ("Olimpia Milano" %in% teams) {
  pal_fill["Olimpia Milano"] <- mil_fill
  pal_line["Olimpia Milano"] <- mil_line
}
if ("Paris Basketball" %in% teams) {
  pal_fill["Paris Basketball"] <- prs_fill
  pal_line["Paris Basketball"] <- prs_line
}
if ("Maccabi Tel Aviv" %in% teams) {
  pal_fill["Maccabi Tel Aviv"] <- tel_fill
  pal_line["Maccabi Tel Aviv"] <- mta_line
}
if ("Virtus Bologna" %in% teams) {
  pal_fill["Virtus Bologna"] <- vir_fill
  pal_line["Virtus Bologna"] <- vir_line
}
if ("Anadolu Efes Istanbul" %in% teams) {
  pal_fill["Anadolu Efes Istanbul"] <- ist_fill
  pal_line["Anadolu Efes Istanbul"] <- ist_line
}

# ---------- Plot (no minute filter) ----------
p <- df |>
  ggplot(aes(x = USG/100, y = TS)) +
  geom_point(aes(fill = Team, color = Team), shape = 21, size = 4, alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  theme_owen() +
  theme(legend.position = "top") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0.1, 0.40, 0.05),
                     limits = c(0.05, 0.35)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0.20, 1.5, 0.1)) +
  scale_fill_manual(name = "Team", values = pal_fill) +
  scale_color_manual(name = "Team", values = pal_line) +
  labs(
    x = "Usage Rate",
    y = "True Shooting %",
    title = "Ante ZIZIC — Skill Curve",
    subtitle = "All EuroCup & EuroLeague Games 2021-25 "
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 8)
  )

p <- p + theme(plot.margin = margin(10, 12, 18, 10))

ggsave("Ante ZIZIC Skill Curve — EuroCup & EL 2021-25.png",
       p, width = 6, height = 6, dpi = 300)
