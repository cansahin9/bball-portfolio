setwd("C:/Users/User/OneDrive/Masaüstü/London Lions/Portfolio/player and team usage chart")

# --- Libraries ---
library(tidyverse)
library(euroleaguer)
library(janitor)
library(scales)
library(ggimage)
library(purrr)
library(stringr)

# --- Custom Theme ---
theme_f5 <- function(font_size = 9) {
  theme_minimal(base_size = font_size, base_family = "Roboto") %+replace%
    theme(
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0, size = 14, face = 'bold'),
      plot.subtitle = element_text(
        color = 'gray65', hjust = 0, margin = margin(2.5, 0, 10, 0), size = 11
      ),
      plot.caption = element_text(
        color = 'gray65', margin = margin(-5, 0, 0, 0), hjust = 1, size = 6
      )
    )
}

# --- 1. Load scraped CSV ---
raw_data <- read.csv("clean_column_names.csv", stringsAsFactors = FALSE)

# --- 2. Lookup table for team names -> official codes ---
team_lookup <- tibble(
  scraped_name = c(
    "Valencia Basket", "Hapoel Tel Aviv", "CB Gran Canaria", "Hapoel Jerusalem BC", 
    "Badalona", "Reyer Venezia", "Besiktas JK", "Aquila Trento", "Turk Ankara", 
    "BC Lietkabelis", "Bahçeşehir Koleji SK", "Wolves Vilnius", "KK Buducnost", 
    "JL Bourg-en-Bresse", "KK Cedevita Junior", "Aris BC", "ratiopharm Ulm", 
    "Trefl Sopot", "Hamburg Towers", "Cluj-Napoca"
  ),
  team_code = c(
    "PAM","HTA","CAN","JER","JOV","VNC","BES","TRN","TTK","LKB",
    "BAH","WOL","BUD","BOU","LJU","ARI","ULM","TSO","HAM","CLU"
  )
)

# --- 3. Merge scraped data with lookup ---
scraped <- raw_data %>%
  left_join(team_lookup, by = c("TEAM" = "scraped_name"))

# --- 4. Fetch team logos/colors from API ---
team_logos <- map_dfr(unique(scraped$team_code), function(code) {
  tryCatch({
    getTeam(team_code = code, season_code = "U2024") %>%
      select(
        team_code = TeamCode,
        team_name = TeamName,
        logo = ImagesCrest,
        primary_color = PrimaryColor,
        secondary_color = SecondaryColor
      ) %>%
      clean_names()
  }, error = function(e) {
    message(paste("Failed for team:", code))
    NULL
  })
})

# Check again
euro_df %>%
  filter(is.na(logo) | logo == "") %>%
  distinct(team_code, TEAM)


# --- 5. Merge logos/colors + clean colors safely ---
euro_df <- scraped %>%
  left_join(team_logos, by = "team_code") %>%
  mutate(
    usg_pct = suppressWarnings(as.numeric(gsub("%", "", USG.)) / 100),   # clean USG%
    logo = ifelse(is.na(logo), "", as.character(logo)),                  # keep empty if missing
    primary_color = ifelse(is.na(primary_color), "#000000", primary_color),
    secondary_color = ifelse(is.na(secondary_color), "#000000", secondary_color),
    primary_color = paste0("#", str_remove_all(primary_color, "^#")),
    secondary_color = paste0("#", str_remove_all(secondary_color, "^#")),
    primary_color = case_when(
      primary_color %in% c("#FFFFFF", "#ffffff") & !(secondary_color %in% c("#FFFFFF", "#ffffff")) ~ secondary_color,
      primary_color %in% c("#FFFFFF", "#ffffff") & secondary_color %in% c("#FFFFFF", "#ffffff") ~ "#000000",
      TRUE ~ primary_color
    )
  ) %>%
  filter(!is.na(usg_pct))   # drop rows without USG%

# --- 6. One logo per team ---
logo_df <- euro_df %>%
  group_by(team_code) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(team_code, logo)

# --- 7. Find max USG player per team ---
euro_df <- euro_df %>%
  group_by(team_code) %>%
  mutate(
    max_usg = max(usg_pct, na.rm = TRUE),
    max_usg_player = case_when(
      usg_pct == max_usg ~ str_remove(PLAYER, "\\s+[A-Z]\\.$"), 
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

# --- 8. Plot ---
p <- euro_df %>%
  ggplot(aes(x = fct_reorder(team_code, -max_usg), y = usg_pct)) +
  geom_point(
    aes(fill = primary_color),
    shape = 21, color = "black", size = 1.5, alpha = 0.666
  ) +
  geom_text(
    data = filter(euro_df, !is.na(max_usg_player)),
    aes(x = team_code, y = max_usg, color = primary_color, label = max_usg_player),
    fontface = 'bold', family = "sans",
    hjust = 0,
    nudge_x = 0.1,
    nudge_y = 0.0075,
    angle = 45,
    size = 1.5
  ) +
  ggimage::geom_image(
    data = logo_df %>% filter(!is.na(logo) & logo != ""),
    aes(x = team_code, y = -0.035, image = logo),
    size = 0.085, inherit.aes = FALSE
  ) +
  scale_y_continuous(
    limits = c(-0.05, 0.35), labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  theme_f5() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "",
    y = "",
    title = "Each EuroCup Team's Most Used Player (2024-25)",
    subtitle = "Teams sorted by their highest usage rate player during the regular season"
  )

# --- 9. Save ---
ggsave("eurocup_usg_leaders_new.png", p, width = 6, height = 3, dpi = 600)
