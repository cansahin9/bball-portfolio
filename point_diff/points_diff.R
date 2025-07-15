# Load necessary libraries
library(tidyverse)
library(rvest)
library(janitor)
library(glue)
library(paletteer)
library(ggtext)


# Caption
caption <- glue("**Data**: Proballers.com  |  **Chart**: Can Sahin")

# Define the year range for SLB (based on what you observed)
years <- 2008:2024

#-------------------------------
# Regular Season Scraper
#-------------------------------
differential_regular <- function(year) {
  Sys.sleep(1)  # delay to prevent connection overload
  
  url <- paste0("https://www.proballers.com/basketball/league/120/super-league-basketball/schedule/", year)
  
  df <- url %>%
    read_html() %>%
    html_element("table") %>%
    html_table() %>%
    clean_names() %>%
    transmute(year = year, result) %>%
    filter(result != "Game preview") %>%
    separate(result, into = c("a", "b"), sep = "-") %>%
    mutate(across(c(a, b), as.numeric)) %>%
    mutate(dif = abs(a - b)) %>%
    group_by(year) %>%
    summarise(dif = mean(dif), .groups = "drop")
  
  return(df)
}

safe_diff_reg <- safely(differential_regular, otherwise = NULL)
regular_df <- map(years, safe_diff_reg) %>%
  map("result") %>%
  compact() %>%
  bind_rows()

#-------------------------------
# Playoffs Scraper
#-------------------------------
differential_po <- function(year) {
  Sys.sleep(1)  # delay
  
  url <- paste0("https://www.proballers.com/basketball/league/259/super-league-basketball-playoffs/schedule/", year)
  
  df <- url %>%
    read_html() %>%
    html_element("table") %>%
    html_table() %>%
    clean_names() %>%
    transmute(year = year, result) %>%
    filter(result != "Game preview") %>%
    separate(result, into = c("a", "b"), sep = "-") %>%
    mutate(across(c(a, b), as.numeric)) %>%
    mutate(dif_po = abs(a - b)) %>%
    group_by(year) %>%
    summarise(dif_po = mean(dif_po), .groups = "drop")
  
  return(df)
}

safe_diff_po <- safely(differential_po, otherwise = NULL)
po_df <- map(years, safe_diff_po) %>%
  map("result") %>%
  compact() %>%
  bind_rows()

#-------------------------------
# Combine Data
#-------------------------------
dat <- regular_df %>%
  left_join(po_df, by = "year") %>%
  pivot_longer(cols = c(dif, dif_po), names_to = "type", values_to = "value")

# Confirm preview
head(dat)


# 📈 Plot --------------------------------------------------------------------

p <- dat %>%
  ggplot(aes(x = year, y = value, group = type)) +
  geom_line(aes(color = type), linewidth = 1.25) +
  geom_point(aes(fill = type), shape = 21, show.legend = FALSE) +
  annotate("text", x = 2019, y = 11.5, label = "No Playoffs (COVID)", size = 2.0, hjust = 0.5) +
  scale_x_continuous(
    breaks = years,
    labels = sprintf("%02d", (years + 1) %% 100)
  ) +
  scale_color_paletteer_d("ggsci::nrc_npg", direction = 1, labels = c("Regular Season", "Playoffs")) +
  theme_minimal(base_family = "Oswald") +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    legend.position = "top",
    plot.title.position = "plot",
    plot.caption = element_markdown(size = 7, hjust = 0),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(family = "", face = "italic", size = 10.5),
    plot.margin = margin(b = 25, t = 25, r = 50, l = 50),
    axis.text = element_text(size = 7)
  ) +
  labs(
    color = "",
    x = "Years",
    y = "Point Differential",
    title = "Point Differential: Regular Season vs Playoffs",
    subtitle = "2008 – 2025 • Super League Basketball (UK)",
    caption = caption
  )

ggsave("super_league_basketball_point_differential_line_chart.png", p, width = 6 * 1.618, height = 6, dpi = 600)
