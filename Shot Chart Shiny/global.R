library(shiny)
library(ggplot2)
library(dplyr)
library(euroleaguer)
library(readr)
library(hexbin)

source("court.R")
source("helpers.R")
source("fetch_data.R")
source("hex_chart.R")  # Copy from BallR into your project


# --- STEP 1: Format function ---
format_season_label <- function(season_code) {
  year <- as.numeric(gsub("E", "", season_code))
  paste0(year, "-", year + 1)
}

# --- STEP 2: Define seasons + labels ---
seasons <- c("E2023", "E2024")  # Add more here as needed
season_labels <- setNames(seasons, sapply(seasons, format_season_label))

# --- Load CSVs ---
games_df <- readr::read_csv("data/games.csv")
players_df <- readr::read_csv("data/players.csv")


