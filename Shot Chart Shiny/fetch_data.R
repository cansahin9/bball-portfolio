library(dplyr)
library(purrr)
library(readr)
library(euroleaguer)
library(memoise)


# Disk-based cache directory
cache_dir <- "data/cache"
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# Create disk cache
cache <- cache_filesystem(cache_dir)

# Memoised shot data function
get_shot_data <- memoise(function(player_name, season_code = "E2024") {
  games <- readr::read_csv("data/games.csv") %>%
    filter(season_code == !!season_code)
  
  purrr::map_dfr(games$GameCode, function(game_code) {
    tryCatch({
      game_data <- getGamePoints(season_code = season_code, game_code = game_code)
      
      game_data %>%
        filter(grepl(player_name, Player, ignore.case = TRUE)) %>%
        filter(!Action_ID %in% c("FTM", "FTA")) %>%
        mutate(
          x = ((CoordX * 0.55)) * 0.0625,
          y = ((CoordY * 0.45) + 58) * 0.0783,
          loc_x = x,
          loc_y = y,
          shot_made_numeric = ifelse(Action_ID %in% c("2FGM", "3FGM"), 1, 0),
          isShotMade = ifelse(Action_ID %in% c("2FGM", "3FGM"), TRUE, FALSE),
          shot_value = ifelse(Action_ID == "3FGM", 3, 2),
          shot_zone_range = case_when(
            y < 8 ~ "Less Than 8 ft.",
            y < 16 ~ "8-16 ft.",
            y < 24 ~ "16-24 ft.",
            TRUE ~ "24+ ft."
          ),
          shot_zone_area = case_when(
            x < -11 ~ "Left Side(L)",
            x < -3 ~ "Left Side Center(LC)",
            x < 3 ~ "Center(C)",
            x < 11 ~ "Right Side Center(RC)",
            TRUE ~ "Right Side(R)"
          )
        )
    }, error = function(e) NULL)
  })
}, cache = cache)
