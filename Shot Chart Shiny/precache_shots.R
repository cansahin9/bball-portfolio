
library(dplyr)
library(purrr)
library(readr)
library(euroleaguer)

games_df <- readr::read_csv("data/games.csv")
players_df <- read_csv("data/players.csv")

all_shots <- purrr::map_dfr(1:nrow(players_df), function(i) {
  player <- players_df$Player[i]
  season_code <- players_df$season_code[i]
  
  cat("Fetching:", player, season_code, "\n")
  
  tryCatch({
    games <- games_df %>% filter(season_code == !!season_code)
    
    purrr::map_dfr(games$GameCode, function(game_code) {
      game_data <- getGamePoints(season_code, game_code)
      
      game_data %>%
        filter(grepl(player, Player, ignore.case = TRUE)) %>%
        filter(!Action_ID %in% c("FTM", "FTA")) %>%
        mutate(
          Player = player,
          season_code = season_code,
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
    })
    
  }, error = function(e) NULL)
})

saveRDS(all_shots, "data/shot_data.rds")


