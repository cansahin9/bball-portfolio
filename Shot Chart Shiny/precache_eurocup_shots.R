library(dplyr)
library(purrr)
library(readr)
library(euroleaguer)

# Load EuroCup games and players
games <- read_csv("data/eurocup_games.csv", show_col_types = FALSE)
players <- read_csv("data/eurocup_players.csv", show_col_types = FALSE)

# Function to fetch shot data safely
get_player_shots <- function(player_name, season_code) {
  message("Fetching: ", player_name, " ", season_code)
  
  game_codes <- games %>%
    filter(season_code == !!season_code) %>%
    pull(GameCode)
  
  map_dfr(game_codes, function(game_code) {
    tryCatch({
      df <- getGamePoints(season_code, game_code)
      df %>%
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
}

# Fetch and combine all players
all_eurocup_shots <- map_dfr(1:nrow(players), function(i) {
  get_player_shots(players$Player[i], players$season_code[i])
})
# After combining all shots
all_eurocup_shots <- all_eurocup_shots %>%
  mutate(season_code = SeasonCode)  # ✅ Add this line


# Save to RDS
saveRDS(all_eurocup_shots, "data/eurocup_shot_data.rds")
message("✅ eurocup_shot_data.rds created with ", nrow(all_eurocup_shots), " rows.")
