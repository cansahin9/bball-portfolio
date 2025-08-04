library(readr)
library(dplyr)
library(purrr)
library(euroleaguer)

# Read only EuroCup games
games_df <- read_csv("data/eurocup_games.csv", show_col_types = FALSE)

if (nrow(games_df) == 0) {
  stop("❌ No EuroCup games found in eurocup_games.csv")
}

# Extract player names
extract_players <- function(season_code, game_code) {
  tryCatch({
    df <- getGamePoints(season_code, game_code)
    if (!"Player" %in% names(df)) return(NULL)
    
    df %>%
      filter(!is.na(Player)) %>%
      distinct(Player) %>%
      mutate(season_code = season_code)
  }, error = function(e) NULL)
}

# Iterate over all games
eurocup_players_df <- map_dfr(1:nrow(games_df), function(i) {
  extract_players(games_df$season_code[i], games_df$GameCode[i])
})

if (nrow(eurocup_players_df) == 0) {
  stop("❌ No players found in EuroCup games.")
} else {
  eurocup_players_df <- eurocup_players_df %>%
    distinct(Player, season_code) %>%
    arrange(season_code, Player)
  
  write_csv(eurocup_players_df, "data/eurocup_players.csv")
  message("✅ eurocup_players.csv created with ", nrow(eurocup_players_df), " rows.")
}

