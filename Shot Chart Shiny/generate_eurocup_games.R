library(readr)
library(dplyr)
library(euroleaguer)

# Define EuroCup seasons
eurocup_seasons <- c("U2023", "U2024")

# Fetch all EuroCup games
eurocup_games <- purrr::map_dfr(eurocup_seasons, function(season) {
  rounds <- tryCatch(getCompetitionRounds(season), error = function(e) NULL)
  if (is.null(rounds)) return(NULL)
  
  purrr::map_dfr(rounds$Round, function(round_num) {
    tryCatch({
      df <- getCompetitionGames(season, round_num)
      df$season_code <- season
      df
    }, error = function(e) NULL)
  })
})

# Save the file if data exists
if (nrow(eurocup_games) == 0) {
  stop("❌ No EuroCup games found.")
} else {
  write_csv(eurocup_games, "data/eurocup_games.csv")
  message("✅ eurocup_games.csv created with ", nrow(eurocup_games), " rows.")
}
