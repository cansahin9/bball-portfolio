library(euroleaguer)
library(dplyr)
library(purrr)

load_all_games <- function(seasons) {
  purrr::map_dfr(seasons, function(season) {
    rounds <- tryCatch(getCompetitionRounds(season_code = season), error = function(e) NULL)
    if (is.null(rounds)) return(NULL)
    
    purrr::map_dfr(rounds$Round, function(round_num) {
      tryCatch({
        df <- getCompetitionGames(season_code = season, round = round_num)
        df$season_code <- season
        df
      }, error = function(e) NULL)
    })
  })
}

extract_all_players <- function(games_df) {
  failed_games <- c()
  
  raw_players <- purrr::map_dfr(1:nrow(games_df), function(i) {
    game <- games_df[i, ]
    season_code <- game$season_code[[1]]
    game_code <- game$GameCode[[1]]
    
    tryCatch({
      gp <- getGamePoints(season_code = season_code, game_code = game_code)
      if (!"Player" %in% names(gp)) {
        failed_games <<- c(failed_games, paste(season_code, game_code))
        return(NULL)
      }
      
      gp %>%
        dplyr::select(Player) %>%
        distinct() %>%
        mutate(season_code = season_code)
      
    }, error = function(e) {
      failed_games <<- c(failed_games, paste(season_code, game_code))
      return(NULL)
    })
  })
  
  if (nrow(raw_players) == 0 || !"Player" %in% names(raw_players)) {
    warning("No valid player data found.")
    return(tibble(Player = character(), season_code = character()))
  } else {
    return(distinct(raw_players, Player, season_code))
  }
}

