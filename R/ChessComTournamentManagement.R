##############################################
###         Required Imports              ####
### Must be installed to use this package ####
##############################################

require(jsonlite)
require(dplyr)

#########################
### Player Management ###
#########################

#' @description Returns players ineligible to participate in the tournament based on the given criteria
#' @param tournament_id ID of the tournament
#' @param max_timeout The maximum allowed timeout percentage required to participate
#' @param max_move_speed The maximum time per move required to participate (in hours)
#' @return Data frame of ineligible players
#' @export
getIneligiblePlayers() <- function(tournament_id, max_timeout, max_move_speed) {
  print(paste0("Fetching registered tournament players for tournament ", tournament_id))
  baseUrl <- "https://api.chess.com/pub/tournament/"
  endpoint <- paste0(baseUrl, tournament_id, sep = "", collapse = NULL)

  tournament_details_raw <- try(fromJSON(toString(endpoint), flatten = TRUE))

  if(class(tournament_details_raw) == "try-error") {
    stop("Error: tournament cannot be found")
  }

  # Extract the registered players
  registered_players <- tournament_details_raw$players %>%
    filter(status == "registered") %>%
    select(username)
  registered_players <- registered_players$username

  user_stats <- data.frame(
    username = character(),
    timeout_percent = numeric()
  )

  i <- 1
  while(i <= length(registered_players)) {
    print(paste0(i, "/", length(registered_players), " Fetching player data for: ", registered_players[i]))
    stats <- .getPlayerStats(registered_players[i])
    user_stats <- user_stats %>% rbind(stats)
    i <- i+1
  }

  # Filter on players who do not meet the requirements to participate
  ineligible_players <- user_stats %>%
    filter(timeout_percent > max_timeout | time_per_move > max_move_speed)

  return(user_stats)
}

################################
### Private Helper Functions ###
################################

#' @description Fetch a player's timeout percent and time per move
#' @param player ID of the player to fetch stats for
#' @return Player's timeout percent and time per move
.getPlayerStats <- function(player) {
  baseUrl <- "https://api.chess.com/pub/player/"
  endpoint <- paste0(baseUrl, player, "/stats", sep = "", collapse = NULL)
  user_stats_raw <- try(fromJSON(toString(endpoint), simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))

  if(class(user_stats_raw) == "try-error") {
    stop(paste("Error: stats for user ", player, " cannot be found", sep = "", collapse = NULL))
  }

  user_stats_unlisted <- unlist(user_stats_raw , use.names = TRUE)

  user_stats <- as.data.frame(t(user_stats_unlisted))
  seconds_in_hour <- 60*60

  user_timeout <- user_stats %>%
    mutate(username = player) %>%
    select(username, chess_daily.record.timeout_percent, chess_daily.record.time_per_move) %>%
    mutate(across(c(chess_daily.record.time_per_move, chess_daily.record.timeout_percent), as.numeric)) %>%
    rename(time_per_move = chess_daily.record.time_per_move) %>%
    rename(timeout_percent = chess_daily.record.timeout_percent) %>%
    mutate(time_per_move = time_per_move/(seconds_in_hour))

  return(user_timeout)
}
