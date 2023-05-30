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
getIneligiblePlayers <- function(tournament_id, max_timeout = NA, max_move_speed = NA) {
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
    total_games = numeric(),
    timeout_percent = numeric(),
    time_per_move = numeric()
  )

  i <- 1
  while(i <= length(registered_players)) {
    print(paste0(i, "/", length(registered_players), " Fetching player data for: ", registered_players[i]))
    stats <- .getPlayerStats(registered_players[i])
    user_stats <- user_stats %>% rbind(stats)
    i <- i+1
  }

  # Filter on players who do not meet the requirements to participate

  ineligible_players <- user_stats

  # Return the ineligible players based on the given criteria
  if(is.na(max_timeout) && is.na(max_move_speed)) {
    warning("No criteria for eligibility given. Returning all registered players!")
  } else if(is.na(max_timeout)) {
    ineligible_players <- ineligible_players %>%
      filter(time_per_move > max_move_speed)
  } else if(is.na(max_move_speed)) {
    ineligible_players <- ineligible_players %>%
      filter(timeout_percent > max_timeout)
  } else {
    ineligible_players <- ineligible_players %>%
      filter(timeout_percent > max_timeout | time_per_move > max_move_speed)
  }
  return(ineligible_players)
}

#' @description Get a list of players within a given club who are eligible to join a tournament with the given parameters
#' @param club_id ID of the club you want to invite members from
#' @param max_timeout The maximum allowed timeout percentage required to participate
#' @param max_move_speed The maximum time per move required to participate (in hours)
#' @param min_games The minimum number of games that must be completed before joining
#' @return Data frame of eligible players within a given club
#' @export
getPlayersToInviteFromClub <- function(club_id, max_timeout = NA, max_move_speed = NA, min_games = 0) {

  # Fetch club members
  all_club_members <- .getAllClubMembers(club_id)
  all_club_members <- all_club_members$username

  all_club_members_stats <- data.frame(
    username = character(),
    timeout_percent = numeric(),
    time_per_move = numeric(),
    last_online = numeric(),
    total_games = numeric()
  )

  # fetch relevant user stats
  i <- 1
  while(i <= length(all_club_members)) {
    print(paste0(i, "/", length(all_club_members), " Fetching player data for: ", all_club_members[i]))
    stats <- .getPlayerStats(all_club_members[i])
    all_club_members_stats <- all_club_members_stats %>% rbind(stats)
    i <- i+1
  }

  # Get a list of users who meet the criteria

  ineligible_players <- all_club_members_stats

  if(is.na(max_timeout) && is.na(max_move_speed)) {
    warning("No criteria for eligibility given. Returning all club members!")
  } else if(is.na(max_timeout)) {
    ineligible_players <- ineligible_players %>%
      filter(time_per_move > max_move_speed)
  } else if(is.na(max_move_speed)) {
    ineligible_players <- ineligible_players %>%
      filter(timeout_percent > max_timeout)
  } else {
    ineligible_players <- ineligible_players %>%
      filter(timeout_percent > max_timeout | time_per_move > max_move_speed)
  }

  eligible_members <- all_club_members_stats %>%
    anti_join(ineligible_players, by = "username") %>%
    filter(total_games > min_games) %>%
    filter(last_online > Sys.Date() - 7) # Online in the past 7 days

  return(eligible_members)
}

################################
### Private Helper Functions ###
################################

#' @description Fetch a player's timeout percent and time per move
#' @param player ID of the player to fetch stats for
#' @return Player's timeout percent and time per move
.getPlayerStats <- function(player) {
  baseUrl <- "https://api.chess.com/pub/player/"
  endpoint <- paste0(baseUrl, player, sep = "", collapse = NULL)
  user_profile <- try(fromJSON(toString(endpoint), flatten = TRUE)) # raw data of member activity (username, join date)

  if(class(user_profile) == "try-error") {
    stop(paste("Error: user ", player, " cannot be found", sep = "", collapse = NULL))
  }

  user_profile <- as.data.frame(user_profile)

  baseUrl <- "https://api.chess.com/pub/player/"
  endpoint <- paste0(baseUrl, player, "/stats", sep = "", collapse = NULL)
  user_stats_raw <- try(fromJSON(toString(endpoint), simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE))

  if(class(user_stats_raw) == "try-error") {
    stop(paste("Error: stats for user ", player, " cannot be found", sep = "", collapse = NULL))
  }

  user_stats_unlisted <- unlist(user_stats_raw , use.names = TRUE)

  user_stats <- as.data.frame(t(user_stats_unlisted))
  seconds_in_hour <- 60*60

  user_stats <- .add_cols(
    user_stats,
    c("chess_daily.last.rating",
      "chess960_daily.last.rating",
      "chess_daily.record.time_per_move",
      "chess_daily.record.timeout_percent",
      "chess_daily.record.win",
      "chess_daily.record.loss",
      "chess_daily.record.draw")
    )

  user_stats <- user_stats %>%
    mutate(username = player) %>%
    inner_join(user_profile, by = "username") %>%
    select(username, chess_daily.record.timeout_percent, chess_daily.record.time_per_move, chess_daily.record.win, chess_daily.record.loss, chess_daily.record.draw, last_online) %>%
    mutate(across(c(chess_daily.record.time_per_move, chess_daily.record.timeout_percent, chess_daily.record.win, chess_daily.record.loss, chess_daily.record.draw), as.numeric)) %>%
    mutate(total_games = sum(chess_daily.record.win, chess_daily.record.loss, chess_daily.record.draw)) %>%
    select(-chess_daily.record.win, -chess_daily.record.loss, -chess_daily.record.draw) %>%
    rename(time_per_move = chess_daily.record.time_per_move) %>%
    rename(timeout_percent = chess_daily.record.timeout_percent) %>%
    mutate(time_per_move = time_per_move/(seconds_in_hour)) %>%
    mutate(last_online = as.Date.POSIXct(last_online))

  return(user_stats)
}

#' @description Fetch all members of a given club
#' @param club_id ID of the club to fetch members for
#' @return List of all members in a club
.getAllClubMembers <- function(club_id) {
  print(paste("Fetching members for club:", club_id))
  baseUrl <- "https://api.chess.com/pub/club/"
  endpoint <- paste0(baseUrl, club_id, "/members", sep = "", collapse = NULL)
  club_members_raw <- try(fromJSON(toString(endpoint), flatten = TRUE))

  if(class(club_members_raw) == "try-error") {
    stop("Error: members cannot be found")
  }

  all_club_members <- rbind(club_members_raw$weekly, club_members_raw$monthly, club_members_raw$all_time) %>%
    select(username) %>%
    as.data.frame()
  return(all_club_members)
}

.add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if(length(add) != 0) df[add] <- NA
  return(df)
}
