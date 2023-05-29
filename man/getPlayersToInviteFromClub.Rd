\name{getPlayersToInviteFromClub}
\alias{getPlayersToInviteFromClub}
\title{Get players eligible for a tournament from a given club}
\usage{
getPlayersToInviteFromClub(club_id, max_timeout = NA, max_move_speed = NA, min_games = 0)
}
\description{
Fetches the timeout percentage and move speed for members of the given club and returns the players who meet the criteria to participate in the tournament.
max_timeout is a percentage of games lost on time in the last 90 days
max_move_speed is the maximum time per move allowed for participation
min_games is the minimum numbers of games the player must complete before joining
}
\examples{
getPlayersToInviteFromClub(club_id = "my-club", max_timeout = 10)
getPlayersToInviteFromClub(club_id = "my-club", max_move_speed = 12)
getPlayersToInviteFromClub(club_id = "my-club", max_timeout = 10, max_move_speed = 12)
getPlayersToInviteFromClub(club_id = "my-club", max_timeout = 10, max_move_speed = 12, min_games = 20)
}
