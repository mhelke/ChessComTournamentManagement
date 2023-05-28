\name{getIneligiblePlayers}
\alias{getIneligiblePlayers}
\title{Get ineligible players for a given tournament}
\usage{
getIneligiblePlayers(tournament_id, max_timeout, max_move_speed)
}
\description{
Fetches the timeout percentage and move speed for registered players and returns the players who do not meet the criteria to participate in the tournament
max_timeout is a percentage of games lost on time in the last 90 days
max_move_speed is the maximum time per move allowed for participation
}
\examples{
getIneligiblePlayers(tournament_id = "my-tournament", max_timeout = 10, max_move_speed = 12)
}
