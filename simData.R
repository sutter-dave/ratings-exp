
source("matchUtil.R")

## this function creates a data frame with a set of generated matches
## for the group (I call this matches in one "week" for the simulation)
## and some accompanying information
getWeekMatches <- function(week,players) {
  numPlayers = length(players)
  
  ms <- matrix(sample(1:numPlayers,numPlayers),nrow = numPlayers / 2)
  matchFrame = data.frame( p1 = ms[,1],p2 = ms[,2] )
  
  ## add a column for the true ranking of each player
  matchFrame$p1TR <- players[matchFrame$p1]
  matchFrame$p2TR <- players[matchFrame$p2]
  
  ## add a column for the true win percentage (for player 1)
  matchFrame$pwin1 <- pMatch(matchFrame$p1TR,matchFrame$p2TR,TRUE)
  
  ## add a random number from 0 to 1, to calculate the winner
  matchFrame$mrv <- runif(length(matchFrame$pwin1))
  
  ## add a column for the winner
  matchFrame$win1 <- matchFrame$pwin1 > matchFrame$mrv
  
  matchFrame$week <- week
  
  matchFrame
}


##====================================
## Simulated players and matches
##====================================

## This function creates a set of players with a randomized "rating"
## and a set of matches with simulated win/loss results
getSimulatedData <- function(seed,numPlayers,numWeeks) {
  
  set.seed(seed)

  ##------------------------------------
  ## simulated player "true" ratings
  ##------------------------------------
  
  players <- runif(numPlayers,MIN_RATING,MAX_RATING) 
  
  ##fix one of the player rankings - player 1 at an integer near the center point
  players[1] = floor( (MIN_RATING + MAX_RATING)/2 )

  ##------------------------------------
  ## "schedule" generation and simulation
  ##------------------------------------

  ## we will make a list with matches (we will say each is one week)
  matches <- lapply(1:numWeeks,getWeekMatches,players=players)
  
  list(players=players,matches=matches,seed=seed)
}
