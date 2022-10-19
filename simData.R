
MIN_RATING <- 0
MAX_RATING <- 100

PROB_CONSTANT <- (MAX_RATING - MIN_RATING) / 3

## This is the probability of player one winning a match, given the
## ranking values r and rOpp
pWin <- function(r,rOpp) {
  xx <- exp( (r - rOpp) / PROB_CONSTANT )
  xx / (1 + xx)
}

## this function creates a data frame with a set of generated matches
## for the group (I call this matches in one "week" for the simulation)
## and some accompanying information
getWeekMatches <- function(players) {
  numPlayers = length(players)
  
  ms <- matrix(sample(1:numPlayers,numPlayers),nrow = numPlayers / 2)
  matchFrame = data.frame( p1 = ms[,1],p2 = ms[,2] )
  
  ## add a column for the true ranking of each player
  matchFrame$p1TR <- players[matchFrame$p1]
  matchFrame$p2TR <- players[matchFrame$p2]
  
  ## add a column for the true win percentage (for player 1)
  matchFrame$pwin1 <- pWin(matchFrame$p1TR,matchFrame$p2TR)
  
  ## add a random number from 0 to 1, to calculate the winner
  matchFrame$mrv <- runif(length(matchFrame$pwin1))
  
  ## add a column for the winner
  matchFrame$win1 <- matchFrame$pwin1 > matchFrame$mrv
  
  matchFrame
}


##====================================
## Simulated players and matches
##====================================

## This function creates a set of players with a randomized "rating"
## and a set of matches with simulated win/loss results
getSimulatedData <- function(NUM_PLAYERS,NUM_WEEKS) {

  ##------------------------------------
  ## simulated player "true" ratings
  ##------------------------------------
  
  players <- runif(NUM_PLAYERS,MIN_RATING,MAX_RATING) 
  
  ##fix one of the player rankings - player 1 at center point
  players[1] = (MIN_RATING + MAX_RATING)/2

  ##------------------------------------
  ## "schedule" generation and simulation
  ##------------------------------------

  ## we will make a list with matches (we will say each is one week)
  matches <- list()
  
  for(i in 1:NUM_WEEKS) {
    matches[[i]] <- getWeekMatches(players)
  }
  
  matches = matches
  
  list(players=players,matches = matches)
}
