## This file runs a test.
source("simData.r")
source("discreteDist.R")

##conventions:
##
## ratings object - a list with the following entries
## - prs - (required) player ratings, a vector of numeric values with length NUM_PLAYERS
## - prsds - (optional) player standard deviations, a vector of numeric values with length NUM_PLAYERS
## - (other, implementatin specific)
##
## test object - a list with the following entries
## - getInitialRatings(NUM_PLAYERS,p1r) - args: number of players, player one (fixed) rating, returns ratings object
## - processMatches(matches, ratings) - args: match data frame, ratings object, returns new ratings object
##
## getSimulatedData(numPlayers,numWeeks,seed) - returns simData
##
## simData: list with entries:
## - players - vector of ratings, min and max values defined in matchUtil
## - matches - list of match data frames, one for each week
##
## match data frame - columns
## - p1 - integer value for first player in match
## - p2 - integer value for second player in match
## - pwin1 - probability of p1 winning
## - mrv - a random number from 0 to 1 (to decide who wins)
## - win1 - TRUE is player 1 wins, FALSE if player2 wins
## 
## NOTES
## - for now I am not assuming the test object functions are vectorized.

NUM_PLAYERS <- 10
NUM_WEEKS <- 2000

test <- getDiscreteDist()

##======================================
## Run the simulation
##======================================

##get the simulated players and results
seed = 1231
simData = getSimulatedData(NUM_PLAYERS,NUM_WEEKS,seed)

playerRatings = simData$players #actual ratings vector
p1R <- playerRatings[1] #fixed rating of player 1
matches = simData$matches


## initial ratings
ratings0 <- test$getInitialRatings(NUM_PLAYERS,p1R)

##evolve the ratings
ratings = list()
ratings[[1]] =  test$processMatches(matches[[1]],ratings0)
for(i in 2:NUM_WEEKS) {
  ratings[[i]] = test$processMatches(matches[[i]],ratings[[i-1]])
}




