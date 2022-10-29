## Elo model

library(dplyr)

source("simData.r")

NUM_PLAYERS <- 10
NUM_WEEKS <- 1000

R0 <- 0

##Our prob contstant is 33, where I think Elo used 200 * sqrt(2) - but that had a slightly different meaning
## For now, we will assume our ratings are 1/8 of his

RATING_GAIN = 1.75
 
  
##============================



runOneWeek <- function(matchFrame,priorRatings) {
  postRatings <- priorRatings
  
  #this function process one match, getting updated ratings
  processMatch <- function(p1,p2,p1Wins) {
    prior_r1 <- priorRatings[p1]
    prior_r2 <- priorRatings[p2]
    
    pp1 = pMatch(prior_r1,prior_r2,TRUE)
    
    post_r1 <- prior_r1 + RATING_GAIN * (p1Wins - pp1)
    post_r2 <- prior_r2 - RATING_GAIN * (p1Wins - pp1) ## same value as p1 but minus sign
    
    postRatings[p1] <<- post_r1
    postRatings[p2] <<- post_r2
  }
  
  #processMatch for each row of the match data frame
  mapply(processMatch,matchFrame$p1,matchFrame$p2,matchFrame$win1)
  
  # return the new ratings vector
  postRatings
}

##======================================
## Run the simulation
##======================================

##get the simulated players and results
simData <- getSimulatedData(NUM_PLAYERS,NUM_WEEKS)
players <- simData$players
matches <- simData$matches

R0 <- players[1]

## initial ratings matrix
##we have the first player hard coded to an integer
ratings0 <- rep(R0,NUM_PLAYERS)

##evolve the ratings
ratings <- list()
ratings[[1]] <-  runOneWeek(matches[[1]],ratings0)
for(i in 2:NUM_WEEKS) {
  ratings[[i]] <- runOneWeek(matches[[i]],ratings[[i-1]])
}

##-----------------------------
## correct for drift in r1 - since we fixed this in other models, as a free parameter
##-----------------------------
finalRatings <- ratings[[NUM_WEEKS]] + R0 - ratings[[NUM_WEEKS]][1]

results <- data.frame(player = 1:NUM_PLAYERS,actual = players,meas = finalRatings)
results$err <- results$meas - results$actual

print(results)
print(sum(results$err^2))