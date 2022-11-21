## Elo model

library(dplyr)

source("simData.r")

######################################################################
## WRAPPER TO RETURN MODEL
## (FIGURE OUT THE PROPER WAY TO PROTECT THESE LATER)
## This wraps all the code that generates the model. I think I want
## to make a package instead
getEloModel <- function() {
  
######################################################################

R0 <- 0

##Our prob contstant is 33, where I think Elo used 200 * sqrt(2) - but that had a slightly different meaning
## For now, we will assume our ratings are 1/8 of his

RATING_GAIN = 1.75
#RATING_GAIN = 5
 
  
##============================
## Exported Functions
##============================

getInitialRatings <- function(numPlayers,r1) {
  ratings0 = list()
  ratings0$prs <- rep(r1,NUM_PLAYERS)
  ratings0
}



processMatches <- function(matchFrame,priorRatings) {
  prs <- priorRatings$prs
  
  #this function process one match, getting updated ratings
  processMatch <- function(p1,p2,p1Wins) {
    prior_r1 <- prs[p1]
    prior_r2 <- prs[p2]
    
    pp1 = pMatch(prior_r1,prior_r2,TRUE)
    
    post_r1 <- prior_r1 + RATING_GAIN * (p1Wins - pp1)
    post_r2 <- prior_r2 - RATING_GAIN * (p1Wins - pp1) ## same value as p1 but minus sign
    
    prs[p1] <<- post_r1
    prs[p2] <<- post_r2
  }
  
  #processMatch for each row of the match data frame
  mapply(processMatch,matchFrame$p1,matchFrame$p2,matchFrame$win1)
  
  # return the new ratings vector
  list(prs=prs)
}

##=======================================
## Model Object
##=======================================

######################################################################
## END OF WRAPPER TO RETURN MODEL

eloModel <- list()
eloModel$getInitialRatings <- getInitialRatings
eloModel$processMatches <- processMatches
eloModel$name <- "Elo Model"
eloModel$hasSD <- FALSE

  eloModel
}
######################################################################


##-----------------------------
## correct for drift in r1 - since we fixed this in other models, as a free parameter
##-----------------------------
dontRun <- function() {
  
finalRatings <- ratings[[NUM_WEEKS]] + R0 - ratings[[NUM_WEEKS]][1]

results <- data.frame(player = 1:NUM_PLAYERS,actual = players,meas = finalRatings)
results$err <- results$meas - results$actual

print(results)
print(sum(results$err^2))
}