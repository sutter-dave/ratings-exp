source("simData.R")

library(dplyr)

NUM_PLAYERS <- 2
NUM_WEEKS <- 20

## the mean of our initial distribution
MEAN_0 <- 50
## the variance of out initial distribution
SD_0 <- 200
## the variance we will use (for now) for the value we want to stay fixed
FIXED_SD_0 <- .1

##=============================
## simulation
##=============================

num_runs <- 100000


calcNewRatings <- function(r1m,r1sd,r2m,r2sd,p1Wins) {
  ## get the random values for the player rankings and the match values
  player1Rs <- rnorm(num_runs,r1m,r1sd)
  player2Rs <- rnorm(num_runs,r2m,r2sd)
  
  matchValues <- runif(num_runs,min=0,max=1)
  
  ##why bother making this data frame. I can just keep the vectors 
  sim <- data.frame(p1=player1Rs,p2=player2Rs,mval=matchValues)
  
  ## calculate the winner
  sim$p1w <- pMatch(sim$p1,sim$p2,TRUE) > sim$mval
  
  ##output scenarios - p1 wins and loses
  
  trueFrame <- filter(sim,p1w == p1Wins)
  
  ## the distributions of p1 and p2 in each dataframe gives the distributions
  ## after the match
  
  post_mean_1 <- mean(trueFrame$p1)
  post_sd_1 <- sd(trueFrame$p1)
  
  post_mean_2 <- mean(trueFrame$p2)
  post_sd_2 <- sd(trueFrame$p2)
  
  list(r1m = post_mean_1, r1sd = post_sd_1, r2m = post_mean_2, r2sd = post_sd_2 )
}



runOneWeek <- function(matchFrame,priorRatings) {
  # copy the ratings. We will update them for each game
  # (later we will evolve the ratings before update, probably)
  postRatings <- priorRatings
  
  #this function process one match, getting updated ratings
  processMatch <- function(p1,p2,p1Wins) {
    prior_r1m <- priorRatings$pm[p1]
    prior_r1sd <- priorRatings$psd[p1]
    prior_r2m <- priorRatings$pm[p2]
    prior_r2sd <- priorRatings$psd[p2]
    
    postInfo = calcNewRatings(prior_r1m,prior_r1sd,prior_r2m, prior_r2sd, p1Wins)
    
    postRatings$pm[p1] <<- postInfo$r1m
    postRatings$psd[p1] <<- postInfo$r1sd 
    postRatings$pm[p2] <<- postInfo$r2m
    postRatings$psd[p2] <<- postInfo$r2sd  
    
    #############
    if(p1Wins) {
      winner <- as.character(p1)
    }
    else {
      winner <- as.character(p2)
    }
    #print(sprintf("m1 = %f sd1 = %f m2 = %f sd2 = %f %s",postRatings$pm[1],postRatings$psd[1],postRatings$pm[2],postRatings$psd[2],winner))
    #############
  }
  
  #processMatch for each row of the match data frame
  mapply(processMatch,matchFrame$p1,matchFrame$p2,matchFrame$win1)
  
  # return the new ratings matrix
  postRatings
}

##=================
## variables
##=================
# rm - vector of player rating means
# rsd - vector of player rating variances


##======================================
## Run the simulation
##======================================

##get the simulated players and results
simData <- getSimulatedData(NUM_PLAYERS,NUM_WEEKS)
players <- simData$players
matches <- simData$matches

## initial ratings matrix
##we have the first player hard coded to an integer
pm0 <- rep(MEAN_0,NUM_PLAYERS)
psd0 <- rep(SD_0,NUM_PLAYERS)

#fix one player - player 1
pm0[1] <- players[1]
psd0[1] <- FIXED_SD_0

ratings0 <- list(pm = pm0, psd = psd0 )

##this is the seed for our monte carlo simulations
set.seed(34234)

##evolve the ratings
ratings <- list()
ratings[[1]] <-  runOneWeek(matches[[1]],ratings0)
for(i in 2:NUM_WEEKS) {
  ratings[[i]] <- runOneWeek(matches[[i]],ratings[[i-1]])
}


results <- data.frame(player = 1:NUM_PLAYERS,actual = players,meas = ratings[[NUM_WEEKS]]$pm, measdev = ratings[[NUM_WEEKS]]$psd)
results$err <- results$meas - results$actual
results$zerr <- results$err / results$measdev

print(results)
print(sum(results$err^2))


                   


