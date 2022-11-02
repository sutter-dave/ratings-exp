set.seed(1699)

source("simData.r")

NUM_PLAYERS <- 10
NUM_WEEKS <- 100

## the mean of our initial distribution
MEAN_0 <- 50
## the variance of out initial distribution
SD_0 <- 200
## the variance we will use (for now) for the value we want to stay fixed
FIXED_SD_0 <- .1

##================================
## discrete model
## This is a discrete model for the probabilistic ranking system.
##================================

##length of the quantized simulation vector
SIM_N <- 100


##-------------------------------------------------------------
## This matrix gives the probability for player 1 to win, where
## first index (rows): index for player 1
## second index (columns): index for player 2
##-------------------------------------------------------------
M <- {
  v <- vector(mode = "numeric", length = SIM_N*SIM_N)
  M <- matrix(v,nrow=SIM_N,ncol=SIM_N)
  for(i in 1:SIM_N) {
    for(j in 1:SIM_N) {
      M[i,j] <- pMatch(i,j,TRUE)
    }
  }
  M
}

##=============================
## player rating evolution functions
##=============================

## ratings list, by week
ratings <- list()

## this function returns updated vectors for the given players
## R1 is the rating probability vector for player 1
## R2 is the rating probability vector for player 2
## p1Wins is TRUE if player 1 wins and false if player 2 wins
## (ties not modeled)
doMatch <- function(r1m,r1sd,r2m,r2sd,p1Wins) {
  
  ##figure out boundary conditions!
  
  R1 <- normalDist(r1m,r1sd)
  R2 <- normalDist(r2m,r2sd)
  
  #winner is the left side
  #multiple and sum other players states (matrix mulitply)
  #multiple without sum for players vector resulting states ("normal" multiply)
  if(p1Wins) {
    R1Post <- R1 * (M %*% R2)
    R2Post <- (R1 %*% M) * R2
  }
  else {
    R2Post <- R2 * (M %*% R1)
    R1Post <- (R2 %*% M) * R1
  }
  r1m <- mean(R1Post)
  r1sd <- sd(R1Post)
  r2m <- mean(R2Post)
  r2sd <- sd(R2Post)
  
  list(R1=c(R1Post)/norm,R2=c(R2Post)/norm) 
}


runOneWeek <- function(matchFrame,priorRatings) {
  # copy the ratings. We will update them for each game
  # (later we will evolve the ratings before update, probably)
  postRatings <- priorRatings
  
  #this function process one match, getting updated ratings
  processMatch <- function(p1,p2,p1Wins) {
    prior_r1m <- postRatings$rm[p1]
    prior_r1sd <- postRatings$rsd[p1]
    prior_r2m <- postRatings$rm[p2]
    prior_r2sd <- postRatings$rsd[p2]
    
    postInfo = doMatch(prior_r1m,prior_r1sd,prior_r2m,prior_r2sd,p1Wins)
    
    postRatings$rm[p1] <<- postInfo$r1m
    postRatings$rsd[p1] <<- postInfo$r1sd
    postRatings$rm[p2] <<- postInfo$r2m
    postRatings$rsd[p2] <<- postInfo$r2sd
  }
  
  #processMatch for each row of the match data frame
  mapply(processMatch,matchFrame$p1,matchFrame$p2,matchFrame$win1)
  
  # return the new ratings matrix
  postRatings
}

##======================================
## Run the simulation
##======================================

##get the simulated players and results
simData = getSimulatedData(NUM_PLAYERS,NUM_WEEKS)
players = simData$players
matches = simData$matches

## initial ratings matrix
##we have the first player hard coded to an integer
pm0 <- rep(MEAN_0,NUM_PLAYERS)
psd0 <- rep(SD_0,NUM_PLAYERS)

#fix one player - player 1
pm0[1] <- players[1]
psd0[1] <- FIXED_SD_0

ratings0 = list(rm = rm0, rsd = rsd0)

##evolve the ratings
ratings = list()
ratings[[1]] =  runOneWeek(matches[[1]],ratings0)
for(i in 2:NUM_WEEKS) {
  ratings[[i]] = runOneWeek(matches[[i]],ratings[[i-1]])
}

## plot the results
pm = numeric()
psd = numeric()
pz = numeric()

for(i in 1:NUM_PLAYERS) {
  
  pm[i] = sum(1:SIM_N * ratings[[NUM_WEEKS]][i,])
  psd[i] = sqrt( sum( (1:SIM_N - pm[i])^2 * ratings[[NUM_WEEKS]][i,]) )
  if(psd[i] > .0000000000001) {
    pz[i] = (players[i] - pm[i]) / psd[i]
  }
  else {
    pz[i] = 0
  }
  
  plot(ratings[[NUM_WEEKS]][i,],t="l")
  abline(v=players[i],col="red",lwd=2)
  abline(v=pm[i],col="blue",lwd=1)
  #print(sprintf("player %d: actual: %f meas: %f",i,players[i],pm[i]))
}

plot(pz)

results <- data.frame(player = 1:NUM_PLAYERS,actual = players,meas = pm, measdev = psd)
results$err <- results$meas - results$actual
results$zerr <- results$err / results$measdev

print(results)
print(sum(results$err^2))

