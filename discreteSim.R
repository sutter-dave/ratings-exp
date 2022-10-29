set.seed(1699)

source("simData.r")

NUM_PLAYERS <- 10
NUM_WEEKS <- 100

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
doMatch <- function(R1,R2,p1Wins) {
  #winner is the left side
  #multiple and sum other players states (matrix mulitply)
  #multiple without sum for players vector resulting states ("normal" multiply)
  if(p1Wins) {
    R1Post = R1 * (M %*% R2)
    R2Post = (R1 %*% M) * R2
    norm = sum(R1Post) ## we can use either to get the normalization
  }
  else {
    R2Post = R2 * (M %*% R1)
    R1Post = (R2 %*% M) * R1
    norm <- sum(R2Post) ## we can use either to get the normalization
  }
  list(R1=c(R1Post)/norm,R2=c(R2Post)/norm) 
}

## This is the same as doMatch, but it is coded out explicitly 
## with loops
doMatchTest <- function(R1, R2, p1Wins) {
  #get the match result probability for each r1, r2 pair
  if(p1Wins) {
    Rw <- R1
    Rl <- R2
  }
  else {
    Rw <- R2
    Rl <- R1
  }
  mRwRl <- matrix(nrow=SIM_N,ncol=SIM_N)
  for(iw in 1:SIM_N) {
    for(il in 1:SIM_N) {
      mRwRl[iw,il] <- M[iw,il] * Rw[iw] * Rl[il]
    }
  }
  
  #get the total probability of getting this result
  m <- 0
  for(iw in 1:SIM_N) {
    for(il in 1:SIM_N) {
      m <- m + mRwRl[iw,il]
    }
  }
  
  #normalize and sum over states of "other" layer (independence approximation)
  RwPost <- 0
  for(il in 1:SIM_N) {
    RwPost <- RwPost + mRwRl[,il]/m
  }
  RlPost = 0
  for(iw in 1:SIM_N) {
    RlPost <- RlPost + mRwRl[iw,]/m
  }
  
  #convert back to players and return
  if(p1Wins) {
    return(list(R1=RwPost,R2=RlPost)) 
  }
  else {
    return(list(R1=RlPost,R2=RwPost)) 
  }
}



runOneWeek <- function(matchFrame,priorRatings) {
  # copy the ratings. We will update them for each game
  # (later we will evolve the ratings before update, probably)
  postRatings <- priorRatings
  
  #this function process one match, getting updated ratings
  processMatch <- function(p1,p2,p1Wins) {
    priorR1 <- priorRatings[p1,]
    priorR2 <- priorRatings[p2,]
    
    postInfo = doMatch(priorR1,priorR2,p1Wins)
    
    postRatings[p1,] <<- postInfo$R1
    postRatings[p2,] <<- postInfo$R2
  }
  
  #processMatch for each row of the match data frame
  mapply(processMatch,matchFrame$p1,matchFrame$p2,matchFrame$win1)
  
  # return the new ratings matrix
  postRatings
}

## test
#xxxw = doMatch(ratings0[1,],ratings0[2,],TRUE)
#yyyw = doMatchTest(ratings0[1,],ratings0[2,],TRUE)

#xxxl = doMatch(ratings0[1,],ratings0[2,],FALSE)
#yyyl = doMatchTest(ratings0[1,],ratings0[2,],FALSE)


#newRatings <- runOneWeek(matches[[1]],ratings0)

##======================================
## Run the simulation
##======================================

##get the simulated players and results
simData = getSimulatedData(NUM_PLAYERS,NUM_WEEKS)
players = simData$players
matches = simData$matches

## initial ratings matrix
##we have the first player hard coded to an integer
ratings0 <- matrix(rep(1/SIM_N,NUM_PLAYERS * SIM_N),nrow=NUM_PLAYERS,ncol=SIM_N)

if(players[1] != floor(players[1])) {
  error("Player 1 rating not an integer! We will assume it is!")
}
ratings0[1,] <- rep(0,SIM_N)
ratings0[1,players[1]] <- 1

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
  print(sprintf("player %d: actual: %f meas: %f",i,players[i],pm[i]))
}

plot(pz)

results <- data.frame(player = 1:NUM_PLAYERS,actual = players,meas = pm, measdev = psd)
results$err <- results$meas - results$actual
results$zerr <- results$err / results$measdev

print(results)
print(sum(results$err^2))

