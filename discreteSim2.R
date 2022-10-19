set.seed(17)

source("simData.r")

##================================
## discrete model
## This is a discrete model for the probabilistic ranking system.
##================================

NUM_PLAYERS <- 2
NUM_WEEKS <- 1000

## we implicitly assume two players in this algo
if(NUM_PLAYERS != 2) {
  error("This only works with 2 players!")
}

# This is the dimension of the ranking vector
# or alternatively, the number of possible discrete rankings.
SIM_N <- 100

## initial ratings matrix
SIM_N_2 = SIM_N^2
ratings0 <- matrix(rep(1/SIM_N_2,SIM_N_2),nrow=SIM_N,ncol=SIM_N)

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
      M[i,j] <- pWin(i,j)
    }
  }
  M
}

##=============================
## player ranking simulation
##=============================

## this function returns updated vectors for the given players
## R1 is the rating probability vector for player 1
## R2 is the rating probability vector for player 2
## p1Wins is TRUE if player 1 wins and false if player 2 wins
## (ties not modeled)
doMatch <- function(priorRatings,p1Wins) {
  #winner is the left side
  #multiple and sum other players states (matrix mulitply)
  #multiple without sum for players vector resulting states ("normal" multiply)
  if(p1Wins) {
    postNoNorm = M * priorRatings
  }
  else {
    postNoNorm = t(M) * priorRatings
  }
  postRatings = postNoNorm / sum(postNoNorm)
  
  postRatings
}


runOneWeek <- function(matchFrame,priorRatings) {
  ## there is only one match with only two players
  p1Wins <- ( ((matchFrame[[1,"p1"]] == 1)&&(matchFrame[[1,"win1"]])) || ((matchFrame[[1,"p1"]] == 2)&&(!matchFrame[[1,"win1"]])) )
  
  doMatch(priorRatings,p1Wins)
}

##======================================
## Run the simulation
##======================================

##get the simulated players and results
simData = getSimulatedData(NUM_PLAYERS,NUM_WEEKS)
players = simData$players
matches = simData$matches

##evolve ratings
ratings = list()
ratings[[1]] =  runOneWeek(matches[[1]],ratings0)
for(i in 2:NUM_WEEKS) {
  ratings[[i]] = runOneWeek(matches[[i]],ratings[[i-1]])
}

## plot results
x = numeric()
y = numeric()
z = numeric()
index = 1
for(i in 1:SIM_N) {
  for(j in 1:SIM_N) {
     x[index] = i
     y[index] = j
     z[index] = ratings[[NUM_WEEKS]][i,j]
     index = index + 1
  }
}


zf = cut(z,breaks=(0:10)*(max(z)/10),labels=1:10,include.lowest=TRUE)

plot(x,y,t="n")
for(iz in 1:10) {
  slctr <- (zf == iz)
  clr <- sprintf("#%x%x%x",256 - iz * 20,256 - iz * 20, 256 - iz * 20)
  points(x[slctr],y[slctr],col=clr,pch=15,cex=1)
}

points(players[1],players[2],col="red")



