set.seed(1699)

source("matchUtil.R")

##================================
## discrete model
## This is a discrete model for the probabilistic ranking system.
##================================

######################################################################
## WRAPPER TO RETURN MODEL
## (FIGURE OUT THE PROPER WAY TO PROTECT THESE LATER)
## This wraps all the code that generates the model. I think I want
## to make a package instead
getDiscreteDist <- function() {
  
######################################################################

##length of the quantized simulation vector
SIM_N <- 100

## this returns the mean and std deviation give a player rating distribution vector 
getStats <- function(pDist) {
  prm <- sum(1:SIM_N * pDist)
  prsd <- sqrt( sum( (1:SIM_N - prm)^2 * pDist ))
  c(prm=prm,prsd=prsd)
}

## This function creates a ratings object for a given distribution,
## packing the distribution along with the mean and standard deviation
getRatingsObject <- function(pDists) {
  pStats <- apply(pDists,2,getStats)
  ratings = list()
  ratings$pDists <- pDists
  ratings$prs <- pStats["prm",]
  ratings$prsds <- pStats["prsd",]
  ratings
}

##spread matrix--------------------------
##construct the tridiagonal spread matrix to model change in rating over time

##quck and dirty implementation
## - I can crank up the decay factor by a lot. 
## If there are no other problems, I shoudl reimplement with a wider spread
## like maybe a true normal distribution

SPREAD_DECAY <- .2
SPREAD_MATRIX <- matrix(rep(0,10000),nrow=100,ncol=100)
SPREAD_MATRIX[row(SPREAD_MATRIX)==col(SPREAD_MATRIX)] <- 1 - SPREAD_DECAY
SPREAD_MATRIX[abs(row(SPREAD_MATRIX)-col(SPREAD_MATRIX)) == 1] <- SPREAD_DECAY/2
#ensure sum to 1 at boundaries
SPREAD_MATRIX[1,1] <- 1 - SPREAD_DECAY/2 
SPREAD_MATRIX[100,100] <- 1 - SPREAD_DECAY/2

DO_SPREAD <- FALSE
#DO_SPREAD <- TRUE

spreadFunction <- function(vec) {
  #c is there because the multiplication gives a matrix output otherwise
  c(SPREAD_MATRIX %*% vec)
}

##---------------------------------------




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

## this function returns updated vectors for the given players
## r1 is the rating probability vector for player 1
## r2 is the rating probability vector for player 2
## p1Wins is TRUE if player 1 wins and false if player 2 wins
## (ties not modeled)
updateRatings <- function(r1,r2,p1Wins) {
  #winner is the left side
  #multiple and sum other players states (matrix mulitply)
  #multiple without sum for players vector resulting states ("normal" multiply)
  if(p1Wins) {
    r1_post = r1 * (M %*% r2)
    r2_post = (r1 %*% M) * r2
    norm = sum(r1_post) ## we can use either to get the normalization
  }
  else {
    r2_post = r2 * (M %*% r1)
    r1_post = (r2 %*% M) * r1
    norm <- sum(r2_post) ## we can use either to get the normalization
  }
  list(r1=c(r1_post)/norm,r2=c(r2_post)/norm) 
}

processMatches <- function(matchFrame,priorRatings) {
  ## get the distributions for all the players
  pDists <- priorRatings$pDists
  
  if(DO_SPREAD) {
    #here we model a potential drift in the ranking over time
    #do not apply to player 1 - fixed rating
    pDists[2:NUM_PLAYERS] <- apply(pDists[2:NUM_PLAYERS],2,spreadFunction)
  }

  
  #this function processes one match, getting updated ratings
  processMatch <- function(p1,p2,p1Wins) {
    prior_r1 <- pDists[,p1]
    prior_r2 <- pDists[,p2]
    
    postInfo = updateRatings(prior_r1,prior_r2,p1Wins)
    
    pDists[,p1] <<- postInfo$r1
    pDists[,p2] <<- postInfo$r2
  }
  
  #processMatch for each row of the match data frame
  mapply(processMatch,matchFrame$p1,matchFrame$p2,matchFrame$win1)
  
  # get the ratings object for the new player distributions
  getRatingsObject(pDists)
}


## This function returns an initial ratings object, given the number of players
## and the ratings for player 1, which is a fixed integer.
getInitialRatings <- function(numPlayers,r1) {
  getDist0 <- function(p) {
    if(p == 1) {
      #player 1 - 100% rating r1 (fixed value)
      rDist <- rep(0,SIM_N)
      rDist[r1] <- 1
    }
    else {
      #other players - uniform distribution
      rDist <- rep(1/SIM_N,SIM_N)
    }
    rDist
  }
  
  pDists <- sapply(1:NUM_PLAYERS,getDist0)
  getRatingsObject(pDists)
}

##=======================================
## Model Object
##=======================================

######################################################################
## END OF WRAPPER TO RETURN MODEL

discreteDist <- list()
discreteDist$getInitialRatings <- getInitialRatings
discreteDist$processMatches <- processMatches

discreteDist$name <- "Discrete Distribution"
discreteDist$hasSD <- TRUE

  discreteDist
}

######################################################################



################################################################################
dontExecute <- function() {
  ## plot the results
  pm = numeric()
  psd = numeric()
  pz = numeric()
  
  ##maybe clean this up with some lapply now that I have a list for ratings[[NUM_WEEKS]]?
  for(i in 1:NUM_PLAYERS) {
    
    pm[i] = sum(1:SIM_N * ratings[[NUM_WEEKS]][[i]])
    psd[i] = sqrt( sum( (1:SIM_N - pm[i])^2 * ratings[[NUM_WEEKS]][[i]]) )
    if(psd[i] > .0000000000001) {
      pz[i] = (players[i] - pm[i]) / psd[i]
    }
    else {
      pz[i] = 0
    }
    
    plot(ratings[[NUM_WEEKS]][[i]],t="l")
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
}


#######################################
#######################################


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

