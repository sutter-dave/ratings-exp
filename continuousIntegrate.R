source("simData.r")

library(cubature)

NUM_PLAYERS <- 10
NUM_WEEKS <- 100

## the mean of our initial distribution
MEAN_0 <- 50
## the variance of out initial distribution
SD_0 <- 200
## the variance we will use (for now) for the value we want to stay fixed
FIXED_SD_0 <- .1

##=======================================
## Continuous Ratings Distribution Model
##=======================================
# Here we model the ratings distribution for a player as a continuous function
# Further we will assume it is a normal distribution.
# We will however fix one player to have an exact value (player 1) 
# We will denote this with a std = 0

sqrt2pi <- sqrt(2*pi)

## this is the ratings distribution for a player
normDist <- function(r,rm,rsd) {
  exp( -(r - rm)^2 / (2 * rsd^2) ) / (sqrt2pi * rsd)
}

##=================
## process to update ratings
##=================

## this gets the post ratings distribution function for two normally distributed player ratings
getPostRatingsDistFunc <- function(r1m,r1sd,r2m,r2sd,p1Wins) {
  function(r) {
    pMatch(r[1],r[2],p1Wins) * normDist(r[1],r1m,r1sd) * normDist(r[2],r2m,r2sd)
  }
}


getPostRatingsMeanFunc <- function(postRatingsDistFunc,post_norm,index) {
  function(r) {
    r[index] * postRatingsDistFunc(r) / post_norm
  }
}

getPostRatingsVarFunc <- function(postRatingsDistFunc,post_norm,post_mean,index) {
  function(r) {
    (r[index] - post_mean)^2 * postRatingsDistFunc(r) / post_norm
  }
}

calcNewRatings <- function(r1m,r1sd,r2m,r2sd,p1Wins,p1Index,p2Index) {
  #create the post ratings distribution (not normalized)
  postRatingsDistFunc <- getPostRatingsDistFunc(r1m,r1sd,r2m,r2sd,p1Wins)
  
  ##############################################################################
  ## plot wave function for r1 and r2 - by integrating out theother player rating
  
  plr1 <- 0
  plr2 <- 0
  
  plf1 <- function(r2) {
    p1 <- postRatingsDistFunc(c(plr1,r2))
  } 
  plf2 <- function(r1) {
    p2 <-postRatingsDistFunc(c(r1,plr2))
  }
  
  plLabel1 = sprintf("Player %d",p1Index)
  plLabel2 = sprintf("Player %d",p2Index)
  
  pldist1 = numeric()
  pldist2 = numeric()
  plx1 = numeric()
  plx2 = numeric()
  for(i in 1:100) {
    plr1 = r1m + (8 * r1sd * (i - 50)/100)
    plx1[i] = plr1
    pldist1[i] = integrate(Vectorize(plf1),lower=r2m - 4 * r2sd,upper = r2m + 4 * r2sd)$value
    
    
    plr2 = r2m + (8 * r2sd * (i - 50)/100)
    plx2[i] = plr2
    pldist2[i] = integrate(Vectorize(plf2),lower=r1m - 4 * r1sd,upper = r1m + 4 * r1sd)$value
  }
  
  plot(plx1,pldist1,type="l",main=plLabel1)
  plot(plx2,pldist2,type="l",main=plLabel2)
  
  
  
  
  ##############################################################################
  
  ##need to get the integegration region - true 4 standard deviations from the prior peak
  lowerBounds <- c(r1m - 4 * r1sd, r2m - 4 * r2sd)
  upperBounds <- c(r1m + 4 * r1sd, r2m + 4 * r2sd)
  
  #calculate the normalization
  resultA <- adaptIntegrate(postRatingsDistFunc, lowerLimit = lowerBounds, upperLimit = upperBounds )
  post_norm <- resultA$integral
  
  #initialize the function to calculate the means (scalar functions)
  postMeanFunc1 <- getPostRatingsMeanFunc(postRatingsDistFunc,post_norm,1)
  postMeanFunc2 <- getPostRatingsMeanFunc(postRatingsDistFunc,post_norm,2)
  
  #calculate the means
  resultB <- adaptIntegrate(postMeanFunc1, lowerLimit = lowerBounds, upperLimit = upperBounds )
  post_mean_1 <- resultB$integral
  
  resultC <- adaptIntegrate(postMeanFunc2, lowerLimit = lowerBounds, upperLimit = upperBounds )
  post_mean_2 <- resultC$integral
  
  #initialize the function to calculate the variances
  postVarFunc1 <- getPostRatingsVarFunc(postRatingsDistFunc,post_norm,post_mean_1,1)
  postVarFunc2 <- getPostRatingsVarFunc(postRatingsDistFunc,post_norm,post_mean_2,2)
  
  #calculate the variances
  resultD <- adaptIntegrate(postVarFunc1, lowerLimit = lowerBounds, upperLimit = upperBounds )
  post_var_1 <- resultD$integral
  
  resultE <- adaptIntegrate(postVarFunc2, lowerLimit = lowerBounds, upperLimit = upperBounds )
  post_var_2 <- resultE$integral
  
  list(r1m = post_mean_1, r1sd = sqrt(post_var_1), r2m = post_mean_2, r2sd = sqrt(post_var_2) )
}

runOneWeek <- function(matchFrame,priorRatings) {
  # copy the ratings. We will update them for each game
  # (later we will evolve the ratings before update, probably)
  postRatings <- priorRatings
  
  ##############################################################################
  ## for distribution plots
  oldMfrow <- par("mfrow")
  par(mfrow=c(1,2))
  ##############################################################################  
  
  #this function process one match, getting updated ratings
  processMatch <- function(p1,p2,p1Wins) {
    prior_r1m <- priorRatings$pm[p1]
    prior_r1sd <- priorRatings$psd[p1]
    prior_r2m <- priorRatings$pm[p2]
    prior_r2sd <- priorRatings$psd[p2]
    
    postInfo = calcNewRatings(prior_r1m,prior_r1sd,prior_r2m, prior_r2sd, p1Wins, p1Index = p1, p2Index = p2)

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
    #print(sprintf("%d m = %f sd = %f %d m = %f sd = %f winner = %s",p1,postRatings$pm[1],postRatings$psd[1],p2,postRatings$pm[2],postRatings$psd[2],winner))
    #############
  }
  
  #processMatch for each row of the match data frame
  mapply(processMatch,matchFrame$p1,matchFrame$p2,matchFrame$win1)
  
  ##############################################################################
  ## for distribution plots
  par(mfrow=oldMfrow)
  ##############################################################################  
  
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


##======================================
## testing functions and code
##======================================
xxx <- function() {
  r1m = 50
  r2m = 50
  r1sd = 200
  r2sd = 200
  p1Wins = TRUE
  
  r1m = 50
  r2m = 50
  r1sd = .1
  r2sd = 200
  p1Wins = TRUE
  
  
  plotDist2D(postRatingsDistFunc,lowerBounds,upperBounds)
  
  plotDist2D <- function(distFunc,lowerLimits,upperLimits) {
    m = matrix(numeric(),nrow=100,ncol=100)
    for(i in 1:100) {
      for(j in 1:100) {
        m[i,j] = distFunc(c(lowerLimits[1] + i * (upperLimits[1] - lowerLimits[1])/100,
                          lowerLimits[2] + j * (upperLimits[2] - lowerLimits[2])/100) )
      }
    }
    image(m)
    print(range(m))
  }
}

getNormalDistFunc <- function(rm,rsd) {
  function(r) {
    normDist(r[1],rm,rsd)*normDist(r[2],rm,rsd)
  }
}
getUpper <- function(rm,rsd) {
  c(rm + 8 * rsd,rm + 8 * rsd)
}
getLower <- function(rm,rsd) {
  c(rm - 8 * rsd,rm - 8 * rsd)
}
nd <- getNormalDistFunc(14,13)
adaptIntegrate(nd,getLower(14,13),getUpper(14,13))
