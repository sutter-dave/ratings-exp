##FAILED!!!

source("simData.r")

library(cubature)

NUM_PLAYERS <- 3
NUM_WEEKS <- 2

#we will set this to the actual value later
R1 <- 0

##=======================================
## Continuous Ratings Distribution Functions
##=======================================

uniformDistFunc <- function(r) {
  1/(MAX_RATING - MIN_RATING)
}

deltaDistFunc <- function(r) {
  if(r == R1) {
    return(1)
  }
  else {
    return(0)
  }
}

getFuncMean <- function(postRatingsDistFunc,post_norm,index) {
  function(r) {
    r[index] * postRatingsDistFunc(r) / post_norm
  }
}

getFuncSD <- function(postRatingsDistFunc,post_norm,post_mean,index) {
  function(r) {
    (r[index] - post_mean)^2 * postRatingsDistFunc(r) / post_norm
  }
}

plotDistFunc <- function(distFunc,pIndex) {
  x <- MIN_RATING:MAX_RATING
  y <- sapply(x,distFunc)
  
  main <- sprintf("Player %d",pIndex)
  plot(x,y,main=main,type="l",lwd=2,color="blue")
}


## process to update ratings
##=================

## this gets the post ratings distribution function for two normally distributed player ratings
getPostDistFunc <- function(distFunc1,distFunc2,p1Wins) {
  function(r1,r2) {
    pMatch(r1,r2,p1Wins) * distFunc1(r1) * distFunc2(r2)
  }
}

##NEED TO VECTORIZE!!!
getPostDistFunc1 <- function(postDistFunc21,p1,p2) {
  if(p1 == 1) {
    postDistFunc1 <- deltaDistFunc  
  }
  else if(p2 == 1) {
    #r2 is player 1 - substitute in player 1 single value
    postDistFunc1 <- function(r1) {
      postDistFunc21(R1,r1)
    }
  }
  else {
    postDistFunc1 = function(r1) {
      #get dist function for fixed first player rating - to integrate out second player

      #integrate over second player ratings
      result <- integrate(postDistFunc21,MIN_RATING,MAX_RATING,r1)
      
      result$value
    }
  }
  
  postDistFunc1
}

##SAME AS ABOVE!!!
getPostDistFunc2 <- function(postDistFunc12,p2,p1) {
  if(p2 == 1) {
    postDistFunc2 <- deltaDistFunc  
  }
  else if(p1 == 1) {
    #r1 is player 1 - substitute in player 1 single value
    postDistFunc2 <- function(r2) {
      postDistFunc12(R1,r2)
    }
  }
  else {
    postDistFunc2 = function(r2) {
      #integrate over first player ratings
      result <- integrate(postDistFunc12,MIN_RATING,MAX_RATING,r2)
      
      result$value
    }
  }
  
  postDistFunc2
}

calcNewDistFuncs <- function(distFunc1,distFunc2,p1Wins,p1Index,p2Index) {
  #create the 2D post ratings distribution 
  postDistFunc <- getPostDistFunc(distFunc1,distFunc2,p1Wins)
  
  postDistFunc1 <- getSinglePostDistFunc1(postDistFunc,p1Index,p2Index)
  postDistFunc2 <- getSinglePostDistFunc2(postDistFunc,p1Index,p2Index)

  list(distFunc1 = postDistFunc1, distFunc2 = postDistFunc2)
}


runOneWeek <- function(matchFrame,priorDistFuncs) {

  ##############################################################################
  ## for distribution plots
  oldMfrow <- par("mfrow")
  par(mfrow=c(1,2))
  ############################################################################## 
  
  ##start with a copy of the old dist funcs
  postDistFuncs <- priorDistFuncs
  
  #this function process one match, getting updated ratings
  processMatch <- function(p1,p2,p1Wins) {
    
    postInfo = calcNewDistFuncs(distFuncs[[p1]], distFuncs[[p2]], p1Wins, p1Index = p1, p2Index = p2)
    
    plotDistFunc(postInfo$distFunc1,p1)
    plotDistFunc(postInfo$distFunc2,p2)

    postDistFuncs[[p1]] <<- postInfo$distFunc1
    postDistFuncs[[p2]] <<- postInfo$distFunc2
  }
  
  #processMatch for each row of the match data frame
  mapply(processMatch,matchFrame$p1,matchFrame$p2,matchFrame$win1)
  
  ##############################################################################
  ## for distribution plots
  par(mfrow=oldMfrow)
  ##############################################################################  
  
  # return the new ratings matrix
  postDistFuncs
}


##======================================
## Run the simulation
##======================================

##get the simulated players and results
simData <- getSimulatedData(NUM_PLAYERS,NUM_WEEKS)
players <- simData$players
matches <- simData$matches

## initial ratings matrix
##we have the first player hard coded to a single value
R1 <- players[1] 
distFuncs0 = list()
distFuncs0[[1]] <- deltaDistFunc
for(i in 2:NUM_PLAYERS) {
  distFuncs0[[i]] <- uniformDistFunc
}

##evolve the ratings
distFuncs <- list()
distFuncs[[1]] <-  runOneWeek(matches[[1]],distFuncs0)
for(i in 2:NUM_WEEKS) {
  distFuncs[[i]] <- runOneWeek(matches[[i]],distFuncs[[i-1]])
}


#results <- data.frame(player = 1:NUM_PLAYERS,actual = players,meas = ratings[[NUM_WEEKS]]$pm, measdev = ratings[[NUM_WEEKS]]$psd)
#results$err <- results$meas - results$actual
#results$zerr <- results$err / results$measdev

#print(results)




################################################################################
##test

distFunc1 <- distFuncs0[[1]]
distFunc2 <- distFuncs0[[2]]

p1Wins <- TRUE
p1Wins <- FALSE

postDistFunc <- getPostDistFunc(distFunc1,distFunc2,p1Wins)
distFunc1 <- getSinglePostDistFunc1(postDistFunc,1,2)
distFunc2 <- getSinglePostDistFunc2(postDistFunc,1,2)

plotDistFunc(distFunc1,1)
plotDistFunc(distFunc2,2)

pdf2Vec <- function(r) {postDistFunc(r[1],r[2])}

pdf2Vec <- function(r) {
  pMatch(r[1],r[2],p1Wins) * distFunc1(r[1]) * distFunc2(r[2])
}

out <- optim(c(50,50),pdf2Vec)

f <- function(r) {
  - exp(- mean(r*r) )
}

#(multi variable) optimization
out <- optim(c(3,4,2,1),f)

