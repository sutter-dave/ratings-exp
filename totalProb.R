## Full Maximal Probability Model

############################
## NOTES 11/21/22
## - I commented out the code that used the previous solution to 
##   find the next solution. This made results worse. That is a problem
##   that needs to be fixed.  I think noise changes the true max value
##   and when we look too long we find it. We need to make it less sensitive
##   to this. Maybe fit the peak based on distributed values.
## We can do more to clean this up. 
## - rewrite the equation. nuemrator in particular ln(exp(x)) -> x!
## - use derivitives
## - at some point maybe expand solution around a fixed point (such as
##   the initial guess, if we are close enough?)
############################

library(dplyr)

source("matchUtil.R")

######################################################################
## WRAPPER TO RETURN MODEL
## (FIGURE OUT THE PROPER WAY TO PROTECT THESE LATER)
## This wraps all the code that generates the model. I think I want
## to make a package instead
getTotalProb <- function() {
  
######################################################################
  
LOWER_BOUND <- MIN_RATING - (MAX_RATING - MIN_RATING)/2
UPPER_BOUND <- MAX_RATING + (MAX_RATING - MIN_RATING)/2

## this gives the derivitive of the math result probability with respect to r
## given r and r2 and result win (TRUE or FALSE)
## (This function is vectorized)
gradLogP_impl <- function(r,rOpp,win) {
  e1 <- exp(r/PROB_CONSTANT)
  e2 <- exp(rOpp/PROB_CONSTANT)
  
  ( (win * e1) / ( win * e1 + (!win) * e2) - e1 / (e1 + e2) ) / PROB_CONSTANT
}

## This gives the contribution to the derivitive of the log of the probability
## with respest to pr, given the ratings r1 (for p1) r2 (for p2) and p1Wins
## (This function is vectorized)
gradLogP <- function(r1,r2,p1Wins,p1,p2,pr) {
  (pr == p1) * gradLogP_impl(r1,r2,p1Wins) + (pr == p2) * gradLogP_impl(r2,r1,!p1Wins)
}

## This gets the vector or probabilities for the different match outcomes
## (This function is vectorized)
getFrameProbVec <- function(rFull,matches) {
  pMatch(rFull[matches$p1],rFull[matches$p2],matches$win1)
}

## This returns the negative of the log of the probability for all match outcomes
## for this value of rFree (the ratings vector excluding player 1, which is fixed)
getFrameLogProb <- function(rFree,matches,r1) {
  rFull <- c(r1,rFree)  
  -sum(log(getFrameProbVec(rFull,matches)))
}

SMOOTH_HCNT = 500
SMOOTH_SD <- 5
getFrameLogProbSmoothed <- function(rFree,matches) {
  val <- getFrameLogProb(rFree,matches) / (2 * SMOOTH_HCNT + 1)
  for(i in 1:SMOOTH_HCNT) {
    rCloud = rnorm(length(rFree),0,SMOOTH_SD)
    val = val + getFrameLogProb(rFree + rCloud,matches) / (2 * SMOOTH_HCNT + 1)
    val = val + getFrameLogProb(rFree - rCloud,matches) / (2 * SMOOTH_HCNT + 1)
  }
  val
}

## this gets the gradient of the negative log of the probability 
## for all match outcomes for this rFree (the ratings vector excluding player 1, which is fixed)
getFrameLogProbGrad <- function(rFree,matches,r1) {
  rFull <- c(r1,rFree)  
  
  p1 <- matches$p1
  p2 <- matches$p2
  r1 <- rFull[p1]
  r2 <- rFull[p2]
  p1Wins <- matches$win1
  
  gradVec = numeric()
  for(p_grad in 2:NUM_PLAYERS) {
    gradVec[p_grad-1] <- -sum(gradLogP(r1,r2,p1Wins,p1,p2,p_grad))
  }
  gradVec
}


## This calculates ratings given a set of matches and an initial ratings guess
calculateRatings <- function(matches,initialRs) {
  ##solve only for the free variables (player one rating is fixed)
  r1 <- initialRs[1]
  initialFreeRs <- initialRs[2:length(initialRs)]
  
  if(length(initialFreeRs) == 1) {
    ## must use brent for 1d solution
    out <- optim(initialFreeRs,getFrameLogProb,matches=matches,r1=r1,method="Brent",lower=LOWER_BOUND,upper=UPPER_BOUND)
  }
  else {
    out <- optim(initialFreeRs,getFrameLogProb,matches=matches,r1=r1)
  }
  
  print(out$par)
  if( (out$convergence == 0) || (max(out$par) > UPPER_BOUND) || (min(out$par) < LOWER_BOUND) )  {
    rs <- initialRs
    print(sprintf("NO CONVERGENCE: %i %i",length(matches$p1),out$convergence))
  }
  else {
    rs <- c(r1,out$par)
    print(sprintf("convergence: %i %i",length(matches$p1),out$convergence))
  }
  
  rs
}


##=========================
## For the incremental interface as in the test
##=========================

getInitialRatings <- function(numPlayers,r1) {
  ##start with everyone at the same ratings, r1
  list(prs=rep(r1,numPlayers),prsds=rep(NA,NUM_PLAYERS))
}


processMatches <- function(matchFrame,priorRatings) {
  #combine all matches into one frame
  #we will carrry around all the matches, since each calc requires all of them
  if("matches" %in% names(priorRatings)) {
    matches <- rbind(priorRatings$matches,matchFrame)
  }
  else {
    matches <- matchFrame
  }
  
  #prs <- calculateRatings(matches,priorRatings$prs)
  prs <- calculateRatings(matches,rep(priorRatings$prs[1],length(priorRatings$prs)))
  
  list(prs=prs,prsds=priorRatings$prsds,matches=matches)
}

##=======================================
## Model Object
##=======================================


######################################################################
## END OF WRAPPER TO RETURN MODEL
totalProbModel <- list()
totalProbModel$getInitialRatings <- getInitialRatings
totalProbModel$processMatches <- processMatches

totalProbModel$name <- "Total Probability"
totalProbModel$hasSD <- FALSE

totalProbModel
}
######################################################################

################################################################################
################################################################################
################################################################################
dontRUn <- function(dummy) {

##get the simulated players and results
simData <- getSimulatedData(NUM_PLAYERS,NUM_WEEKS)
players <- simData$players
matches <- simData$matches

R0 <- players[1]

##matches to matrix (converts logical var to numeric)
#t(matrix(unlist(matches),nrow=7))

oneFrame <- data.frame(p1 = unlist(lapply(matches,function(row) row$p1)),
                        p2 = unlist(lapply(matches,function(row) row$p2)),
                        win1 = unlist(lapply(matches,function(row) row$win1)))

##use brent (only) if there are only two players!!
out <- optim(rep(R0,NUM_PLAYERS-1),getFrameLogProb,oneFrame=oneFrame,method="Brent",lower = -100,upper = 200)

meas <- c(R0,out$par)
results <- data.frame(player = 1:NUM_PLAYERS, actual = players, meas = meas, err = meas - players)
print(results)
print(sum(results$err^2))

################################################################################

  
outS <- optim(rep(R0,NUM_PLAYERS-1),getFrameLogProbSmoothed,oneFrame=oneFrame)
outS1 <- optim(outS$par,getFrameLogProbSmoothed,oneFrame=oneFrame)
outS2 <- optim(outS1$par,getFrameLogProbSmoothed,oneFrame=oneFrame)
outS3 <- optim(outS2$par,getFrameLogProbSmoothed,oneFrame=oneFrame)

out1 <- optim(out$par,getFrameLogProb,oneFrame=oneFrame)
out2 <- optim(out1$par,getFrameLogProb,oneFrame=oneFrame)
out3 <- optim(out2$par,getFrameLogProb,oneFrame=oneFrame)

players[2:NUM_PLAYERS]
out$par
outS$par
outS1$par
outS2$par
outS3$par
out1$par
out2$par
out3$par


sum((players[2:NUM_PLAYERS] - out$par)^2)
sum((players[2:NUM_PLAYERS] - outS$par)^2)
sum((players[2:NUM_PLAYERS] - out1$par)^2)
sum((players[2:NUM_PLAYERS] - outS1$par)^2)
sum((players[2:NUM_PLAYERS] - out2$par)^2)
sum((players[2:NUM_PLAYERS] - outS2$par)^2)
sum((players[2:NUM_PLAYERS] - out3$par)^2)
sum((players[2:NUM_PLAYERS] - outS3$par)^2)

out$value
outS$value
outS1$value
outS2$value
outS3$value
out1$value
out2$value
out3$value



#out <- optim(rep(R0,NUM_PLAYERS-1),getFrameLogProb,oneFrame=oneFrame)
#out2 <- optim(out$par,getFrameLogProb,oneFrame=oneFrame)
#out3 <- optim(out2$par,getFrameLogProb,oneFrame=oneFrame)
#out4 <- optim(out3$par,getFrameLogProb,oneFrame=oneFrame)

#meas <- c(R0,out4$par)
#results <- data.frame(player = 1:NUM_PLAYERS, actual = players, meas = meas, err = meas - players)
#print(results)
#print(sum(results$err^2))

#print("just first iter:")
##In the ones I have looked at, the first iteration does better
##maybe more searching starts looking at the machine error structure

#meas <- c(R0,out$par)
#results <- data.frame(player = 1:NUM_PLAYERS, actual = players, meas = meas, err = meas - players)
#print(results)
#print(sum(results$err^2))

control = list(reltol=1e-4)
out0 <- optim(rep(R0,NUM_PLAYERS-1),getFrameLogProb,oneFrame=oneFrame,control=control)
outa <- optim(out0$par,getFrameLogProb,oneFrame=oneFrame,control=control)
outb <- optim(outa$par,getFrameLogProb,oneFrame=oneFrame,control=control)
outc <- optim(outb$par,getFrameLogProb,oneFrame=oneFrame,control=control)
outd <- optim(outc$par,getFrameLogProb,oneFrame=oneFrame,control=control)

meas <- c(R0,out0$par)
results <- data.frame(player = 1:NUM_PLAYERS, actual = players, meas = meas, err = meas - players)
print(results)
print(sum(results$err^2))

meas <- c(R0,outa$par)
results <- data.frame(player = 1:NUM_PLAYERS, actual = players, meas = meas, err = meas - players)
print(results)
print(sum(results$err^2))

meas <- c(R0,outb$par)
results <- data.frame(player = 1:NUM_PLAYERS, actual = players, meas = meas, err = meas - players)
print(results)
print(sum(results$err^2))

meas <- c(R0,outc$par)
results <- data.frame(player = 1:NUM_PLAYERS, actual = players, meas = meas, err = meas - players)
print(results)
print(sum(results$err^2))

meas <- c(R0,outd$par)
results <- data.frame(player = 1:NUM_PLAYERS, actual = players, meas = meas, err = meas - players)
print(results)
print(sum(results$err^2))

#players

#meas = c(R0,out$par)

#meas - players

#meas <- c(R0,out4$par)
#err2 <- sum( (meas - players)^2 )

#outA <- optim(players[2:NUM_PLAYERS],getFrameLogProb,oneFrame=oneFrame)

#pvec <- getFrameProbVec(out2$par,oneFrame)





##======================================================================
## dev

plotDist<- function(rvec,oneFrame,index) {
  rFree <- rvec[2:NUM_PLAYERS]
  rIndex <- rFree[index-1]
  
  x <- numeric()
  y <- numeric()
  for(i in 1:100) {
    x[i] <- rIndex + (i-50)/1
    rFree[index-1] <- x[i]
    y[i] <- getFrameLogProb(rFree,oneFrame)
  }
  
  plot(x,y,t="l")
  abline(h=out$value + 1)
  abline(v=out$par[index-1],col="blue")
  abline(v=players[index],col="red")
}

par(mfrow=c(2,4))
plotDist(c(R0,out$par),oneFrame,2)
plotDist(c(R0,out$par),oneFrame,3)
plotDist(c(R0,out$par),oneFrame,4)
plotDist(c(R0,out$par),oneFrame,5)
plotDist(c(R0,out$par),oneFrame,6)
plotDist(c(R0,out$par),oneFrame,7)
plotDist(c(R0,out$par),oneFrame,8)
plotDist(c(R0,out$par),oneFrame,9)




##1D (2 players)==========================================================
#optimize(getFrameLogProb,c(1,100),oneFrame)


#x = numeric()
#y = numeric()
#for(i in 1:100) {
#  x[i] = i
#  y[i] = getFrameLogProb(i,oneFrame)
#}
#plot(x,y)

#pMatch <- function(r1,r2,p1Wins) 
#gradLogP <- function(r1,r2,p1Wins,p1,p2,pr)

## test 1

testing <- function(dummy) {
v1 <- pMatch(30,50,TRUE)
v2 <- pMatch(30.02,50,TRUE)

dv <- gradLogP_impl(30.01,50,TRUE)
dvf <- gradLogP(30.01,50,TRUE,1,2,1)

dvest = (log(v2) - log(v1) ) / .02

## test 2

v1 <- pMatch(30,50,FALSE)
v2 <- pMatch(30.02,50,FALSE)

dv <- gradLogP_impl(30.01,50,FALSE)
dvf <- gradLogP(50,30.01,TRUE,1,2,2)

dvest = (log(v2) - log(v1) ) / .02


rFree0 <- out0$par
valFree0 <- out0$value

rFree1 <- rFree0
rFree1[2] <- rFree0[2] + 1

rFreeM <- rFree0
rFreeM[2] <- rFree0[2] + .5

testOut0 <- getFrameLogProb(rFree0,oneFrame)
testOut1 <- getFrameLogProb(rFree1,oneFrame)

testOut1 - testOut0

testGrad <- getFrameLogProbGrad(rFreeM,oneFrame)


getFrameLogProb(rFree,oneFrame)
getFrameLogProbGrad(rFree,oneFrame)



out0 <- optim(rep(R0,NUM_PLAYERS-1),getFrameLogProb,oneFrame=oneFrame)
out1 <- optim(rep(R0,NUM_PLAYERS-1),getFrameLogProb,gr=getFrameLogProbGrad,oneFrame=oneFrame,method="BFGS")
out2 <- optim(rep(R0,NUM_PLAYERS-1),getFrameLogProb,gr=getFrameLogProbGrad,oneFrame=oneFrame,method="CG")
out3 <- optim(rep(R0,NUM_PLAYERS-1),getFrameLogProb,gr=getFrameLogProbGrad,oneFrame=oneFrame,method="L-BFGS-B")

abfunc <- function(rFree,oneFrame) {
  sum(abs(getFrameLogProbGrad(rFree,oneFrame)))
}

outA <- optim(rep(R0,NUM_PLAYERS-1),abfunc,oneFrame=oneFrame)



}
}
