## This file runs a test.
source("simData.r")
source("discreteDist.R")
source("eloModel.R")
source("continuousIntegrate.R")
source("continuousMonteCarlo.R")
source("totalProb.R")

##conventions:
##
## ratings object - a list with the following entries
## - prs - (required) player ratings, a vector of numeric values with length NUM_PLAYERS
## - prsds - (optional) player standard deviations, a vector of numeric values with length NUM_PLAYERS
## - (other, implementatin specific)
##
## test object - a list with the following entries
## - getInitialRatings(NUM_PLAYERS,p1r) - args: number of players, player one (fixed) rating, returns ratings object
## - processMatches(matches, ratings) - args: match data frame, ratings object, returns new ratings object
##
## getSimulatedData(numPlayers,numWeeks,seed) - returns simData
##
## simData: list with entries:
## - players - vector of ratings, min and max values defined in matchUtil
## - matches - list of match data frames, one for each week
##
## match data frame - columns
## - p1 - integer value for first player in match
## - p2 - integer value for second player in match
## - pwin1 - probability of p1 winning
## - mrv - a random number from 0 to 1 (to decide who wins)
## - win1 - TRUE is player 1 wins, FALSE if player2 wins
## 
## NOTES
## - for now I am not assuming the test object functions are vectorized.

NUM_PLAYERS <- 10L
NUM_WEEKS <- 100L

## This method takes the ratings output and adds the error from the 
## actaul values  and the summed errors squared. If the standard deviation 
## is present, it does the same for the zerr
evalRatings <-function(ratings,actualRatings) {
  ratings$perrs <- ratings$prs - actualRatings
  ratings$err <- sum(ratings$perrs[2:NUM_PLAYERS]^2)
  if("prsds" %in% names(ratings)) {
    ratings$pzerrs <- ratings$perrs[2:NUM_PLAYERS] / ratings$prsds[2:NUM_PLAYERS]
    ratings$zerr <- sum(ratings$pzerrs^2)
  }
  ratings
}

## This does one iteration step of the model for a given set of matches.
## it also appends the error values to the output ratings object
processMatches <- function(model,matchFrame,priorRatings,actualRatings) {
  postRatings <- model$processMatches(matchFrame,priorRatings)  
  evalRatings(postRatings,actualRatings)
}

fieldToVec <- function(lst,field) {
  sapply(lst,function(entry) entry[[field]])
}

## This function runs a model on a given set of matches
## and actual ratings
runModel <- function(model,matches,actualRatings) {
  
  numPlayers <- length(actualRatings)
  numWeeks <- length(matches)
  
  ## initial ratings
  ratings0 <- model$getInitialRatings(numPlayers,actualRatings[1])
  
  ##evolve the ratings
  ratings = list()
  ratings[[1]] =  processMatches(model,matches[[1]],ratings0,actualRatings)
  for(i in 2:numWeeks) {
    ratings[[i]] = processMatches(model,matches[[i]],ratings[[i-1]],actualRatings)
  }
  
  list(ratings=ratings,model=model)
}

runComparison <- function(models,matches,actualRatings) {
  ##run all the models on the given data
  modelsResults <- lapply(models,runModel,matches=matches,actualRatings=actualRatings)
  
  ##get colors for the models, indexed by name
  modelColors <- palette.colors(length(models),palette="Dark 2")
  names(modelColors) <- names(models)
  
  ## get the plot data for these results
  modelsPlotData <- lapply(modelsResults,collectPlotData,modelColors=modelColors)
  
  errPlotLimits <- getPlotLimits(modelsPlotData,"err")
  
  plot(1:NUM_WEEKS,modelsPlotData[[1]]$err,t="n",
       ylim=errPlotLimits,
       main=sprintf("Err^2: %i players, %i weeks",NUM_PLAYERS,NUM_WEEKS))
  dummy <- lapply(modelsPlotData,plotModelField,field="err")
  legend("topright",col=sapply(modelsPlotData,function(plotData) plotData$col),lwd=2,legend=sapply(modelsPlotData,function(plotData) plotData$name))
  
  zerrPlotLimits <- getPlotLimits(modelsPlotData,"zerr")
  
  plot(1:NUM_WEEKS,modelsPlotData[[2]]$zerr,t="n",
       ylim=zerrPlotLimits,
       main=sprintf("Err^2: %i players, %i weeks",NUM_PLAYERS,NUM_WEEKS))
  dummy <- lapply(modelsPlotData,plotModelField,field="zerr")
  
}

## This collects values from the models results for plotting
collectPlotData <- function(modelResults,modelColors) {
  
  plotData <- list()
  ## error
  plotData$err <- sapply(modelResults$ratings,function(ratingsEntry) ratingsEntry$err)
  
  ## z error
  if(modelResults$model$hasSD) {
    plotData$zerr <- sapply(modelResults$ratings,function(ratingsEntry) ratingsEntry$zerr)
  }
  else {
    plotData$zerr <- rep(NA,NUM_WEEKS)
  }
  
  plotData$name <- modelResults$model$name
  plotData$col <- modelColors[[modelResults$model$name]]
  
  plotData
}

getPlotLimits <- function(modelsPlotData,field) {
  allData <- sapply(modelsPlotData,function(modelPlotData) modelPlotData[[field]])
  c(min(allData,na.rm=TRUE),max(allData,na.rm=TRUE))
}

plotModelField <- function(modelPlotData,field) {
  lines(1:NUM_WEEKS,modelPlotData[[field]],col=modelPlotData$col,lwd=2)
}

#,col=modelPlotData$col,

##======================================
## Run the simulation
##======================================

## load models
models <- list(getEloModel(),getDiscreteDist())
#models <- list(getEloModel(),getDiscreteDist(),getTotalProb())
#models <- list(getEloModel(),getDiscreteDist(),getContinuousMonteCarlo())
#models <- list(getTotalProb())
#getContinuousIntegrate(),
#getContinuousMonteCarlo())

names(models) <- sapply(models,function(mdl) {mdl$name})

##get the simulated players and results
seed = 67
simData = getSimulatedData(NUM_PLAYERS,NUM_WEEKS,seed)

actualRatings = simData$players #actual ratings vector
matches = simData$matches

runComparison(models,matches,actualRatings)


######################################################

DONTRUN <- function() {

lines(1:NUM_WEEKS,,t="l",col=cols[1],lwd=2)

eloRatings <- runModel(eloModel,matches,actualRatings)
discDistRatings <- runModel(discDistModel,matches,actualRatings)
contIntegrateRatings <- runModel(contIntegrateModel,matches,actualRatings)
contMonteCarloRatings <- runModel(contMonteCarloModel,matches,actualRatings)

modelNames = c("Elo","Discreet Dist","Continuous Integrate","Continuous Monte Carlo")
cols <- c("black","red","blue","purple")

maxErr <- max(fieldToVec(eloRatings,"err"),fieldToVec(discDistRatings,"err"),fieldToVec(contIntegrateRatings,"err"),fieldToVec(contMonteCarloRatings,"err"))
minErr <- 0

plot(1:NUM_WEEKS,fieldToVec(eloRatings,"err"),t="n",
     ylim=c(minErr,maxErr),
     main=sprintf("Err^2: %i players, %i weeks",NUM_PLAYERS,NUM_WEEKS))
lines(1:NUM_WEEKS,fieldToVec(eloRatings,"err"),t="l",col=cols[1],lwd=2)
lines(1:NUM_WEEKS,fieldToVec(discDistRatings,"err"),t="l",col=cols[2],lwd=2)
lines(1:NUM_WEEKS,fieldToVec(contIntegrateRatings,"err"),t="l",col=cols[3],lwd=2)
lines(1:NUM_WEEKS,fieldToVec(contMonteCarloRatings,"err"),t="l",col=cols[4],lwd=2)
legend("topright",lwd=2,col=cols,legend=modelNames)

maxZErr <- max(fieldToVec(discDistRatings,"zerr"),fieldToVec(contIntegrateRatings,"zerr"),fieldToVec(contMonteCarloRatings,"zerr"))
minZErr <- 0

plot(1:NUM_WEEKS,fieldToVec(discDistRatings,"zerr"),t="n",
     ylim=c(minZErr,maxZErr),
     main=sprintf("Z Err^2: %i players, %i weeks",NUM_PLAYERS,NUM_WEEKS))
lines(1:NUM_WEEKS,fieldToVec(discDistRatings,"zerr"),t="l",col=cols[2],lwd=2)
lines(1:NUM_WEEKS,fieldToVec(contIntegrateRatings,"zerr"),t="l",col=cols[3],lwd=2)
lines(1:NUM_WEEKS,fieldToVec(contMonteCarloRatings,"zerr"),t="l",col=cols[4],lwd=2)
legend("bottomright",lwd=2,col=cols[2:4],legend=modelNames[2:4])

}



