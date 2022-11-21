################################
## matchUtil.R
## This file contains some common utilities for matches
################################


MIN_RATING <- 1
MAX_RATING <- 100

PROB_CONSTANT <- 100 / 3

## probability of result p1Wins given r1 and r2
pMatchOLD <- function(r1,r2,p1Wins) {
  xx <- exp( (r1 - r2)/PROB_CONSTANT )
  
  if(p1Wins) {
    xp <- xx / (1 + xx)
  }
  else {
    xp <- 1 / (1 + xx)
  }
  
  xp
}

## This gives the probability of the result p1Wins (TRUE or FALSE)
## given the values of r1 and r2
## (This function is vectorized)
pMatch <- function(r1,r2,p1Wins) {
  e1 <- exp(r1/PROB_CONSTANT)
  e2 <- exp(r2/PROB_CONSTANT)
  (p1Wins * e1 + (!p1Wins) * e2) / (e1 + e2)
}
