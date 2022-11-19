################################
## matchUtil.R
## This file contains some common utilities for matches
################################


MIN_RATING <- 1
MAX_RATING <- 100

PROB_CONSTANT <- 100 / 3

## probability of result p1Wins given r1 and r2
pMatch <- function(r1,r2,p1Wins) {
  xx <- exp( (r1 - r2)/PROB_CONSTANT )
  
  if(p1Wins) {
    xp <- xx / (1 + xx)
  }
  else {
    xp <- 1 / (1 + xx)
  }
  
  xp
}
