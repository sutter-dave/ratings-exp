library(dplyr)

## values for calculating a match.
## a normal distribution is used
## The winner is given if the value from the match distribution
## is greater or less than the cutoff - which is the different in 
## rankings of the two players
match_val_mean = 0
match_val_sigma = 3

## create some players
player1 <- list(mu = 10,sigma = 2)
player2 <- list(mu = 12, sigma = 3)

players <- rbind(player1,player2)



getPlayer1WinPercent <- function(deltaRank) {
  1 - pnorm(deltaRank,mean=0,sd=3)
}


##=============================
## simulation
##=============================

num_runs <- 100000

## get the random values for the player rankings and the match values
player1Rs <- rnorm(num_runs,player1$mu,player1$sigma)
player2Rs <- rnorm(num_runs,player2$mu,player2$sigma)

matchValues <- rnorm(num_runs,match_val_mean,match_val_sigma)

sim <- data.frame(p1=player1Rs,p2=player2Rs,mval=matchValues)

## calculate the winner
sim$p1win <- sim$p1 - sim$p2 > sim$mval

##output scenarios - p1 wins and loses

p1Wins <- filter(sim,p1win == TRUE)
p1Loses <- filter(sim,p1win == FALSE)

## the distributions of p1 and p2 in each dataframe gives the distributions
## after the match

p1_win_mean <- mean(p1Wins$p1)
p1_win_sd <- sd(p1Wins$p1)

p1_loses_mean <- mean(p1Loses$p1)
p1_loses_sd <- sd(p1Loses$p1)

p2_win_mean <- mean(p1Loses$p2)
p2_win_sd <- sd(p1Loses$p2)

p2_loses_mean <- mean(p1Wins$p2)
p2_loses_sd <- sd(p1Wins$p2)
                   


