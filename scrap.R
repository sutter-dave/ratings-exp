##here I find the probability of a seqeuence n wins in m matches
##for values dr (r1 - r2).
##doign this for different dr gives the distribution if out
##prior was uniform

alpha <- 2

evo <- function(dr,n,m) {
  u <- exp(dr/alpha)
  u^n / (1 + u)^m
}

drVec = -100:100 / 10


dist8 = evo(drVec,6,8)
dist9 = evo(drVec,7,9)
dist9a = evo(drVec,6,9)
plot(drVec,dist8,t="l",col="blue")
points(drVec,dist9,t="l",col="red")
points(drVec,dist9a,t="l",col="green")

m8 = sum(dist8 * drVec) / sum(dist8)
mv8 = sum( dist8 * (drVec - m8)^2) / sum(dist8)
m9 = sum( dist9 * drVec) / sum(dist9)
mv9 = sum( dist9 * (drVec - m9)^2) / sum(dist9)
m9a = sum( dist9a * drVec) / sum(dist9a)
mv9a = sum( dist9a * (drVec - m9a)^2) / sum(dist9a)


dist8a = dist9 + dist9a
points(drVec,dist8a,t="l",col="black")

