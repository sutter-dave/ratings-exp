#mu = 3
#sigma = 4
f <- function(x) {
  1/sqrt(2*pi)/4 * exp(-(x - 3)^2/32)
}

result <- integrate(f,lower=-Inf,upper=Inf)

g <- function(x) {
  1/(2*pi*4*6) * exp(-( (x[1]-3)^2/32 + (x[2]-2)^2/72 ))
}

result <- adaptIntegrate(g,lowerLimit=c(-Inf,-Inf),upperLimit=c(Inf,Inf))

