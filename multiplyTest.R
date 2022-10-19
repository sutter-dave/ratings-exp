## Matrix Test File

# This creates a matrix filling, incrementing the first index (rows) first
# A[1,1] = 1; A[2,1] = 2; A[1,2] = 0; A[2,2] = 3
A <- matrix( c(1,2,0,3), nrow=2, ncol=2)
x <- c(1,2)

## Mulitples on the right index of A (columns) and the index of X, then sums
br = A %*% x

## multiples on the left index of A (rows) and the index of x, then sums
bl = x %*% A

## DIFFERENT ORDERING THAN MATRIX MULTIPLICATION!!!
## index-wise multiplication for the FIRST index of A and the index of x
## SAME IN BOTH CASES!
cr = A * x
cl = A * x

## THIS IS HOW WE HAVE TO MULTIPLY ON THE SECOND INDEX OF A
cr2 = t(t(A) * x)