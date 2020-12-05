library(numbers)
library(memoise)
library(useful)

# Return myu function in "Merten's Conjecture"
# (https://www.youtube.com/watch?v=uvMGZb0Suyc)
# Accepts a vector of integers,
# returns vector of myu values.
myu <- function(n) {
  pf <- lapply(n, primeFactors)
  upf <- lapply(pf, unique)
  ifelse(compare.list(pf, upf),
         ifelse(sapply(upf, length) %% 2 == 0, 1, -1),
         0)
}
mem_myu <- memoise(myu)

# Return sum of myu values in range 1..n
msum <- function(n) sum(mem_myu(seq(n)))
xmax <- 2000
x <- seq(xmax)
y <- sapply(x, msum)
plot(x, y, type="l", ylim=c(-40, 40), xlab=NA, ylab="msum",
     main="Merten's Conjecture")

# Draw Merten's boundaries
lines(x, sqrt(x), col="red")
lines(x, -sqrt(x), col="red")
