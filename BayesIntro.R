## Working on examples as I learn Bayesian Analysis from Rasmus Baath's video series on youtube
## https://www.youtube.com/watch?v=3OJEae7Qb_o

## Swedish Fish example
### Of the 16 customers that receieved a brochure, 6 subscribed - what is the expected share of the population that would subsribe?
### run bayes(16, 100000, 6)

bayes <- function(sz, iterations, target){
  # sz is the number of samples in each iteration 
  # iterations is the number of iterations to run 
  # target is the target number of outcomes
  x <- runif(iterations)
  y <- x
  for (i in 1:length(x)){
    y[i] <- sum(rbinom(sz,1,x[i]))
  }
  z = x[y==target]
  hist(z, breaks = 30, xlim=c(0,1), col = 'lightblue2')
}
