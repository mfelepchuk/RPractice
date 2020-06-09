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
  h = hist(z, breaks = 30, xlim=c(0,1), col = 'lightblue2')
  h
  return(list("x" = x, "y" = y, "z" = z, "hist" = h))
}


#what is the probability that sign up rate between 30% and 40%
sum(e$z <.4 & e$z >.3)/length(e$z)

#Maximum Likelihood Estimator:
median(e$z)

## A/B Test

A <- bayes(16, 1000000,6)
B <- bayes(16, 1000000,10)

X = 0
for (i in 1:length(A$y)){
  X[i] = A$y[i] == 6 && B$y[i] == 10
  }
X = as.logical(X)

df <- data.frame('rateA'=A$x[X], 'rateB' = B$x[X])
df$diff = df$rateB- df$rateA
df$profitA = 1000*df$rateA - 30
df$profitB = 1000*df$rateB - 300
df$profit_diff = df$profitB - df$profitA
hist(df$profit_diff, breaks=30)

mean(df$profit_diff < 0)

# New informative prior

bayes <- function(sz, iterations, target){
  x <- rbeta(iterations,3,25)
  y <- x
  for (i in 1:length(x)){
    y[i] <- sum(rbinom(sz,1,x[i]))
  }
  z = x[y==target]
  h = hist(z, xlim=c(0,1), col = 'lightblue2')
  h
  return(list("x" = x, "y" = y, "z" = z, "hist" = h))
}

e = bayes(16,100000,6)
