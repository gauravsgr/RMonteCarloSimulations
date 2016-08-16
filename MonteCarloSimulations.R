###################################################################################
#                         Some monte carlo simulations                            #
###################################################################################


## Finding the integration of y = x^2 under the limits 0, 1
n <- 10000
f <- function(x) x^2
plot(runif(n), runif(n), col='blue', pch=20)    # spraying 10k points on the x, y plane ranging b/w 0 & 1
curve(f, 0,1, n=100, col='white', add=TRUE)     # adding the curve for representation

ps <- matrix(runif(2*n), ncol=2)                # creating a matrix of 10K rows and 2 cols
g <- function(x,y) y <= x^2                     # the function for the condition check
z <- g(ps[,1], ps[,2])                          # boolean list to store if the above condition is met by the point
plot(ps[!z,1], ps[!z,2], col='blue', pch=20)
points(ps[z,1], ps[z,2], col='green', pch=20)
curve(f, 0,1, n=100, col='white', add=TRUE)

# the integral is simply the count of all the points under the curve divided by the total number of points
# which is the probability that the point lands under the curve i.e. 33.33% {integral (x^2) => x^3 / 3 => 1/3 - 0/3 => 0.33}
sum(ifelse(z == TRUE, 1, 0)) / n

# Approximation Error #

# In these kind of experiments at least 95% of the time, the error does
# not exceed the reciprocal of the square root of the number of trials
ks <- seq(1,7, by=.2)
g <- function(k) {
  n <- 10^k
  f <- function(x,y) y <= x^2
  z <- f(runif(n), runif(n))
  length(z[z]) / n
}
a <- sapply(ks,g)

head(a) # We see that approximation error reduces as we increase the no. of trials

# Numerical stability: Approximations improve as the no. of iterations increase #

# Plotting out the theoretical limit and the actual errors shows
# that with enough terms the two appear to converge
plot(ks, 1/sqrt(10^ks), type='l')
lines(ks, abs(1/3 - a), col='blue')



## Estimating value of pi
ps <- matrix(runif(2*n), ncol=2)                # creating a matrix of 10K rows and 2 cols
g <- function(x,y) sqrt(x^2 + y^2) <= 1         # the function for the condition check
z <- g(ps[,1], ps[,2])                          # boolean list to store if the above condition is met by the point
plot(ps[!z,1], ps[!z,2], col='blue', pch=20)
points(ps[z,1], ps[z,2], col='green', pch=20)
curve(sqrt(1-x^2), 0,1, n=100, col='white', add=TRUE)

# Since the unit circle has area of pi, a quarter of that will have area pi/4
# Hence, the end result will need to be multiplied by 4 to get the final approx
g <- function(k) {
  n <- 10^k
  f <- function(x,y) sqrt(x^2 + y^2) <= 1
  z <- f(runif(n),runif(n))
  length(z[z]) / n
}
a <- sapply(1:7, g)   # Approximation increases with increase in trials or n
a*4                  

# Improving the numerical stability by doing 100 iterations with a million trials in each
trials <- 4 * sapply(rep(6,100), g)
mean(trials)                                  # Approximate value of pi with a 0.1% precision (due to a million trials)
e <- 1/sqrt(10^6)                             # Error value, roughly 95% of the approximations should have an e less
sum(ifelse(abs(trials - pi)/pi <= e,1,0))/100 # Roughly 95% of the times, we are under the error value calculated above
plot(density(trials))                         # The value of pi hovers at 3.14
hist(trials)



## Simulating the toss of a coin
r <- runif(10^6)
toss <- ifelse(r > .5, 1, -1)                 # if > 0.5, we have a heads and the gambler makes a dollar else he loses a dollar
plot(cumsum(toss), type='l')                  # cummulative loss or gain with each trial
outcomes <- sapply(1:1000, function(i) sum(ifelse(runif(1000) > .5, 1, -1)))
hist(outcomes)



## Simulating the game with the following rules with the roll of a dice
# 1 then lose $2
# 2 loses $1
# 5 wins $1
# 6 wins $2
iterations = 100
profit = c()
for(i in 1:iterations){
  r <- round(runif(10^6,1,6),0)
  value = ifelse(r == 1, -2, ifelse(r == 2, -1, ifelse(r == 5, 1, ifelse(r == 6, 2, 0))))
  profit = c(profit, sum(value))
}
hist(profit)
plot(density(profit))


# References:
# https://math.dartmouth.edu/~prob/prob/prob.pdf
# https://cartesianfaith.com/2013/12/15/probability-and-monte-carlo-methods/   
