## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "", prompt = TRUE, collapse = TRUE)
devtools::load_all()

## ---- include = FALSE----------------------------------------------------
# Set a random number `seed'.  
set.seed(1004)

## ------------------------------------------------------------------------
runif(10)

## ------------------------------------------------------------------------
rbinom(n = 1, size = 6, prob = 0.2)

## ------------------------------------------------------------------------
set.seed(1826)
rbinomial(size = 6, prob = 0.2)

## ------------------------------------------------------------------------
set.seed(1826)
size <- 6
prob <- 0.2
u <- runif(size)
u
distress <- u < prob
distress
n_successes <- sum(distress)
n_successes

## ------------------------------------------------------------------------
as.numeric(distress)

## ---- fig.show='hold'----------------------------------------------------
lambda <- 2
exp_sim <- rexp(n = 1000, rate = lambda)

## ---- fig.width = 6, fig.height = 4--------------------------------------
# A histogram (see Section 2.5 of the STAT1004 notes)
hist(exp_sim, probability = TRUE, ylim = c(0, lambda), main = "")
x <- seq(0, max(exp_sim), len = 500)
lines(x, dexp(x, rate = lambda))

## ---- fig.width = 6, fig.height = 4--------------------------------------
u <- runif(1000)
exp_inv <- -log(u)/lambda
hist(exp_inv, probability = TRUE, ylim = c(0, lambda), main = "", breaks = 14, col = "grey")
lines(x, dexp(x, rate = lambda))

## ---- fig.width = 6, fig.height = 4--------------------------------------
u <- runif(1000)
mu <- 0
sigma <- 1
norm_sim <- qnorm(u, mean = mu, sd = sigma)
hist(norm_sim, probability = TRUE, main = "", col = "grey")
x <- seq(min(norm_sim), max(norm_sim), len = 500)
lines(x, dnorm(x, mean = mu, sd = sigma))

## ---- fig.width = 6, fig.height = 4, message = FALSE---------------------
library(rust)
?ru
# Simulate from a standard normal N(0,1) distribution
ru_sim <- ru(logf = function(x) -x ^ 2 / 2, d = 1, n = 1000, init = 0.1)
# The function ru returns an object of class "ru"
class(ru_sim)
# The default plot method for objects of class "ru" produces a plot to compare the 
# simulated values and the p.d.f. of the distribution from which they are simulated
plot(ru_sim, xlab = "x")

## ---- fig.width = 6, fig.height = 4--------------------------------------
# two-dimensional normal with positive association ----------------
rho <- 0.9
covmat <- matrix(c(1, rho, rho, 1), 2, 2)
log_dmvnorm <- function(x, mean = rep(0, d), sigma = diag(d)) {
  x <- matrix(x, ncol = length(x))
  d <- ncol(x)
  return(- 0.5 * (x - mean) %*% solve(sigma) %*% t(x - mean))
}
ru_sim2 <- ru(logf = log_dmvnorm, sigma = covmat, d = 2, n = 1000, init = c(0, 0))

## ---- fig.width = 6, fig.height = 4--------------------------------------
plot(ru_sim2, xlab = expression(X[1]), ylab = expression(X[2]))

