## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "", prompt = TRUE, collapse = TRUE)
#devtools::load_all()

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

