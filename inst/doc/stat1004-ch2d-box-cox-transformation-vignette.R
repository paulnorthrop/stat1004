## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "", prompt = TRUE, collapse = TRUE)
#devtools::load_all()

## ---- include = FALSE----------------------------------------------------
# Set a random number `seed'.  
set.seed(2601)

## ---- fig.show = 'hold', fig.width = 3.4, fig.height = 2.75--------------
library(stat1004)
x <- rlnorm(100)
boxcox_plot(x, density_fn = dlnorm, main = "data and true density function")
boxcox_plot(x, density_fn = dlnorm, lambda = 0, main = "after transformation")

## ---- fig.width = 6.5----------------------------------------------------
library(MASS, warn.conflicts = FALSE)
res <- MASS::boxcox(x ~ 1)
# Find the value of lambda with the largest profile log-likelihood
res$x[which.max(res$y)]

## ---- fig.show = 'hold', fig.width = 3.4, fig.height = 2.75--------------
x2 <- rexp(100)
boxcox_plot(x2, density_fn = dexp, main = "data and true density function")
boxcox_plot(x2, density_fn = dexp, lambda = 1 / 3, main = "after transformation")
boxcox(x2 ~ 1)
abline(v = 1/3, col = "red")

## ------------------------------------------------------------------------
# A function to define the p.d.f.
dtri <- function(x) {
  return(ifelse(x > 0 & x < 1, 2 * x, 0))
}  
# A function to simulate from this distribution
rtri <- function(n = 1) {
  return(sqrt(runif(n)))
}  

## ---- fig.show = 'hold', fig.width = 3.4, fig.height = 2.75--------------
x3 <- rtri(200)
boxcox_plot(x3, density_fn = dtri)
boxcox_plot(x3, density_fn = dtri, lambda = 2)

## ---- fig.show = 'hold', fig.width = 3.4, fig.height = 2.75--------------
boxcox(x3 ~ 1, lambda = seq(0, 4, 1 / 10))
boxcox_plot(x3, density_fn = dtri, lambda = 1.5)

## ---- fig.width = 6.5----------------------------------------------------
x4 <- ox_births$time
boxcox_plot(x4)

## ---- fig.width = 6.5, fig.height = 4.5----------------------------------
xlab <- "population (1000s)"
ylab <- "Buchanan's % vote"
y <- 100 * USelection$buch / USelection$tvot
x <- USelection$npop / 1000
plot(x, y, pch = 16, xlab = xlab, ylab = ylab)

## ------------------------------------------------------------------------
boxcox(y ~ log(x))

## ---- fig.show = 'hold', fig.width = 3.4, fig.height = 2.75--------------
xlab <- "population (1000s)"
ylab <- "Buchanan vote"
plot(x, y, pch = 16, log = "xy", xlab = xlab, ylab = ylab)
ylab <- expression(sqrt(Buchanan~vote))
plot(x, sqrt(y), pch = 16, log = "x", xlab = xlab, ylab = ylab)

