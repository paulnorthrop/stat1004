## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "", prompt = TRUE, collapse = TRUE)
#devtools::load_all()

## ------------------------------------------------------------------------
library(stat1004)

## ------------------------------------------------------------------------
# Print the data to the screen 
shuttle

## ------------------------------------------------------------------------
# The function head() prints only the first 6 lines of the data (useful to see the structure of large datasets)
head(shuttle)

## ------------------------------------------------------------------------
# Tabulate the number of O-rings with thermal distress
table(shuttle[, "damaged"])

## ------------------------------------------------------------------------
# Repeat and assign the output to the vector o_ring_table 
o_ring_table <- table(shuttle[, 3])

## ---- fig.show='hold'----------------------------------------------------
barplot(o_ring_table, xlab = "number of distressed O-rings", ylab = "frequency")
attach(shuttle)
plot(pressure, temperature)

## ------------------------------------------------------------------------
# The opposite of the function attach() is the function detach()
detach(shuttle)

## ---- fig.width = 6, fig.height = 6--------------------------------------
pairs(shuttle[, 3:5])

## ------------------------------------------------------------------------
# Remove the zeros from the O-ring damage data
not_zero <- shuttle$damaged > 0
not_zero

## ---- fig.show='hold', fig.width = 3.4, fig.height = 3.5-----------------
xlim <- range(shuttle$temperature)
xlim
# Plot with no zeros
plot(shuttle$temperature[not_zero], shuttle$damaged[not_zero], xlim = xlim, ylim = c(0, 3), ann = FALSE)
title(xlab = "temperature / deg F", ylab = "number of distressed O-rings")
# Plot of all the data
plot(shuttle$temperature, shuttle$damaged, ann = FALSE)
title(xlab = "temperature / deg F", ylab = "number of distressed O-rings")

## ------------------------------------------------------------------------
# Create a matrix y containing 2 columns:
#   column 1: number of O-rings WITH thermal distress
#   column 2: number of O-rings WITHOUT thermal distress
y <- cbind(shuttle[1:23, 3], 6 - shuttle[1:23, 3])
head(y)
x <- shuttle[1:23, 4]
x

## ------------------------------------------------------------------------
shuttle_fit <- glm(y ~ x, family = binomial)
# Produce a summary of the estimates.  There is a lot of output: only look at the
# numbers in the column headed "Estimate".
summary(shuttle_fit)
alpha_hat <- shuttle_fit$coefficients[1]
beta_hat <- shuttle_fit$coefficients[2]

## ---- fig.show='hold'----------------------------------------------------
temp <- seq(from = 30, to = 85, by = 0.1)
linear_predictor <- alpha_hat + beta_hat * temp
fitted_curve <- exp(linear_predictor) / (1 + exp(linear_predictor))

## ---- fig.width = 6, fig.height = 5--------------------------------------
plot(shuttle$temperature, shuttle$damaged / 6, ann = FALSE, ylim = c(0, 1))
title(xlab = "temperature / deg F", ylab = "proportion of distressed O-rings")
lines(temp, fitted_curve)

## ---- fig.width = 6, fig.height = 5---------------------------------------------
repeated_data <- which(duplicated(shuttle[, 3:4]))
shuttle[repeated_data, ]
new_damaged <- shuttle$damaged
new_damaged[c(11, 13, 17, 22)] <- new_damaged[c(11, 13, 17, 22)] + 0.2  
new_damaged[15] <- new_damaged[15] - 0.2  
plot(shuttle$temperature, new_damaged / 6, ann = FALSE, ylim = c(0, 1), pch = 16)
title(xlab = "temperature (deg F)", ylab = "proportion of distressed O-rings")
lines(temp, fitted_curve)
legend("topright", legend = c("sample proportions", "fitted curve"), pch=c(16, -1), lty = c(-1, 1))
abline(v = 31, lty = 2)

## ---- include = FALSE-----------------------------------------------------------
# Set a random number `seed'.  
set.seed(1004)

## -------------------------------------------------------------------------------
# Simulate 10 fake datasets of size 23, using the real temperatures.
res <- shuttle_sim(n_sim = 10)
res

## -------------------------------------------------------------------------------
# Simulate the number of distressed O-rings for 1000 launches at 31 deg F.
res <- shuttle_sim(n_sim = 1000, temperature = 31)
res[1:100]
table(res)

## ---- fig.width = 7, fig.height = 5---------------------------------------------
shuttle_sim_plot(n_sim = 50, col = "grey")

## ---- fig.width = 7, fig.height = 5---------------------------------------------
shuttle_sim_plot(n_sim = 50, n_reps = 10, plot_real_data = FALSE, lty = 1)

## ---- fig.width = 7, fig.height = 5---------------------------------------------
shuttle_sim_plot(n_sim = 50, n_reps = 100, plot_real_data = FALSE, lty = 1)

## ---- fig.width = 7, fig.height = 12--------------------------------------------
x <- shuttle_sim_plot(n_sim = 1000, plot = FALSE)
shuttle_sim_hists(x, temps = c(31, 50, 65, 80), col = 8)

