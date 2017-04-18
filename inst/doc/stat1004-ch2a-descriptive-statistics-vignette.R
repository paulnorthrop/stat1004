## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
devtools::load_all() 

## ---- message = FALSE----------------------------------------------------
library(stat1004)
?ox_births

## ------------------------------------------------------------------------
ox_mat <- matrix(NA, ncol = 7, nrow = 16)
for (i in 1:7) {
  day_i_times <- ox_births$time[which(ox_births$day == i)]
  ox_mat[1:length(day_i_times), i] <- sort(day_i_times)
  colnames(ox_mat) <- paste("day", 1:7, sep = "")
}  
ox_mat

## ------------------------------------------------------------------------
birth_times <- ox_births[, "time"]
sort(birth_times)

## ------------------------------------------------------------------------
five_number(birth_times)

## ------------------------------------------------------------------------
summary(birth_times)

## ------------------------------------------------------------------------
five_number(birth_times, type = 7)

## ---- message = FALSE----------------------------------------------------
?quantile

## ------------------------------------------------------------------------
mean(birth_times)

## ------------------------------------------------------------------------
sd(birth_times)
var(birth_times)
sd(birth_times) ^ 2

## ------------------------------------------------------------------------
# Standardized sample skewness
skew(birth_times)
# Sample quartile skewness
q_skew(birth_times)

## ------------------------------------------------------------------------
q_skew(birth_times, type = 7)

## ------------------------------------------------------------------------
five_number(ox_mat, na.rm = TRUE)
summary(ox_mat)
colMeans(ox_mat, na.rm = TRUE)
apply(ox_mat, 2, sd, na.rm = TRUE)
skew(ox_mat, na.rm = TRUE)
q_skew(ox_mat, na.rm = TRUE)

