## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "", prompt = TRUE, collapse = TRUE)
#devtools::load_all()

## ---- include = FALSE----------------------------------------------------
# Set a random number `seed'.  
set.seed(2601)

## ------------------------------------------------------------------------
library(stat1004)
head(aussie_births)

## ------------------------------------------------------------------------
boys <- sum(aussie_births[, "sex"] == "boy")
girls <- sum(aussie_births[, "sex"] == "girl")
p_hat <- boys / (boys + girls)
p_hat

## ---- fig.align='center', fig.width=7------------------------------------
#barplot(dbinom(0:44, 44, p_hat), names.arg = 0:44, width = 0.5, xlim = c(0, 44))
#plot(0:44, dbinom(0:44, 44, p_hat), type = "h")

## ------------------------------------------------------------------------
pbinom

## ---- fig.align='center', fig.width=7------------------------------------
pjn <- rbinom(100000, 44, 1 / 2)
barplot(table(pjn))

