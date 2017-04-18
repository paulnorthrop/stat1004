## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
devtools::load_all() 

## ---- message = FALSE----------------------------------------------------
library(stat1004)
birth_times <- ox_births[, "time"]
sort(birth_times)

## ---- fig.width = 6, fig.height = 4--------------------------------------
ox_ecdf <- ecdf(birth_times)
ox_ecdf(1)
ox_ecdf(2)
2/95
ox_ecdf(4.7)
23/95
ox_ecdf(19)
ox_ecdf(100)

## ---- fig.width = 6, fig.height = 4--------------------------------------
plot(ox_ecdf, main = "", xlab = "time (hours)", ylab = "cumulative relative frequency", pch = 1)

## ---- fig.show='hold', fig.width = 6.5-----------------------------------
hist(birth_times, prob = TRUE, col = 8, main = "", xlab = "time (hours)")
br <- c(seq(from = 0, to = 12, by = 2), 20)
hist(birth_times, breaks = br, col = 8, main = "", xlab = "time (hours)")

## ------------------------------------------------------------------------
ox_tab <- hist(birth_times, plot = FALSE)
ls(ox_tab)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
cum_rel_freq <- cumsum(c(0, ox_tab$counts)) / length(birth_times)
plot(ox_tab$breaks, cum_rel_freq, type = "b", pch = 16, ylab = "cumulative relative frequency", xlab = "time (hours)")
abline(h = 1, lty = 2)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
# adjust plot margins, produce a 2 by 2 array of plots (fill row 1 then row 2)
par(mar = c(4, 4, 1, 1), mfrow = c(2, 2))
# no vertical axis
hist(birth_times, col = 8, prob = TRUE, axes = FALSE, xlab = "time (hours)", ylab = "", main = "")
axis(1, line = 0.5)
# no vertical axis plus rug of points
hist(birth_times, col = 8, prob = TRUE, axes = FALSE, xlab = "time (hours)", ylab = "",main = "")
axis(1, line = 0.5)
rug(birth_times, line = 0.5, ticksize = 0.05)
# non-shaded frequency polygon
n <- length(ox_tab$mids)
ox_tab$mids <- c(ox_tab$mids[1], ox_tab$mids, ox_tab$mids[n])
ox_tab$density <- c(0, ox_tab$density, 0)
plot(ox_tab$mids, ox_tab$density, xlab = "time (hours)", ylab = "density", type = "l", las = 1) 
axis(1, line = 0)
# shaded frequency polygon with no vertical axis
plot(ox_tab$mids, ox_tab$density, xlab="time (hours)", ylab = "", type = "l", bty= "l", axes = FALSE)
axis(1, line = -0.4)
polygon(ox_tab$mids, ox_tab$density, col = 8)

## ------------------------------------------------------------------------
# The default plot
stem(birth_times)
# The plot that appears in the notes
stem(birth_times, scale = 2)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
stripchart(birth_times, method = "stack", pch = 16, at = 0, offset = 2/3)
title(xlab = "time (hours)")
x <- round(birth_times)
stripchart(x, method = "stack", pch = 16, at = 0, axes = FALSE, offset = 2/3)
title(xlab = "time (hours)")
x_labs <- c(min(x), pretty(x), max(x))
axis(1, at = x_labs)

