## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "", prompt = TRUE, collapse = TRUE)
devtools::load_all()

## ---- message = FALSE----------------------------------------------------
library(stat1004)
birth_times <- ox_births[, "time"]
day <- ox_births[, "day"]

## ---- message = FALSE----------------------------------------------------
ox_mat <- matrix(NA, ncol = 7, nrow = 16)
for (i in 1:7) {
  day_i_times <- ox_births$time[which(ox_births$day == i)]
  ox_mat[1:length(day_i_times), i] <- sort(day_i_times)
  colnames(ox_mat) <- paste("day", 1:7, sep = "")
}  
ox_mat

## ---- fig.show = 'hold', fig.width = 3.4, fig.height = 2.75--------------
par(mar = c(4, 4, 0.5, 1))
xlab <- "time (hours)"
x_labs <- c(min(birth_times), pretty(birth_times), max(birth_times))
# top left
box_plot(birth_times~day, col = 8, ylab = xlab, pch = 16, xlab = "day")
# top right
box_plot(birth_times~day, col = 8, horizontal = TRUE,  axes = FALSE, xlab = xlab, ylab = "day", pch = 16)
axis(1, at = x_labs, labels = x_labs)
axis(2, at = 1:7, labels = 1:7, lwd = 0, lty = 0)
# bottom left
box_plot(birth_times~day, axes = FALSE, ylab = xlab, pch = 16, lty = 1, range = 0, boxcol = "white", staplewex = 0, medlty = "blank", medpch = 16, xlab = "day")
axis(1, at = 1:7, labels = 1:7, lwd = 0, lty = 0)
axis(2, at = x_labs, labels = x_labs)
# bottom right
box_plot(birth_times~day, horizontal = TRUE, axes = FALSE, xlab = xlab, pch = 16, lty = 1, range = 0, boxcol = "white", staplewex = 0, medlty = "blank", medpch = 16)
axis(1, at = x_labs, labels = x_labs)
axis(2, at = 1:7, labels = 1:7, lwd = 0, lty = 0, las = 1)

## ------------------------------------------------------------------------
# County identifiers and location
head(USelection[, 1:4])
# County demographic variables
head(USelection[, 5:12])
# Numbers of votes for candidates
head(USelection[, 13:22])

## ---- fig.width = 6, fig.height = 5--------------------------------------
plot(-USelection[, "lon"], USelection[, "lat"], xlab = "longitude (degrees north)", ylab = "latitude (degrees east)", pch = 16)

## ---- fig.width = 6, fig.height = 5--------------------------------------
pbuch <- 100*USelection$buch/USelection$tvot
is_PB <- USelection[, "co_names"] == "PalmBeach"
pch <- 1 + 3 * is_PB
pch
plot(USelection$npop, pbuch, xlab = "population", ylab = "Buchanan % vote", pch = pch)
which_PB <- which(is_PB)
text(USelection[which_PB, "npop"], pbuch[which_PB] + 0.1, "Palm Beach", cex = 0.8)

## ---- fig.width = 7, fig.height = 7--------------------------------------
pairs(USelection[, 5:12])

## ---- fig.width = 7, fig.height = 7--------------------------------------
x <- USelection$npop / 1000
y <- sqrt(pbuch)
ystring <- expression(sqrt("% Buchanan vote"))
rm_PB <- which(!is_PB)
scatter(x[rm_PB], y[rm_PB], pch = 16, xlab ="Total Population (1000s)", ylab = ystring, log = "x")
points(x[which_PB], y[which_PB], pch = "X")
text(x[which_PB], y[which_PB] + 0.04, "Palm Beach", cex = 0.8)

## ---- fig.width = 6, fig.height = 6--------------------------------------
scatter_hist(x, y, log = "x", pch = 16, xlab ="Total Population (1000s)", ylab = ystring)

## ---- fig.width = 6, fig.height = 6--------------------------------------
logx <- log(x)
xbreaks <- seq(from = min(logx), to = max(logx), len = 25)
ybreaks <- seq(from = min(y), to = max(y), len = 25)
scatter_hist(x, y, log = "x", pch = 16, xlab ="Total Population (1000s)", ylab = ystring, xbreaks = xbreaks, ybreaks = ybreaks)

