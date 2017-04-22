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

