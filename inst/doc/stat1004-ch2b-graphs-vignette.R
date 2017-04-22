## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "", prompt = TRUE, collapse = TRUE)
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

## ------------------------------------------------------------------------
xlab <- "time (hours)"

## ---- fig.width = 6, fig.height = 4--------------------------------------
plot(ox_ecdf, main = "", xlab = xlab, ylab = "cumulative relative frequency", pch = 1)

## ---- fig.show='hold', fig.width = 6.5-----------------------------------
hist(birth_times, prob = TRUE, col = 8, main = "", xlab = xlab)
br <- c(seq(from = 0, to = 12, by = 2), 20)
hist(birth_times, breaks = br, col = 8, main = "", xlab = xlab)

## ------------------------------------------------------------------------
ox_tab <- hist(birth_times, plot = FALSE)
ls(ox_tab)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
cum_rel_freq <- cumsum(c(0, ox_tab$counts)) / length(birth_times)
plot(ox_tab$breaks, cum_rel_freq, type = "b", pch = 16, ylab = "cumulative relative frequency", xlab = xlab)
abline(h = 1, lty = 2)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
# adjust plot margins, produce a 2 by 2 array of plots (fill row 1 then row 2)
par(mar = c(4, 4, 1, 1), mfrow = c(2, 2))
# no vertical axis
hist(birth_times, col = 8, prob = TRUE, axes = FALSE, xlab = xlab, ylab = "", main = "")
axis(1, line = 0.5)
# no vertical axis plus rug of points
hist(birth_times, col = 8, prob = TRUE, axes = FALSE, xlab = xlab, ylab = "",main = "")
axis(1, line = 0.5)
rug(birth_times, line = 0.5, ticksize = 0.05)
# non-shaded frequency polygon
n <- length(ox_tab$mids)
ox_tab$mids <- c(ox_tab$mids[1], ox_tab$mids, ox_tab$mids[n])
ox_tab$density <- c(0, ox_tab$density, 0)
plot(ox_tab$mids, ox_tab$density, xlab = xlab, ylab = "density", type = "l", las = 1) 
axis(1, line = 0)
# shaded frequency polygon with no vertical axis
plot(ox_tab$mids, ox_tab$density, xlab = xlab, ylab = "", type = "l", bty= "l", axes = FALSE)
axis(1, line = -0.4)
polygon(ox_tab$mids, ox_tab$density, col = 8)

## ------------------------------------------------------------------------
# The default plot
stem(birth_times)

# The plot that appears in the notes
stem(birth_times, scale = 2)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
stripchart(birth_times, method = "stack", pch = 16, at = 0, offset = 2/3)
title(xlab = xlab)
x <- round(birth_times)
stripchart(x, method = "stack", pch = 16, at = 0, axes = FALSE, offset = 2/3)
title(xlab = xlab)
x_labs <- c(min(x), pretty(x), max(x))
axis(1, at = x_labs)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
# type = 6
b1 <- box_plot(birth_times, horizontal = TRUE, main = "type = 6", xlab = xlab)
# type = 7
b2 <- boxplot(birth_times, horizontal = TRUE, main = "type = 7")

## ------------------------------------------------------------------------
as.vector(b1$stats)
as.vector(b2$stats)

## ---- fig.show = 'hold', fig.width = 3.4, fig.height = 2.5---------------
par(mar = c(4, 1, 0.5, 1))
x_labs <- c(min(birth_times), pretty(birth_times), max(birth_times))
# top left
box_plot(birth_times, horizontal = TRUE, col = 8, xlab = xlab, pch = 16)
# top right
boxplot(birth_times, horizontal = TRUE, col = 8, axes = FALSE, xlab = xlab, pch = 16)
axis(1, at = x_labs, labels = x_labs)
# bottom left
boxplot(birth_times, horizontal = TRUE, axes = FALSE, xlab = xlab, pch = 16, lty = 1, range = 0, staplewex = 0)
axis(1, at = x_labs, labels = x_labs)
# bottom right
boxplot(birth_times, horizontal = TRUE, axes = FALSE, xlab = xlab, pch = 16, lty = 1, range = 0, boxcol = "white", staplewex = 0, medlty = "blank", medpch = 16)
axis(1, at = x_labs, labels = x_labs)

## ------------------------------------------------------------------------
shuttle$damaged
O_tab <- table(shuttle$damaged)
O_tab

## ------------------------------------------------------------------------
set.seed(47)
x <- rpois(10, 5)
table(x)

## ------------------------------------------------------------------------
table(c(x, 0:11)) - 1

## ------------------------------------------------------------------------
tab <- table(c(x, 0:11)) - 1
names(tab)[12] <- ">10"
tab

## ------------------------------------------------------------------------
class(shuttle$damaged)

## ------------------------------------------------------------------------
fac_dam <- factor(shuttle$damaged, levels = 0:6)
class(fac_dam)

## ------------------------------------------------------------------------
O_tab_fac <- table(fac_dam)
O_tab_fac

## ---- fig.show = 'hold', fig.width = 6.5, fig.height = 4-----------------
par(mfrow=c(2,2))
par(oma=c(0,0,0,0),mar=c(4,4,1,2)+0.1)
xlab <- "number of damaged O-rings"
ylab <- "frequency"
barplot(O_tab, xlab = xlab, ylab = ylab, las = 1)
barplot(O_tab, space = 1, xlab = xlab, ylab = ylab, las = 1)
barplot(O_tab, space = 1, xlab = xlab, ylab = ylab, las = 1)
abline(h=0)
yy <- as.numeric(O_tab)
xx <- as.numeric(unlist(dimnames(O_tab),use.names=F))
plot(xx, yy, pch = c("0","1","2","3"), axes = FALSE, xlab ="", ylab = ylab, ylim = c(0, 16))
title(xlab="number of damaged o-rings",line=0.25)
axis(2, lty = 1, at = yy, labels = yy, pos = -0.3, las = 1)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
barplot(O_tab_fac, xlab = xlab, ylab = ylab, las = 1)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
blood_types
lab <- paste(blood_types$ABO, substr(blood_types$Rh, 3, 3), sep = "")
barplot(blood_types$percentage, space = 1, xlab = "blood group", ylab = "percentage", las = 1, names.arg = lab)
abline(h=0)

## ---- fig.width = 3, fig.height = 3--------------------------------------
par(mar = c(1, 1, 0, 1))
slices <- rep(c("white","grey66","grey33","black"), 2)
pie(blood_types$percentage, labels = lab, col = slices)

## ------------------------------------------------------------------------
head(ftse)
tail(ftse)

## ------------------------------------------------------------------------
ftse_ts <- ts(ftse$price, frequency = 52, start = c(1984, 14))
head(ftse_ts)
attributes(ftse_ts)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
plot(as.Date(ftse$date), ftse$price, type = "l", ylab = "weekly FTSE 100 share index", xlab = "year")

## ---- fig.width = 6.5, fig.height = 4------------------------------------
plot(ftse_ts, ylab = "FTSE 100 (in 1000s)", xlab = "year", las = 1, axes = FALSE)
q2 <- c(min(ftse_ts), (2:6) * 1000, max(ftse_ts))
axis(2, at = q2, labels = round(q2 / 1000, 1), las = 1)
axis(4, at= q2, labels = round(q2 / 1000, 1), las=1)
axis(1)
abline(h = par("usr")[3])

## ------------------------------------------------------------------------
head(flu)
tail(flu)

## ---- fig.width = 6.5, fig.height = 4------------------------------------
par(mar = c(5, 5, 4, 3) + 0.1, cex.axis = 0.8, cex.lab = 0.75)
flu_ts <- ts(flu[,2], frequency = 13, start = c(1967, 4))
plot(flu_ts, ylab="number of 'flu consultations (in 1000s)", xlab = "year", las = 1, axes=FALSE)
q2 <- c(min(flu_ts),(1:6) * 100, max(flu_ts))
axis(2, at = q2, labels = round(q2, 1), las = 1)
axis(4, at = q2, labels = round(q2, 1), las = 1)
abline(h=0)
axis(1,pos=-1)

