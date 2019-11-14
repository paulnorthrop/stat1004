# ========================= mean_vs_median_normal_movie =======================

#' Sample mean vs sample median: normally distributed data
#'
#' A movie to compare the sampling distributions of the sample mean
#' and sample median based on a random sample of size \eqn{n} from a
#' standard normal distribution.
#'
#' @param n An integer scalar.  The size of the samples drawn from a
#'   standard normal distribution.
#' @param delta_n A numeric scalar.  The amount by which the value of the
#'   sample size is increased/decreased after one click of the +/- button.
#' @param pos A numeric integer.  Used in calls to \code{\link{assign}}
#'   to make information available across successive frames of a movie.
#'   By default, uses the current environment.
#' @param envir An alternative way (to \code{pos}) of specifying the
#'   environment. See \code{\link{environment}}.
#' @details The movie is based on simulating repeatedly samples of size
#'   \code{n} from a standard normal N(0,1) distribution.  It contains
#'   three plots.  The top plot contains a histogram of the most recently
#'   simulated dataset, with the N(0,1) probability density function (p.d.f.)
#'   superimposed.
#'
#'   Each time a sample is simulated the sample mean and sample median are
#'   calculated.  These values are indicated on the top plot as a filled
#'   circle: a red circle for the sample mean and a blue circle for the
#'   sample median; and are added to the values plotted in the middle and
#'   bottom plots.
#'
#'   The plot in the middle contains a histogram of
#'   the sample means of \emph{all} the simulated samples.  The p.d.f.s of
#'   the sampling distributions of the sample mean (thick red line)
#'   and the sample medians (thin blue line) are superimposed.
#'
#'   The plot on the bottom contains a histogram of
#'   the sample medians of \emph{all} the simulated samples.  The p.d.f.s of
#'   the sampling distributions of the sample mean (thin red line)
#'   and the sample medians (thick blue line) are superimposed.
#'
#'   An extra sample is produced by clicking the + button next to
#'   "number of samples of size n". [Ignore the - button.]
#'   The value of the sample size can be changed using the +/-
#'   buttons in the panel.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @seealso  \code{\link{mean_vs_median_t_movie}}: compares the
#'   sampling distributions of the sample mean and sample median based
#'   on a random sample of size \eqn{n} from a Student's t distribution.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' # Produce movie using values from the Aussie births data
#' \dontrun{
#' mean_vs_median_normal_movie()
#' }
#' @export
mean_vs_median_normal_movie <- function(n = 10, delta_n = 1, pos = 1,
                                        envir = as.environment(pos)) {
  # Assign variables to an environment so that they can be accessed inside
  # mean_vs_median_normal_plot()
  old_n <- 0
  assign("old_n", old_n, envir = envir)
  # Create buttons for movie
  nsim <- 1
  mean_vs_median_panel <- rpanel::rp.control("sample size", n = n, nsim = 1,
                                             ntop = 1000, envir = envir)
  rpanel::rp.doublebutton(mean_vs_median_panel, n, delta_n, range = c(1, 1000),
                          repeatinterval = 20, initval = n,
                          title = "sample size, n",
                          action = mean_vs_median_normal_plot)
  rpanel::rp.doublebutton(mean_vs_median_panel, nsim, 1, range = c(1, 10000),
                          repeatinterval = 20, initval = 1,
                          title = "number of samples of size n",
                          action = mean_vs_median_normal_plot)
  rpanel::rp.do(mean_vs_median_panel, mean_vs_median_normal_plot)
  return(invisible())
}

# Function to be called by mean_vs_median_normal_movie().

mean_vs_median_normal_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    graphics::par(mfrow = c(3, 1), oma = c(0, 0, 0, 0),
                  mar = c(4, 4, 1, 2) + 0.1, cex.axis = 1.5, cex.lab = 1.5)
    mu <- 0
    sigma <- 1
    y <- stats::rnorm(n,mean=mu,sd=sigma)
    mean.y <- mean(y)
    if (nsim==1 | n!=old_n) {
      sample.means <- mean(y)
      nsim <- 1
    } else {
      sample.means <- c(sample.means, mean(y))
    }
    assign("sample.means", sample.means, envir = envir)
    median.y <- stats::median(y)
    if (nsim == 1 | n != old_n) {
      sample.medians <- stats::median(y)
      nsim <- 1
    }
    else {
      sample.medians <- c(sample.medians,median(y))
    }
    assign("sample.medians", sample.medians, envir = envir)
    h.low <- -2.5; h.up <- 2.5
    br <- seq(from = h.low, to = h.up, by = 0.5)
    ytop <- stats::dnorm(0, sd = sigma) * 1.6
    y <- y[y > h.low & y < h.up]
    br2 <- sort(c(seq(from = floor(h.low), to = ceiling(h.up), by = 1),mu))
    ### histogram with rug
    graphics::hist(y, col = 8, probability = TRUE, las = 1, breaks = br,
                   axes = FALSE, xlab = "raw data", ylab = "density",
                   main = "", ylim = c(0, ytop))
    graphics::axis(2)
    graphics::axis(1, at = br2, labels = br2, line = 0.5)
    graphics::rug(y,line = 0.5, ticksize = 0.05)
    graphics::title(paste("sample size, n = ",n))
    graphics::curve(stats::dnorm(x, mean = mu, sd = sigma), from = h.low,
                    to = h.up, n = 500, bty ="l", ylab = "density", las = 1,
                    xpd = TRUE, lwd = 3, add = TRUE, lty = 2)
    u <- graphics::par("usr")
    graphics::legend(u[2], u[4], legend = expression(paste("N(0,",1,")")),
           lty = 2, lwd = 3, xjust = 1, cex = 1.5)
    graphics::segments(mean.y, 0, mean.y, -10, col = "red", xpd = TRUE, lwd = 2)
    graphics::points(mean.y, 0,pch = 16, col = "red", cex = 2)
    graphics::segments(median.y, 0, median.y, -10, col = "blue", xpd = TRUE, lwd = 2)
    graphics::points(median.y, 0, pch = 16, col = "blue", cex = 2)
    ytop <- dnorm(0, sd = sigma / sqrt(n)) * 1.1
    if (n <= 25) {
      my.by <- 0.1
    }
    if (n > 25) {
      my.by <- 0.05
    }
    my.by.2 <- 1
    # Means
    y <- sample.means
    y <- y[y > h.low & y < h.up]
    br <- seq(from = h.low, to = h.up, by = my.by)
    br2 <- sort(c(seq(from = floor(h.low), to = ceiling(h.up), by = my.by.2),
                  mu))
    ### histogram with rug
    graphics::hist(y, col = 8, probability = TRUE, las = 1, breaks = br,
                   axes = FALSE, xlab = "sample mean", ylab = "density",
                   main = "", ylim = c(0,ytop), xpd = TRUE)
    graphics::axis(2)
    graphics::axis(1, at = br2, labels = br2, line = 0.5)
    graphics::rug(y, line = 0.5, ticksize = 0.05, col = "red")
    graphics::curve(stats::dnorm(x, mean = mu, sd = sigma / sqrt(n)),
                    from = h.low, to = h.up, n = 500, bty="l",
                    ylab = "density", las = 1, xpd = TRUE, lwd = 3,
                    add = TRUE, lty = 2, col = 2)
    graphics::curve(stats::dnorm(x, mean = mu,
                                 sd = sqrt(1.57) * sigma / sqrt(n)),
                    from = h.low, to = h.up, n = 500, bty = "l",
                    ylab = "density",las = 1, xpd = TRUE, lwd = 1.5,
                    add = TRUE, lty = 2, col = 4)
    u <- graphics::par("usr")
    graphics::legend(u[2], u[4], legend = c("N(0,1/n)", "N(0,1.57/n)"),
                     lty = 2, lwd = c(3, 1.5), xjust = 1, cex = 1.5,
                     col = c(2,4))
    graphics::arrows(mean.y, 2 * ytop, mean.y, 0, col = "red", lwd = 2,
                     xpd = TRUE)
    graphics::segments(median.y, 2 * ytop, median.y, -10, col = "blue",
                       xpd = TRUE, lwd = 2)
    # Medians
    y <- sample.medians
    y <- y[y > h.low & y < h.up]
    br <- seq(from = h.low, to = h.up, by = my.by)
    br2 <- sort(c(seq(from = floor(h.low), to = ceiling(h.up), by = my.by.2),
                  mu))
    ### histogram with rug
    graphics::hist(y, col = 8, probability = TRUE, las = 1, breaks = br,
                   axes = FALSE, xlab = "sample median", ylab = "density",
                   main = "", ylim = c(0, ytop), xpd = TRUE)
    graphics::axis(2)
    graphics::axis(1, at = br2, labels = br2, line = 0.5)
    graphics::rug(y, line = 0.5, ticksize = 0.05, col = "blue")
    graphics::curve(stats::dnorm(x, mean = mu,
                                 sd = sqrt(1.57) * sigma / sqrt(n)),
                    from = h.low, to = h.up, n = 500, bty = "l",
                    ylab = "density", las = 1, xpd = TRUE, lwd = 3,
                    add = TRUE, lty = 2, col = 4)
    graphics::curve(stats::dnorm(x, mean = mu, sd = sigma/sqrt(n)),
                    from = h.low, to = h.up, n = 500, bty = "l",
                    ylab = "density", las = 1, xpd = TRUE, lwd = 1.5,
                    add = TRUE, lty = 2, col = 2)
    u <- graphics::par("usr")
    graphics::legend(u[2], u[4], legend = c("N(0,1.57/n)", "N(0,1/n)"),
                     lty = 2, lwd = c(3, 1.5), xjust = 1, cex = 1.5,
                     col = c(4,2))
    graphics::arrows(median.y, 2 * ytop, median.y, 0 , col = "blue", lwd = 2,
                     xpd = TRUE)
    old_n <- n
    assign("old_n", old_n, envir = envir)
    graphics::par(old_par)
  })
  return(invisible(panel))
}
