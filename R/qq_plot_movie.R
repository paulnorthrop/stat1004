# ================================ qq_plot_movie ==============================

#' Quantile-Quantile plot movie
#'
#' A movie to illustrate the constructions of a normal Quantile-Quantile
#' (QQ) plot.
#'
#' @param data A numeric vector.  Data on which to base the QQ plot.
#' @param mu,sigma Numeric scalars.  The mean and standard deviation of
#'   the normal distribution to which the data \code{data} are to be compared.
#'   If \code{mu} and/or \code{sigma} are not supplied then they are estimated
#'   from the data.
#' @details This movie enables the user to scroll forwards and backwards
#'   through plots that describe the way in which a (normal) QQ plot is
#'   produced.  This movie is designed for simple illustrative examples
#'   with small numbers of observations, that is, for small
#'   \code{length(data)}.  The example below has only 9 observations.
#'   In cases where there are many more observations than this the plots
#'   will be a bit of a mess!
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' \dontrun{
#' # Create the same dataset used in the lecture notes/slides
#' set.seed(382)
#' x <- sort(round(rnorm(9, mean = 7.72, sd = 3.57), 2))
#' qq_plot_movie(data = x, mu = 7.72, sigma = 3.57)
#' }
#' @export
qq_plot_movie <- function(data = NULL, mu = NULL, sigma = NULL) {
  if (is.null(data)) {
    stop("data must be supplied")
  }
  data <- sort(data)
  n <- length(data)
  if (is.null(mu)) {
    mu <- mean(data)
  }
  if (is.null(sigma)) {
    sigma <- stats::sd(data)
  }
  ps <- (1:n) / (n + 1)
  normal_quantiles <- stats::qnorm(ps, mean = mu, sd = sigma)
  nq_labels <- round(100 * ps, 0)
  which_step <- 1
  # Create buttons for movie
  qq_panel <- rpanel::rp.control("QQ plot information", data = data,
                                 normal_quantiles = normal_quantiles,
                                 nq_labels = nq_labels,
                                 mu = mu, sigma = sigma,
                                 which_step = which_step, n = n)
  rpanel::rp.doublebutton(qq_panel, which_step, 1, range = c(1, 8),
                          repeatinterval = 20, initval = 1,
                          title = "forwards (+) / backwards (-)",
                          action = qq_movie_plot)
  rpanel::rp.do(qq_panel, qq_movie_plot)
  return(invisible())
}

# Function to be called by poisson_process_movie().

qq_movie_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    range_x <- stats::qnorm(c(0.01, 0.99), mean = mu, sd = sigma)
    range_x[1] <- floor(range_x[1])
    range_x[2] <- ceiling(range_x[2])
    x_locs <- c(range_x[1], data, range_x[2])
    xlabs <- parse(text = paste("x[(", 1:n, ")]", sep = ""))
    my_dnorm <- function(x) {
      return(stats::dnorm(x, mean = mu, sd = sigma))
    }
    if (which_step == 1) {
      graphics::plot(my_dnorm, from = range_x[1], to = range_x[2],
                     axes = FALSE, ann = FALSE, col = 0, xlim = range_x)
      u <- graphics::par("usr")
      y_vals <- rep(u[3], length(data))
      graphics::points(data, y_vals, ylim = range(data), ann = FALSE,
                       pch = 16, xpd = TRUE)
      graphics::axis(1, at = x_locs, labels = c("", xlabs, ""), pos = 0)
      graphics::title(main = "ordered data")
    } else if (which_step == 2) {
      graphics::plot(my_dnorm, from = range_x[1], to = range_x[2],
                     axes = FALSE, ann = FALSE, col = 1, xlim = range_x)
      u <- graphics::par("usr")
      y_vals <- rep(u[3], length(data))
      graphics::points(data, y_vals, ylim = range(data), ann = FALSE,
                       pch = 16, xpd = TRUE)
      graphics::axis(1, at = x_locs, labels = c("", xlabs, ""), pos = 0)
      graphics::title(main = "normal p.d.f.")
    } else if (which_step == 3) {
      graphics::plot(my_dnorm, from = range_x[1], to = range_x[2],
                     axes = FALSE, ann = FALSE, col = 1, xlim = range_x)
      u <- graphics::par("usr")
      y_vals <- rep(u[3], length(data))
      graphics::segments(normal_quantiles, rep(0, n), normal_quantiles,
                         stats::dnorm(normal_quantiles, mean = mu,
                                      sd = sigma), lty = 2)
      graphics::axis(1, at = normal_quantiles, labels = nq_labels,
                     line = -7, lty = 0, cex.axis = 1)
      graphics::axis(1, at = normal_quantiles,
                     labels = round(normal_quantiles, 2), pos = 0)
      graphics::axis(1, at = c(range_x[1], range_x[2]), labels = c("", ""),
                     pos = 0)
      graphics::title(main = "normal quantiles")
    } else if (which_step == 4) {
      graphics::plot(my_dnorm, from = range_x[1], to = range_x[2],
                     axes = FALSE, ann = FALSE, col = 1, xlim = range_x)
      y_vals <- rep(0, length(data))
      graphics::points(data, y_vals, ylim = range(data), ann = FALSE,
                       pch = 16, xpd = TRUE)
      graphics::segments(normal_quantiles, rep(0, n), normal_quantiles,
                         stats::dnorm(normal_quantiles, mean = mu,
                                      sd = sigma), lty = 2)
      graphics::axis(1, at = normal_quantiles, labels = nq_labels,
                     line = -7, lty = 0, cex.axis = 1)
      graphics::axis(1, at = normal_quantiles,
                     labels = round(normal_quantiles, 2), pos = 0)
      graphics::axis(1, at = x_locs, labels = c("", xlabs, ""), line = -2.5,
                     col = 0)
      graphics::axis(1, at = c(range_x[1], range_x[2]), labels = c("", ""),
                     pos = 0)
      graphics::title(main = "ordered data and normal quantiles")
    } else if (which_step == 5) {
      graphics::plot(normal_quantiles, data, axes = FALSE, ann = FALSE,
                     pch = 16)
      graphics::axis(2, at = x_locs, labels = c("", xlabs, ""))
      graphics::axis(1, at = normal_quantiles,
                     labels = round(normal_quantiles, 2))
      graphics::axis(1, at = c(range_x[1], range_x[2]), labels = c("", ""))
      graphics::title(main = "plot the ordered data against the normal quantiles")
    } else if (which_step == 6) {
      graphics::plot(normal_quantiles, data, axes = FALSE, ann = FALSE,
                     pch = 16)
      title(xlab = "normal quantiles", ylab = "ordered data")
      graphics::axis(2, at = x_locs, labels = c("", xlabs, ""))
      graphics::axis(1, at = normal_quantiles,
                     labels = round(normal_quantiles, 2))
      graphics::axis(1, at = c(range_x[1], range_x[2]), labels = c("", ""))
      graphics::abline(a = 0, b = 1, lty = 3, lwd = 2)
      graphics::title(main = "line of equality")
    } else if (which_step == 7) {
      graphics::plot(normal_quantiles, data, axes = FALSE, ann = FALSE,
                     pch = 16)
      title(xlab = "normal quantiles", ylab = "ordered data")
      graphics::axis(2, at = x_locs, labels = c("", xlabs, ""))
      graphics::axis(1, at = normal_quantiles,
                     labels = round(normal_quantiles, 2))
      graphics::axis(1, at = c(range_x[1], range_x[2]), labels = c("", ""))
      myqqline(data, mu = mu, sigma = sigma, type = 6, lty = 2, lwd = 2)
      graphics::title(main = "line drawn through the quartiles")
      yy <- stats::quantile(data, c(0.25, 0.75), type = 6)
      ql <- yy[1]
      qu <- yy[2]
      xx <- stats::qnorm(c(0.25, 0.75), mean = mu, sd = sigma)
      Ql <- xx[1]
      Qu <- xx[2]
      u <- graphics::par("usr")
      graphics::segments(Ql, u[3], Ql, ql, lty = 2)
      graphics::segments(u[1], ql, Ql, ql, lty = 2)
      graphics::segments(Qu, u[3], Qu, qu, lty = 2)
      graphics::segments(u[1], qu, Qu, qu, lty = 2)
      graphics::text(Ql, u[3], expression(Q[L]), xpd = TRUE, pos = 3)
      graphics::text(u[1], ql, expression(q[L]), xpd = TRUE, pos = 4)
      graphics::text(Qu, u[3], expression(Q[U]), xpd = TRUE, pos = 3)
      graphics::text(u[1], qu, expression(q[U]), xpd = TRUE, pos = 4)
    } else if (which_step == 8) {
      graphics::plot(normal_quantiles, data, axes = FALSE, ann = FALSE,
                     pch = 16)
      title(xlab = "normal quantiles", ylab = "ordered data")
      graphics::axis(2, at = x_locs, labels = c("", xlabs, ""))
      graphics::axis(1, at = normal_quantiles,
                     labels = round(normal_quantiles, 2))
      graphics::axis(1, at = c(range_x[1], range_x[2]), labels = c("", ""))
      graphics::abline(a = 0, b = 1, lty = 3, lwd = 2)
      myqqline(data, mu = mu, sigma = sigma, type = 6, lty = 2, lwd = 2)
      graphics::title(main = "both types of line")
      graphics::legend("topleft", legend = c("line of equality",
                                             "line through the quartiles"),
                       lty = c(3, 2), lwd = 2)
    }
    graphics::par(old_par)
  })
  return(invisible(panel))
}


myqqline <- function (y, datax = FALSE, mu = 0, sigma = 1, type = 6, ...) {
  y <- stats::quantile(y[!is.na(y)], c(0.25, 0.75), type = type)
  x <- stats::qnorm(c(0.25, 0.75), mean = mu, sd = sigma)
  if (datax) {
    slope <- diff(x) / diff(y)
    int <- x[1] - slope * y[1]
  }
  else {
    slope <- diff(y) / diff(x)
    int <- y[1] - slope * x[1]
  }
  return(graphics::abline(int, slope, ...))
}
