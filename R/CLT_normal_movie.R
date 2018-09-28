# ============================== clt_normal_movie =============================

#' Central Limit Theorem movie: normal data
#'
#' A movie to illustrate the ideas of a sampling distribution of a random
#' variable and the central limit theorem (CLT).  In this case (based on
#' random samples from a normal distribution) the CLT provides an exact
#' result.
#'
#' @param n An integer scalar.  The size of the samples drawn from a
#'   normal distribution.
#' @param mu,sigma Numeric scalars.  The respective mean and standard
#'   deviation of the normal distribution from which data are to be
#'   simulated using \code{\link{rnorm}}.
#' @param xlab A character scalar.  A name to use to label the horizontal
#'   axis of the plots.
#' @param pos A numeric integer.  Used in calls to \code{\link{assign}}
#'   to make information available across successive frames of a movie.
#'   By default, uses the current environment.
#' @param envir An alternative way (to \code{pos}) of specifying the
#'   environment. See \code{\link{environment}}.
#' @details Loosely speaking, a consequence of the
#'   \href{https://en.wikipedia.org/wiki/Central_limit_theorem}{Central Limit Theorem (CLT)}
#'   is that, in many situations, the mean of a \strong{large number} of
#'   independent random variables has \strong{approximately} a normal distribution,
#'   even if these original variables are not normally distributed.
#'
#'   This movie illustrates this in the very special case where the original
#'   variables \emph{are} normally distributed.  Samples of size \code{n}
#'   are repeatedly simulated from a normal distribution.  These samples are
#'   summarized using a histogram that appears at the top of the movie screen.
#'   For each sample the mean of these \code{n} values is calculated, stored
#'   and added to another histogram plotted below the first histogram.
#'   The respective probability density functions (p.d.f.s) of the original
#'   variables and the means are superimposed on these histograms.
#'   The latter is know to be exactly a normal p.d.f. in this special case.
#'
#'   The user may choose the sample size \code{n}, that is, the number of
#'   values over which a mean is calculated, the mean \code{mu} and/or
#'   standard deviation \code{sigma} of the normal distribution from which
#'   values are simulated and the label \code{xlab} for the horizontal axis.
#'
#'   Once it starts, two aspects of this movie are controlled by the user.
#'   Firstly, there are buttons to increase (+) or decrease (-) the sample
#'   size, that is, the number of values over which a mean is calculated.
#'   Then there is a button labelled "simulate another sample of size n".
#'   Each time this button is clicked a new sample is simulated and its sample
#'   mean added to the bottom histogram.
#'
#'   Another movie (\code{\link{clt_exponential_movie}}) illustrates the CLT
#'   in the case where the original variables are exponentially distributed.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @seealso \code{\link{clt_exponential_movie}}: a similar movie using data
#'   simulated from an exponential distribution.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' # Produce movie using values based on the Aussie births data
#' \dontrun{
#' clt_normal_movie(44, 7.22, sqrt(1.36), "weight (pounds)")
#' }
#' @export
clt_normal_movie <- function(n = 30, mu = 0, sigma = 1, xlab = "x", pos = 1,
                             envir = as.environment(pos)) {
  # Assign variables to an environment so that they can be accessed inside
  # clt_normal_movie_plot()
  old_n <- 0
  assign("old_n", old_n, envir = envir)
  assign("mu", mu, envir = envir)
  assign("sigma", sigma, envir = envir)
  assign("xlab", xlab, envir = envir)
  # Create buttons for movie
  clt_norm_panel <- rpanel::rp.control("sample size", n = n, mu = mu,
                                       sigma = sigma, envir = envir)
  rpanel::rp.doublebutton(clt_norm_panel, n, 1, range=c(1, 1000),
                          repeatinterval = 20, initval = n,
                          title = "sample size, n",
                          action = clt_normal_movie_plot)
  rpanel::rp.button(clt_norm_panel, repeatinterval = 20,
            title = "simulate another sample of size n",
            action = clt_normal_movie_plot)
  rpanel::rp.do(clt_norm_panel, clt_normal_movie_plot)
  return(invisible())
}

# Function to be called by clt_normal_movie().

clt_normal_movie_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 2, 2) + 0.1)
    assign("mu", mu, envir = envir)
    assign("sigma", sigma, envir = envir)
    assign("xlab", xlab, envir = envir)
    y <- stats::rnorm(n, mean = mu, sd = sigma)
    mean_y <- mean(y)
    if (n != old_n) {
      sample_means <- mean_y
    } else {
      sample_means <- c(sample_means, mean_y)
    }
    assign("sample_means", sample_means, envir = envir)
    h_low <- mu - 3 * sigma
    h_up <- mu + 3 * sigma
    ytop <- dnorm(0, sd = sigma) * 1.5
    y <- y[y > h_low & y < h_up]
    # Histogram with rug
    graphics::hist(y, col = 8, probability = TRUE, axes = FALSE,
                   xlab = xlab, ylab = "density", main = "",
                   ylim = c(0, ytop), xlim = c(h_low, h_up))
    axis(2)
    axis(1, line = 0.5)
    graphics::rug(y, line = 0.5, ticksize = 0.05)
    graphics::title(paste("sample size, n = ",n))
    graphics::curve(stats::dnorm(x, mean = mu, sd = sigma), from = h_low,
                       to = h_up, n = 500, bty = "l", ylab = "density",
                       las = 1, xpd = TRUE, lwd = 2, add =TRUE, lty = 2)
    my_mean <- round(mu, 2)
    my_var <- round(sigma ^ 2, 2)
    my_leg <- paste("N(", my_mean, ",", my_var,")" )
    graphics::legend("topright", legend = my_leg, lty = 2, lwd = 2)
    graphics::segments(mean_y, 0, mean_y, -10, col = "red", xpd = TRUE, lwd = 2)
    graphics::points(mean_y, 0, pch = 16, col = "red", cex = 1.5)
    ytop <- dnorm(0, sd = sigma / sqrt(n)) * 1.5
    y <- sample_means
    y <- y[y > h_low & y < h_up]
    my_xlab <- paste("sample mean of", xlab)
    # Histogram with rug
    graphics::hist(y, col = 8, probability = TRUE, las = 1, axes = FALSE,
         xlab = my_xlab, ylab = "density", main = "",
         ylim = c(0, ytop), xpd = TRUE, xlim = c(h_low, h_up))
    graphics::axis(2)
    graphics::axis(1, line = 0.5)
    graphics::rug(y, line = 0.5, ticksize = 0.05, col = "red")
    graphics::curve(stats::dnorm(x, mean = mu, sd = sigma / sqrt(n)),
                    from = h_low, to = h_up, n = 500, bty = "l",
                    ylab="density", las = 1, xpd = TRUE, lwd = 2,
                    add = TRUE, lty = 2)
    my_leg_2 <- paste("N(", my_mean, ",", my_var, "/ n)" )
    graphics::legend("topright", legend = my_leg_2, lty = 2, lwd = 2)
    graphics::arrows(mean_y, 2* ytop, mean_y, 0, col = "red", lwd = 2, xpd = TRUE)
    old_n <- n
    assign("old_n", old_n, envir = envir)
    graphics::par(old_par)
  })
  return(invisible(panel))
}
