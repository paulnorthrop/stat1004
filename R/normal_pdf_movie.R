# ============================== normal_pdf_movie =============================

#' Normal p.d.f. movie
#'
#' A movie to show how the probability density function (p.d.f.) of the normal
#' distribution varies as the mean and/or variance of the distribution
#' are changed.
#'
#' @param starting_mean A numeric scalar.  The value of the mean used
#'   to produce the first plot of the p.d.f.
#' @param starting_var A positive numeric scalar.  The value of the variance
#'   used to produce the first plot of the p.d.f.
#' @param delta_mean A numeric scalar.  The amount by which the value of the
#'   mean is increased/decreased after one click of the +/- button for the
#'   mean.
#' @param delta_var A numeric scalar.  The amount by which the value of the
#'   variance is increased/decreased after one click of the +/- button for the
#'   variance.
#' @details The values of the mean and/or variance can be changed using the
#'   +/- buttons in the panel.
#'   For the purposes of this movie, the variance cannot be reduced below a
#'   value of 0.1.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @seealso \code{\link{normal_areas_movie}}: normal central probability areas
#'   movie.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' # Produce movie using values from the Aussie births data
#' \dontrun{
#' normal_pdf_movie()
#' normal_pdf_movie(delta_var = 1)
#' }
#' @export
normal_pdf_movie <- function(starting_mean = 0, starting_var = 1,
                             delta_mean = 0.1, delta_var = 0.1) {
  if (starting_var <= 0) {
    stop("starting_var must be positive")
  }
  normal.panel <- rpanel::rp.control("normal parameters", mu = starting_mean,
                                     sigma2 = starting_var)
  plot_pdf_normal(list(mu = starting_mean, sigma2 = starting_var))
  mu <- starting_mean
  sigma2 <- starting_var
  # Create buttons for movie
  rpanel::rp.doublebutton(normal.panel, mu, delta_mean, range=c(-1e6, 1e6),
                  repeatinterval = 20, title = "mean:",
                  action = plot_pdf_normal)
  rpanel::rp.doublebutton(normal.panel, sigma2, delta_var, range=c(0.1, 1e6),
                  repeatinterval = 20, title = "variance:",
                  action = plot_pdf_normal)
  return(invisible())
}

# Function to be called by normal_pdf_movie().

plot_pdf_normal <- function(panel) {
  with(panel, {
    sigma <- sqrt(sigma2)
    ytop <- stats::dnorm(0, mean = 0, sd =sqrt(0.1))
    from <- min(stats::qnorm(0.001, mean = mu, sd = sigma), -10)
    to <- max(stats::qnorm(0.999, mean = mu, sd = sigma), 10)
    graphics::curve(stats::dnorm(x,mean = mu, sd = sigma), from = from, to = to,
                    n = 500, bty = "l", axes = FALSE, ylab = "f(z)",
                    ylim = c(0, ytop), xlab = "z", las = 1, xpd = TRUE,
                    lwd = 2)
    graphics::axis(1, pos = 0)
    graphics::axis(2, pos = from, las = 1)
    mutext <- as.character(round(mu, 3))
    sigtext <- as.character(round(sigma2, 3))
    graphics::title(paste("normal distribution:  mean =", mutext, ", variance =", sigtext))
  })
  panel
}
