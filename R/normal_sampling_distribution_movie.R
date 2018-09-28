# ======================= normal_sampling_distns_movie ==================

#' Normal sampling distributions movie
#'
#' A movie to show how the sampling distributions of the sample mean
#' and sample variance based on a random sample from a normal distribution
#' depend on the size \eqn{n} of the sample.
#'
#' @param starting_n A numeric scalar.  The value of the sample size used
#'   to produce the first plot of the p.d.f.
#' @param delta_n A numeric scalar.  The amount by which the value of the
#'   sample size is increased/decreased after one click of the +/- button.
#' @param mu A numeric scalar.  The mean of the normal distribution from
#'   which the random sample are drawn.
#' @param sigma A numeric scalar.  The standard deviation of the normal
#'   distribution from which the random sample are drawn.
#' @details The movie is based on two plots.  The top plot shows the
#'   probability density function (p.d.f.) of the sampling distribution of the
#'   sample mean, that is, a N(\eqn{\mu}, \eqn{\sigma}^2 / n) distribution.
#'   The bottom plot contains the p.d.f. of the sampling distribution of the
#'   sample variance, that is, a gamma distribution
#'   (\code{\link[stats]{GammaDist}}) with shape parameter
#'   \eqn{(n - 1) / 2} and rate parameter \eqn{(n - 1) / 2\sigma^2}.
#'
#'   The number of samples simulated is increased
#'
#'   The value of the sample size can be changed using the +/-
#'   buttons in the panel.  For the purposes of this movie the value of
#'   \eqn{n} cannot exceed 100.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' \dontrun{
#' normal_sampling_distns_movie()
#' }
#' @export
normal_sampling_distns_movie <- function(starting_n = 30, delta_n = 1, mu = 0,
                                         sigma = 1) {
  if (starting_n <= 0) {
    stop("starting_n must be positive")
  }
  # Create buttons for movie
  normal_sd_panel <- rpanel::rp.control("sample size", n = starting_n, mu = mu,
                                        sigma = sigma, ntop = 100)
  n <- starting_n
  rpanel::rp.doublebutton(normal_sd_panel, n, delta_n, range = c(1, 100),
                          repeatinterval = 20, initval = starting_n,
                          title = "sample size",
                          action = plot_normal_sampling_distributions)
  rpanel::rp.do(normal_sd_panel, plot_normal_sampling_distributions)
  return(invisible())
}

# Function to be called by normal_sampling_distributions_movie().

plot_normal_sampling_distributions <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    graphics::par(mfrow=c(2, 1), oma =c(0, 0, 0, 0), mar = c(4, 4, 2, 4) + 0.1)
    # Top plot: sampling distributon of the sample mean
    mu_low <- mu - 2 * sigma
    mu_up <- mu + 2 * sigma
    ytop <- dnorm(0, mean = 0, sd = sigma / sqrt(ntop))
    graphics::curve(stats::dnorm(x, mean = mu, sd = sigma / sqrt(n)),
          from = mu_low, to = mu_up, n = 500, bty = "l", axes = FALSE,
          ylab = "density", ylim = c(0, ytop), xlab=expression(hat(mu)),
          las = 1, xpd = TRUE, lwd = 2)
    graphics::title(paste("sample size = ",n))
    graphics::axis(2)
    br <- pretty(seq(from = mu_low, to = mu_up, len = 100))
    graphics::axis(1, at = br, labels = br, pos = 0)
    graphics::abline(v = mu, lty = 2)
    # Bottom plot: sampling distributon of the sample variance
    sigma_low <- 0
    alphalow <- 1 / 2
    betalow <- (2 - 1) / (2 * sigma ^ 2)
    sigma_up <- stats::qgamma(0.95, shape = alphalow, rate = betalow)
    alphatop <- (ntop - 1) / 2
    betatop <- (ntop - 1) / (2 * sigma ^ 2)
    ytop <- stats::dgamma((alphatop - 1) / betatop, shape = alphatop,
                          rate = betatop)
    alpha <- (n - 1) / 2
    beta <- (n - 1) / (2 * sigma ^ 2)
    graphics::curve(stats::dgamma(x, shape = alpha, rate = beta),
                    from = sigma_low, to = sigma_up, n = 500, bty = "l", axes = FALSE,
                    ylab = "density", ylim = c(0, ytop),
                    xlab = expression(hat(sigma)^2), las = 1, xpd = TRUE,
                    lwd = 2)
    graphics::axis(2, pos = 0)
    br <- pretty(seq(from = sigma_low, to = sigma_up, len = 100))
    graphics::axis(1, pos = 0, at = br, labels = br)
    graphics::abline(v = sigma ^ 2, lty = 2)
    graphics::par(old_par)
  })
  return(invisible(panel))
}
