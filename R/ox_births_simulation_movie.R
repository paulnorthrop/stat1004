# =========================== ox_births_movie ===========================

#' Oxford Birth Times Simulation Movie
#'
#' A movie to illustrate that as we increase the size of a sample simulated
#' from a continuous probability distribution a histogram of these data
#' gives a closer approximation to the underlying probability density
#' function (p.d.f.).
#'
#' @param starting_n A numeric scalar.  The size of the first sample to be
#'   simulated.
#' @param delta_n A numeric scalar.  The amount by which the sample size
#'   is increased (or decreased) after one click of the + (or -) button
#'   in the parameter window.
#' @details \code{ox_births_movie} first fits a
#'   \href{https://en.wikipedia.org/wiki/Gamma_distribution}{gamma distribution}
#'   (see \code{\link[stats]{GammaDist}}) to the \code{time} data in the
#'   \code{\link{ox_births}} data.  Then we simulate samples (using
#'   \code{\link[stats]{rgamma}}) from the fitted gamma distribution and produce a
#'   histogram of the simulated data.  The p.d.f. of the fitted gamma distribution
#'   is superimposed on the histogram.
#'
#'   We will not study the gamma distribution in STAT1004, but you will encounter
#'   it in STAT1005 if you take that next term.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about STAT1004 movies.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' \dontrun{
#' ox_births_movie()
#' }
#' @export
ox_births_movie <- function(starting_n = 100, delta_n = 1000) {
  # Fit a gamma distribution to the birth times
  x <- stat1004::ox_births[, "time"]
  fit_gamma <- stats::glm(x ~ 1, family = stats::Gamma(link = "identity"))
  disp <- MASS::gamma.dispersion(fit_gamma)
  alpha <- 1 / disp
  mean_val <- fit_gamma$coeff
  beta <- alpha / mean_val
  n <- starting_n
  gamma_sim_panel <- rpanel::rp.control("simulation of birth times",
                                        n = starting_n, alpha = alpha,
                                        beta = beta)
  plot_gamma_sim(list(n = starting_n, alpha = alpha, beta = beta))
  rpanel::rp.doublebutton(gamma_sim_panel, n, delta_n, range=c(1, 1e6),
                          repeatinterval = 20, initval = starting_n,
                          title = "sample size, n:", action = plot_gamma_sim)
  invisible()
}

plot_gamma_sim <- function(panel) {
  with(panel, {
    set.seed(37)
    gamsim <- stats::rgamma(n, shape = alpha, rate = beta)
    xx <- seq(from = min(gamsim), to = max(gamsim), len = 101)
    gam_dens <- dgamma(xx, shape = alpha, rate = beta)
    # Pick the number of bins (I got here largely by trial-and-error)
    # to make the movie look `nice'
    binwidth <- min(1000 / n ^ (2 / 3), 2)
    n_bins <- max(gamsim) / binwidth
    temp <- graphics::hist(gamsim, plot = FALSE, breaks = n_bins)
    ylim <- range(c(gam_dens, temp$density))
    graphics::hist(gamsim, prob = TRUE, main = "", ylim = c(0, ylim[2]),
                   breaks = n_bins, col = 8)
    graphics::curve(dgamma(x, shape = alpha, rate = beta), from = 0,
                    to = max(gamsim), add = TRUE)
    graphics::title(paste("simulated data, n =", n))
  })
  return(panel)
}
