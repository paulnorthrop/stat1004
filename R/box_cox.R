# =========================== boxcox_plot ===========================

#' Plot Box-Cox transformed density functions
#'
#' Box-Cox transforms the input data \code{x} and plots a histogram of the
#' transformed data.  The Box-Cox transformation parameter is \code{lambda}.
#' If the probability density function (pdf) from which the data have arisen is
#' known and the function \code{density_fn} is supplied to calculate it then
#' this pdf is also Box-Cox transformed and the resulting transformed pdf
#' is superimposed on the histogram.
#'
#' @param x A numeric vector of data.
#' @param lambda A numeric scalar.  Box-Cox transformation parameter
#'   \eqn{\lambda}.
#' @param density_fn A function to calculate the pdf underlying the input data.
#' @param breaks The argument \code{breaks} of \code{\link{hist}}.
#'   Provided to give control of the appearance of histograms.
#' @param main The argument \code{main} of \code{\link{hist}}.
#'   Provided to enable a title to the added to the plot.
#' @param ... Further arguments to \code{density_fn} (if any).
#' @details The equation
#' \deqn{y = (x ^ \lambda - 1) / \lambda.}
#'   is a Box-Cox transformation of a variable \eqn{x} to produce a
#'   transformed variable variable \eqn{y}.  The value of the parameter
#'   \eqn{\lambda} governs the behaviour of the transformation.
#'
#' See the vignette
#' \href{../doc/stat1004-ch2d-box-cox-transformation-vignette.html}{Chapter 2: Graphs (More Than One Variable)}
#' for more details of what the code in \strong{Examples} below is doing.
#'
#' @return Nothing, just the plot.
#' @examples
#' # Log-normal distribution --------------------
#'
#' # X has a log-normal distribution if ln(X) has a normal distribution
#'
#' # Simulate a sample of size 100 from a log-normal distribution
#' x <- rlnorm(100)
#'
#' # Plot the data and the log-normal density function
#' boxcox_plot(x, density_fn = dlnorm, main = "data and true density function")
#' # If we want to transform to approximate normality which power should we use?
#' boxcox_plot(x, density_fn = dlnorm, lambda = 0, main = "after transformation")
#'
#' # We can use the data to suggest a good value of lambda.
#' # We need the boxcox() function in the MASS package.
#' library(MASS)
#'
#' # Very loosely speaking ...
#' # In this plot better values of lambda have the largest values.
#' # "Better" means "transformed data closer to looking like a sample
#' # from a normal distribution.
#' # We could choose a nice value of lambda close to the best value.
#' # The interval is a 95% confidence interval for lambda.
#' boxcox(x ~ 1)
#'
#' # exponential distribution --------------------
#'
#' x2 <- rexp(100)
#' boxcox_plot(x2 ,density_fn = dexp, main = "data and true density function")
#'
#' boxcox_plot(x2, density_fn = dexp, lambda = 1 / 3, main = "after transformation")
#' boxcox(x2 ~ 1)
#' abline(v = 1/3, col = "red")
#'
#' # A distribution that I made up --------------------
#'
#' dpn <- function(x) ifelse(x > 0 & x < 1, 2 * x, 0)
#' rpn <- function(n = 1) sqrt(runif(n))
#' x3 <- rpn(100)
#' boxcox_plot(x3, density_fn = dpn)
#' boxcox_plot(x3, density_fn = dpn, lambda = 2)
#'
#' boxcox(x3 ~ 1)
#' boxcox(x3 ~ 1, lambda = seq(0, 4, 1 / 10))
#' boxcox_plot(x3, density_fn = dpn, lambda = 1.5)
#' @export
boxcox_plot <- function(x, lambda = 1, density_fn = NULL, breaks = "Sturges",
                        main = "", ...) {
  # If lambda is different from 1 then Box-Cox transform x
  if (lambda == 1) {
    y <- x
  } else {
    y <- box_cox(x, lambda = lambda)
  }
  bc_density_fn <- function(x, lambda = 1, ...) {
    log_jacobian <- (1 - lambda) * log(x)
    density_fn(x, ...) * exp(log_jacobian)
  }
  # The probability density function is not supplied just plot the histogram.
  # Otherwise, superimpose the probability density function.
  # If lambda is different from 1 then Box-Cox transform x and the
  # probabilty density function.
  my_xlab <- ifelse(lambda == 1, "x", "y")
  if (is.null(density_fn)) {
    graphics::hist(y, prob = TRUE, col = 8, xlab = my_xlab, breaks = breaks,
                   main = main)
  } else {
    if (lambda == 1) {
      temp <- graphics::hist(y, prob = TRUE, col = 8, xlab = my_xlab,
                             breaks = breaks, main = main, plot = FALSE,
                             warn.unused = FALSE)
      y_vec <- seq(from = 0, to = max(temp$breaks), len = 1001)
      z <- density_fn(y_vec, ...)
      graphics::hist(y, prob = TRUE, ylim = c(0, max(z)), col = 8,
                     xlab = my_xlab, main = main, breaks = breaks)
      graphics::lines(y_vec, z)
    } else {
      x_plot <- seq(from = min(x), to = max(x), len = 1001)
      temp <- graphics::hist(y, prob = TRUE, col = 8, xlab = my_xlab,
                             breaks = breaks, main = main, plot = FALSE,
                             warn.unused = FALSE)
      z <- bc_density_fn(x = x_plot, lambda = lambda, ...)
      graphics::hist(y, prob = TRUE, ylim = c(0, max(z)), col = 8,
                     xlab = my_xlab, main = main, breaks = breaks)
      y_plot <- box_cox(x_plot, lambda = lambda)
      graphics::lines(y_plot, z)
    }
  }
  return(invisible())
}

# =========================== box_cox ===========================

box_cox <- function (x, lambda = 1, lambda_tol = 1e-6) {
  #
  # Computes the Box-Cox transformation of a vector.
  #
  # Args:
  #   x          : A numeric vector. (Positive) values to be Box-Cox
  #                transformed.
  #   lambda     : A numeric vector.  Transformation parameter.
  #   lambda_tol : A numeric scalar.  For abs(lambda) < lambda_tol use
  #                a first order Taylor series expansion about lambda = 0.
  #
  # Returns:
  #   A numeric vector.  The transformed value
  #     (x^lambda - 1) / (lambda * gm ^ (lambda - 1))
  #
  if (abs(lambda) > lambda_tol) {
    retval <- (x ^ lambda - 1) / lambda
  } else {
    retval <- log(x) * (1 + lambda / 2)
  }
  return(retval)
}

