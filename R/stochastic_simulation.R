# =========================== rbinomial ===========================

#' Simulates from a binomial distribution
#'
#' Simulates \strong{one} value from a binomial distribution with parameters
#' \code{size} (the number of independent Bernoulli trials) and \code{prob}
#' (the probability of success on any given trial).
#'
#' @param size An integer scalar.  The number of trials.
#' @param prob A numeric scalar.  The probability of success on each trial.
#' @details Take a look at this function's code in the
#' {\href{../doc/stat1004-stochastic-simulation-vignette.html}{Stochastic Simulation}}
#' vignette and see whether you can understand how it works.
#' @return A numeric scalar: the simulated number of successes in \code{size}
#'   trials.
#' @examples
#' # Simulate one value from a binomial(6, 0.2) distribution.
#' rbinomial(size = 6, prob = 0.2)
#' @seealso \code{link{rbinom}} for the official R function for simulating
#'   from a binomial distribution.
#' @export
rbinomial <- function(size, prob) {
  # Simulate size values (pseudo-)randomly between 0 and 1.
  u <- runif(size)
  # Find out whether (TRUE) or not (FALSE) each value of u is less than prob.
  distress <- u < prob
  # Count the number of TRUEs, i.e. the number of successes.
  n_successes <- sum(distress)
  # Return the number of successes.
  return(n_successes)
}
