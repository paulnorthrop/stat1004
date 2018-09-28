# ============================ binomial_pmf_movie =============================

#' Binomial p.m.f. movie
#'
#' A movie to illustrate how the probability mass function (p.m.f.) of a
#' binomial (n, p) random variable depends on n and p.
#'
#' @param starting_n A numeric scalar.  The value of n for the first graph.
#' @param starting_p A numeric scalar.  The value of p for the first graph.
#' @param delta_n A numeric scalar.  The amount by which n is increased
#'   (or decreased) after one click of the + (or -) button in the parameter
#'   window.
#' @param delta_p A numeric scalar.  The amount by which p is increased
#'   (or decreased) after one click of the + (or -) button in the parameter
#'   window.
#' @param observed_value A non-negative integer.  If \code{observed_value} is
#'   supplied then the corresponding line in the plot of the p.m.f. is coloured
#'   in red.  If \code{observed_value} is not an integer then
#'   \code{round(observed_value)} is used.
#' @details The probability mass function of a binomial random variable with
#'   parameters \eqn{n} (the number of Bernoulli trials performed) and
#'   \eqn{p} (the probabilities of success on a each trial) is plotted.
#'   The values of \eqn{n} and \eqn{p} can be changed by clicking on the
#'   relevant buttons.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' \dontrun{
#' binomial_pmf_movie()
#'
#' # Increase n and see what happens
#' binomial_pmf_movie(delta_n = 10)
#'
#' # Sample size of the Aussie births data (26 boys, 18 girls)
#' binomial_pmf_movie(starting_n = 44, starting_p = 0.1, delta_p = 0.1,
#'                    observed_value = 26)
#'
#' # Start at p = 0.591 (approximately 26/44)
#' binomial_pmf_movie(starting_n = 44, starting_p = 0.591, delta_p = 0.01,
#'                    observed_value = 26)
#' }
#' @export
binomial_pmf_movie <- function(starting_n = 1, starting_p = 1 /2, delta_n = 1,
                               delta_p = 0.05, observed_value = NA) {
  if (!is.na(observed_value) && observed_value < 0) {
    stop("observed_value cannot be negative")
  }
  observed_value <- round(observed_value)
  binomial_panel <- rpanel::rp.control("binomial(n,p) probabilities",
                                       n = starting_n, prob = starting_p,
                                       observed_value = observed_value)
  prob <- starting_p
  n <- starting_n
  plot_binomial_pmf(list(n = starting_n, prob = starting_p,
                         observed_value = observed_value))
  rpanel::rp.doublebutton(panel = binomial_panel, variable = prob,
                          step = delta_p, range = c(0, 1), repeatinterval = 20,
                          initval = starting_p, title = "P(success), p:",
                          action = plot_binomial_pmf)
  rpanel::rp.doublebutton(panel = binomial_panel, variable = n, step = delta_n,
                          range=c(1, 1000000), repeatinterval = 20,
                          initval = starting_n, title = "number of trials, n:",
                          action = plot_binomial_pmf)
  invisible()
}

plot_binomial_pmf <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    probs <- dbinom(0:n, n, prob)
    graphics::plot(c(0,n), c(0,1), type = "n", xlab = "y", ylab = "P(Y=y)",
                   axes = FALSE, ylim = c(0, max(probs)), xlim= c(0, n),
                   bty = "l", las = 1)
    if (n < 51){
      graphics::axis(1, at = 0:n, labels = 0:n, cex.axis = 0.7)
    }
    else{
      graphics::axis(1)
    }
    graphics::box(bty = "l")
    graphics::axis(2, las = 1)
    if (is.null(observed_value)) {
      col <- 1
    } else {
      col <- rep(1, n + 1)
      col[observed_value + 1] <- 2
    }
    graphics::segments(0:n, rep(0, n + 1), 0:n, probs, lwd = 3, col = col)
    ptext <- as.character(round(prob, 3))
    graphics::title(paste("binomial p.m.f.:  n =", n, "  p =", ptext))
    graphics::par(old_par)
  })
  return(panel)
}
