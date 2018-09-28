# ================================= lev_inf_movie =============================

#' Leverage and influence movie
#'
#' A movie to examine the influence of a single outlying observation on a
#' least squares regression line.
#' @param association A character scalar.  Determines the type of association
#'   between (not-outlying) observations: "positive" for positive linear
#'   association; "negative" negative linear association; "none" for no
#'   association.
#' @param n An integer scalar.  The size of the sample of (non-outlying)
#'   observations.
#' @details \code{n} pairs of observations are simulated with the property
#'   that the mean of response variable \eqn{y} is a linear function of the
#'   values of the explanatory variable \eqn{x}.  These pairs of observations
#'   are plotted using filled black circles.  An extra observation is plotted
#'   using a filled red circle.  Initially this observation is placed in the
#'   middle of the plot.
#'
#'   Superimposed on the plot are two least squares regression lines:
#'   one based on all the data (`with outlier') and one in which the
#'   `red' observation has been removed (`without outlier'.
#'   Initially these lines coincide.
#'
#'   The location of the `red' observation can be changed using the
#'   +/- buttons so that the effect of the position of this observation
#'   on the `with outlier' line can be seen.
#'
#'   We see that if the red observation is outlying, that is, it is far
#'   from the least squares line fitted to the other observations, then
#'   its \strong{influence} on the least squares regression line depends on
#'   its x-coordinate.  If its x-coordinate is much larger or smaller than
#'   the x-coordinate of the other observations (high \strong{leverage}) then
#'   the influence is higher than if it has a similar x-coordinate to the
#'   other observations (low \strong{leverage}).  An observation with high
#'   leverage does not necessarily have high influence: if its y-coordinate
#'   falls very close to the regression line fitted to the other observations
#'   then its influence will be low.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' \dontrun{
#' lev_inf_movie()
#' lev_inf_movie(association = "none")
#' }
#' @export
lev_inf_movie <- function(association = c("positive", "negative", "none"),
                          n = 25) {
  association <- match.arg(association)
  if (association == "positive") {
    set.seed(40)
    x <- sort(stats::runif(n) * 2)
    set1 <- stats::rnorm(n, mean = x, sd = 0.5)
  } else if (association == "negative") {
    set.seed(40)
    x <- sort(stats::runif(n) * 2)
    set1 <- stats::rnorm(n, mean = 2 - x, sd = 0.5)
  } else {
    set.seed(4)
    x <- sort(stats::runif(n) / 2 + 0.5)
    set1 <- stats::rnorm(n, mean = 0.75, sd = 0.175)
  }
  x <- (x - min(x)) / range(x - min(x))[2]
  set1 <- (set1 - min(set1)) / range(set1 - min(set1))[2]
  init.x <- mean(x)
  init.y <- mean(set1)
  outx <- init.x
  outy <- init.y
  # Create buttons for movie
  lev_inf_1_panel <- rpanel::rp.control("parameters", x = x, set1 = set1,
                                        outx = init.x, outy = init.y)
  rpanel::rp.doublebutton(lev_inf_1_panel, outx, 0.2, range = c(-0.8, 1.8),
                          repeatinterval = 20, initval = 1,
                          title = "x coefficient:", action = lev_inf_1_plot)
  rpanel::rp.doublebutton(lev_inf_1_panel, outy, 0.2, range = c(-0.8,1.8),
                          repeatinterval = 20, initval = 0,
                          title = "y coefficient:", action = lev_inf_1_plot)
  rpanel::rp.do(lev_inf_1_panel, lev_inf_1_plot)
  return(invisible())
}

# Function to be called by lev_inf_1_movie().

expl_plot <- function(x, y, c1, c2, ntitle, p = 0.0185, q = 0.05, nleg = NULL,
                      ...){
  graphics::plot(x, y, axes = FALSE, ann = FALSE, ...)
  graphics::abline(coef = c1, lty = 2, lwd = 2)
  graphics::abline(coef = c2, lty = 1, lwd = 2)
  graphics::axis(2, at = c(-10, 10))
  graphics::title(ntitle)
  graphics::axis(1, at = c(-10, 10))
  u <- graphics::par("usr")
  v <- u[4] - u[3]
  h <- u[2] - u[1]
  graphics::text(u[2] - p * h / 2, u[3] - q * v, "x", xpd = TRUE)
  graphics::text(u[1] - p * h, u[4], "y", xpd = TRUE)
  u <- graphics::par("usr")
  if (!is.null(nleg)) {
    graphics::legend(u[2], u[3],
              legend = c("with outlier","without outlier","outlier"),
              lty = c(1, 2, -1), xjust = 1, yjust = 0, lwd = c(2, 2),
              pch = c(-1, -1, 16), col = c(1, 1, 2))
  }
  graphics::points(x, y, ...)
  return(invisible(x))
}

lev_inf_1_plot <- function(panel){
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    graphics::par(oma = c(0, 0, 0, 0), mar = c(3, 4, 1, 2), las = 1, pch = 16,
                  bty = "l", mfrow = c(1, 1))
    lm1 <- stats::lm(set1 ~ x)
    c1 <- lm1$coeff
    pcol <- c(rep(1, length(x)), 2)
    x <- c(x, outx)
    set1 <- c(set1, outy)
    lm1b <- stats::lm(set1 ~ x)
    c2 <- lm1b$coeff
    expl_plot(x, set1, c1, c2,
              ntitle = "Effects of an outlier on LS regression line",
              nleg = 1, xlim = c(-0.75, 1.75), ylim = c(-0.75, 1.75),
              col = pcol)
    graphics::par(old_par)
  })
  return(invisible(panel))
}


