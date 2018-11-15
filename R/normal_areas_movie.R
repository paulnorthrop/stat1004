# ============================== normal_areas_movie =============================

#' Normal areas movie
#'
#' A movie to show how the probability that a standard normal random variable
#' lies within plus or minus a multiple of its standard deviation (that is, 1)
#' varies with the value of the multiple.
#'
#' @param starting_multiple A non-negative numeric scalar.  The value of the
#'   multiple used in the first plot.
#' @param delta_multiple A numeric scalar.  The amount by which the value of
#'   the multiple is increased/decreased after one click of the +/- button.
#' @param ndec The number of decimal places to which to round the probability
#'   that is superimposed on the plot.
#' @details The value of the multiple can be changed using the
#'   +/i buttons in the panel.
#'   For the purposes of this movie, the multiple cannot be increased above
#'   a value of 5.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @seealso \code{\link{normal_pdf_movie}}: normal probability density function
#'   movie.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' # Produce movie using values from the Aussie births data
#' \dontrun{
#' normal_areas_movie()
#' }
#' @export
normal_areas_movie <- function(starting_multiple = 1, delta_multiple = 1,
                               ndec = 3) {
  if (starting_multiple < 0) {
    stop("starting_multiple must be positive")
  }
  plot_areas_normal(list(multiple = starting_multiple, ndec = ndec))
  multiple <- starting_multiple
  normal.panel <- rpanel::rp.control("normal(0,1) probabilities",
                             multiple = starting_multiple, ndec = ndec)
  rpanel::rp.doublebutton(normal.panel, multiple, delta_multiple,
                  range = c(0, 5), repeatinterval = 20,
                  title = "+/- multiple of SD.", action = plot_areas_normal)
  rpanel::rp.do(normal.panel, plot_areas_normal)
  return(invisible())
}

# Function to be called by normal_areas_movie().

plot_areas_normal <- function(panel) {
  with(panel, {
    n <- 100
    ytop <- dnorm(0, mean = 0, sd = 1)
    graphics::curve(dnorm(x, mean = 0, sd = 1), from = -5, to = 5, n = 100,
                    bty = "l", axes = FALSE, ylab = "", ylim = c(-0.025, ytop),
                    xlab = "", las = 1, xpd = TRUE, lwd = 2)
    graphics::axis(1, pos = 0)
    graphics::axis(2, pos = -10, las = 1)
    graphics::abline(h = 0)
    xx <- seq(from = -multiple, to = multiple, length = n)
    yy <- c(0,stats::dnorm(xx, mean = 0, sd = 1), 0)
    xx <- c(xx[1], xx, xx[length(xx)])
    graphics::polygon(xx, yy, col = gray(0.8))
    div <- 16
    div2 <- 7
    if (multiple > 0) {
      graphics::arrows(0, -ytop/div2, multiple, -ytop / div2, code = 2,
                       xpd = TRUE, angle=15, length = 0.2)
      graphics::arrows(0, -ytop/div2, -multiple, -ytop / div2, code = 2,
                       xpd = TRUE, angle=15, length = 0.2)
    }
    graphics::text(0,-ytop / 5, paste("mean +/- ",multiple," SD"), xpd = TRUE)
    graphics::text(0, ytop / 3, round(2 * stats::pnorm(multiple) - 1, ndec))
  })
  return(panel)
}
