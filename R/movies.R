# ========================== movies =========================

#' General information about STAT1004 movies
#'
#' These movies are animations used to illustrate key ideas in STAT1004.
#' They are produced using the package \code{\link[rpanel]{rpanel}}.
#' You need to install \code{rpanel} once, using RStudio's Install button
#' in the Packages menu or \code{install.packages("rpanel")} on the command
#' line).
#' Prior to creating a movie the rpanel package must be loaded,
#' using `library(rpanel)`, in order that these functions work.
#' If \code{rpanel} is not loaded then an error message
#' similar to \code{Error in shuttle_movie() : could not find function
#' "rp.control"} will be produced.
#' For the one of the movies the \code{tkrplot} package is also required.
#' See \strong{Examples} below.
#'
#' @details
#' When one of these functions is called R opens up a small
#' \emph{parameter window} containing clickable buttons that can be
#' used to change parameters underlying the plot. For the effects of
#' these buttons see the documentation of the individual functions below.
#'
#' The parameter window does not close automatically after the movie:
#' the user needs to close it manually.
#'
#' Some movies create objects in the global environment, that is, objects
#' that will be listed when \code{ls()} is used.  \code{rm} can be used to remove
#' these objects if desired.  For example \code{rm(name)} can be used to remove
#' object \code{name}.
#'
#' @return Nothing is returned, only the animation is produced.
#' @references Bowman, A., Crawford, E., Alexander, G. and Bowman, R. W.
#'  (2007). rpanel: Simple Interactive Controls for R Functions Using the
#'  tcltk Package.  \emph{Journal of Statistical Software}, \strong{17(9)},
#'  1-18.
#'  \url{http://www.jstatsoft.org/v17/i09/}.
#' @seealso \code{\link{shuttle_movie}}: illustrates uncertainty in the
#'   linear logistic regression curves fitted to the real space shuttle data.
#' @seealso \code{\link{scatterplot_movie}}: straightening scatter plots
#'   using variable transformation (US 2000 Presidential Election).
#' @seealso \code{\link{world_bank_movie}}: explores graphically relationships
#'   between four annual World Bank Development Indicators and how this changes
#'   over time.
#' @seealso  \code{\link{ox_births_movie}}: shows how the shape of a histogram
#'   of simulated data tends to become smoother as the sample size increases.
#' @seealso  \code{\link{binomial_pmf_movie}}: shows how the probability mass
#'   function (p.m.f.) of a binomial random variable depends on its two
#'   parameters.
#' @seealso  \code{\link{clt_normal_movie}}: illustrates the ideas of a
#'   sampling distribution of a random variable and the central limit
#'   theorem (CLT), using normally distributed data.
#' @seealso  \code{\link{clt_exponential_movie}}: illustrates the ideas of a
#'   sampling distribution of a random variable and the central limit
#'   theorem (CLT), using exponentially distributed data..
#' @seealso  \code{\link{poisson_process_movie}}: illustrates the link between
#'   the Poisson process and the Poisson distrbution for the number of events
#'   that occur during a time interval of fixed length.
#' @seealso  \code{\link{normal_pdf_movie}}: shows the effect of the
#'   mean and variance parameters of a normal distribution on its p.d.f..
#' @seealso  \code{\link{normal_areas_movie}}: shows how the probability that
#' a standard normal random variable lies within plus or minus a multiple of
#' its standard deviation varies with the value of the multiple.
#' @seealso  \code{\link{qq_plot_movie}}: shows how a (normal) QQ plot is
#'   constructed, using a small example dataset.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' \dontrun{
#' # Load package rpanel
#' library(rpanel)
#'
#' # Challenger space shuttle data -------------
#'
#' # Movie based on datasets of size 23
#' shuttle_movie()
#' # Movie based on datasets of size 230
#' shuttle_movie(n_reps = 10)
#'
#' # Rosling-style animated scatterplot -------------
#'
#' # Load package tkrplot
#' library(tkrplot)
#' # [Use install.packages("tkrplot") if necessary]
#'
#' rp.bubbleplot(log(gdp_per_capita), log(co2_per_capita), 1960:2014,
#'               size = population_size, col = life_expectancy,
#'               interpolate = TRUE, hscale = 1.5, vscale = 1.5)
#' }
#' @name movies
NULL
## NULL

