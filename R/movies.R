# ========================== movies =========================

#' General information about the movies
#'
#' These movies are animations used to illustrate key ideas in STAT0002.
#' They are produced using the package \code{\link[rpanel]{rpanel.package}}.
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
#' (Some of these movies, and other movies are also available in a more
#' user-friendly form, via the \code{\link[smovie]{smovie}} package.
#' If you have \code{smovie} installed (\code{install.packages("smovie")})
#' then you can access these using \code{library(smovie)} and then
#' \code{movies()}.
#' See \code{\link[smovie]{movies}} for more details.)
#'
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
#' @seealso  \code{\link{poisson_process_movie}}: illustrates the link between
#'   the Poisson process and the Poisson distribution for the number of events
#'   that occur during a time interval of fixed length.
#' @seealso  \code{\link{poisson_process_check}}: uses the plots in
#'   \code{\link{poisson_process_movie}} to perform informal graphical checks
#'   of whether the arrival times of a sequence of events is consistent
#'   with arising from a Poisson process.
#' @seealso  \code{\link{normal_pdf_movie}}: shows the effect of the
#'   mean and variance parameters of a normal distribution on its p.d.f..
#' @seealso  \code{\link{normal_areas_movie}}: shows how the probability that
#' a standard normal random variable lies within plus or minus a multiple of
#' its standard deviation varies with the value of the multiple.
#' @seealso  \code{\link{qq_plot_movie}}: shows how a (normal) QQ plot is
#'   constructed, using a small example dataset.
#' @seealso  \code{\link{normal_sampling_distns_movie}}: shows how the
#'   sampling distributions of the sample mean and sample variance based
#'   on a random sample from a normal distribution depend on the size
#'   \eqn{n} of the sample.
#' @seealso  \code{\link{clt_normal_movie}}: illustrates the ideas of a
#'   sampling distribution of a random variable and the central limit
#'   theorem (CLT), using normally distributed data.
#' @seealso  \code{\link{clt_exponential_movie}}: illustrates the ideas of a
#'   sampling distribution of a random variable and the central limit
#'   theorem (CLT), using exponentially distributed data..
#' @seealso  \code{\link{mean_vs_median_normal_movie}}: compares the
#'   sampling distributions of the sample mean and sample median based
#'   on a random sample of size \eqn{n} from a standard normal distribution.
#' @seealso  \code{\link{mean_vs_median_t_movie}}: compares the
#'   sampling distributions of the sample mean and sample median based
#'   on a random sample of size \eqn{n} from a Student's t distribution.
#' @seealso  \code{\link{two_by_two_movie}}: studies the distribution
#'   of the Pearson chi-squared test statistic used to test for lack of
#'   association in a 2 by 2 contingency table.
#' @seealso  \code{\link{lin_reg_movie}}: visualizes the fitting of a
#'   regression line using least squares.
#' @seealso  \code{\link{lev_inf_movie}}: examine the influence of a
#'   single outlying observation on a least squares regression line.
#' @seealso  \code{\link{corr_sim_movie}}: illustrates the sampling
#'   distribution of the (Pearson product moment) sample correlation
#'   coefficient.
#' @section Examples:
#' See the examples given for the individual movies listed below.
#' @name movies
NULL
## NULL

