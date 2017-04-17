#' stat1004: Introduction to Probability and Statistics at UCL
#'
#' Provides example datasets, R code and tutorials to help students taking
#' STAT1004 Introduction to Probability and Statistics at University College
#' London to understand the course material and to see how R can be used to
#' perform some of the analyses in the course.  This package can also be used
#' to complement STAT1006 Introduction to Practical Statistics, enabling
#' students to become more familiar with R code by seeing it in action and
#' playing with it themselves.
#'
#' @section Please note:
#' \strong{The main sources of information about the material in STAT1004
#' are the lectures and tutorials, with notes, lecture slides, exercises
#' etc available from the
#' \href{https://moodle.ucl.ac.uk/course/view.php?id=3419&section=0}{STAT1004 Moodle page}.
#' The focus of this R package is mainly on R code.}
#'
#' @section How to learn about R:
#' The best way to learn how to use a computer program is to play with code and
#' see what it does.  R is based on \emph{functions} that look like \code{name()}, where
#' \code{name} is the name of the function and where various \emph{arguments} inserted
#' inside the brackets \code{()}.  In the R code provided with this package you will
#' see many functions like this.  To find out what a function called \code{name} does
#' use either \code{?name} or \code{help(name)}.
#'
#' If you have any questions about this package please ask them via the
#' \href{https://moodle.ucl.ac.uk/mod/hsuforum/view.php?id=1410864}{STAT1004 Moodle Discussion Forum}.
#'
#' @section Vignettes (tutorials demonstrating how to use R code):
#'
#' \itemize{
#'   \item {\href{../doc/stat1004-shuttle-vignette.html}{Chapter 1: Challenger Space Shuttle Disaster}}
#'   \item {\href{../doc/stat1004-stochastic-simulation-vignette.html}{Chapter 1: Stochastic Simulation}}
#' }
#'
#' @section Datasets:
#'
#' \itemize{
#'   \item {\link[=shuttle]{Challenger Space Shuttle Disaster}}
#' }
#'
#'
#' @seealso The \href{https://moodle.ucl.ac.uk/course/view.php?id=3419&section=0}{STAT1004 Moodle page}
#'   for: various forms of feedback, a discussion forum, lecturecast recordings,
#'   course notes, exercises, quizzes, short videos, past exam papers and revision
#'   lecture material, real investigations.
#' @seealso The \href{https://moodle.ucl.ac.uk/course/view.php?id=7596}{STAT1006 Moodle page}
#'   (for students taking STAT1006).
#' @seealso The \href{https://moodle.ucl.ac.uk/course/view.php?id=37877}{Introduction to R Moodle page}
#'   (for students not taking STAT1006, e.g. Natural Sciences students).
#' @docType package
#' @name stat1004
#' @import methods
#' @importFrom stats runif
NULL

#' Challenger space shuttle disaster dataset
#'
#' These data are discussed in the first STAT1004 lecture.
#' A paper (Dalal \emph{et al.}, 1989), in which these data are analysed
#' with a view to estimating (retrospecitvely) the probability of a
#' catastrophic failure of the Challenger space shuttle under the
#' launch conditions on 28th January 1986 is used as a
#' \href{https://moodle.ucl.ac.uk/mod/resource/view.php?id=2125123}{worked example}
#' for the
#' \href{https://moodle.ucl.ac.uk/course/view.php?id=3419&section=4}{STAT1004 Meet your Professor ICA}.
#'
#' @format A dataframe with 24 rows and 5 columns:
#' \itemize{
#' \item{\code{flight}:}{ the flight number}
#' \item{\code{date}:}{ the date}
#' \item{\code{damaged}:}{ the number of O-rings with thermal distress}
#' \item{\code{temperature}:}{ temperature, in degrees Fahrenheit}
#' \item{\code{pressure}:}{ pressure, pounds per square inch}}
#'
#' The first 23 rows contain data for test flights.  The last row
#' contains the data for the ill-fated flight on 28th January 1986,
#' where the number of damaged 0-rings is missing (NA).
#' @source Dalal, S. R, Fowlkes, E. B. and Hoadley, B. (1989)
#'   Risk Analysis of the Space Shuttle: Pre-Challenger Prediction
#'   of Failure. \emph{Journal of the American Statistical Association},
#'   \strong{84}(408), 945-957.
#'   \url{http://dx.doi.org/10.1080/01621459.1989.10478858}
"shuttle"
