# =========================== five_number ===========================

#' Five number summary
#'
#' Calculates the five number summary of a vector of data or of each column
#' of a matrix of data, using the estimators of the lower quartile,
#' median and upper quartile in the STAT002 notes.
#'
#' @param x A numeric vector or matrix.
#' @param type Argument \code{type} used in the call to
#'   \code{\link{quantile}} to estimate the 25\%, 50\% and 75\% quantiles.
#' @param na.rm A logical scalar.  If true, any \code{\link{NA}} and NaNs
#'   are removed from \code{x} before the sample quantiles are computed.
#' @return A numeric vector (if the input was a vector) or matrix (if the input
#'   was a matrix).
#' @details The five number summary contains the sample minimum and maximum and
#'   estimates of the lower quartile, median and upper quartile, i.e. the
#'   25\%, 50\% and 75\% quantiles.  These quantiles are estimated using the
#'   \code{\link[stats]{quantile}} function.  By default, \code{type = 6} is
#'   used in the call to \code{quantile} in order to use the estimator defined
#'   in the STAT002 notes.
#' @examples
#' birth_times <- ox_births[, "time"]
#' five_number(birth_times)
#'
#' # Note: summary() uses type = 7 in the call to quantile()
#' five_number(birth_times, type = 7)
#' summary(birth_times)
#' @seealso \code{\link[stats]{quantile}} for calculating sample quantiles.
#' @export
five_number <- function(x, type = 6, na.rm = FALSE) {
  five_number_vec <- function(x, type, na.rm) {
    if (any(is.na(x)) & !na.rm) {
      return(rep(NA, 5))
    }
    max_val <- max(x, na.rm = na.rm)
    min_val <- min(x, na.rm = na.rm)
    quartiles <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = na.rm,
                                 type = type)
    return(c(min_val, quartiles, max_val))
  }
  x <- apply(cbind(x), 2, FUN = five_number_vec, type = type, na.rm = na.rm)
  rownames(x) <- c("min", "25%", "50%", "75%", "max")
  if (ncol(x) == 1) {
    x <- x[, 1]
  }
  return(x)
}

# ========================== sample skewness functions =========================

#' Sample skewness functions
#'
#' Calculates sample measures of skewness (the sample quartile skewness or
#' standardized sample skewness) of a vector of data, or of each
#' column of a matrix of data, based on the estimators described in the
#' the STAT002 notes.
#'
#' @param x A numeric vector or matrix.
#' @param type Relevant to \code{q_skew} only.  Argument \code{type} used in
#'   the call to \code{quantile} to estimate the 25\%, 50\% and 75\% quantiles.
#' @param na.rm A logical scalar.  If true, any \code{\link{NA}} and NaNs
#'   are removed from \code{x} before the constituent parts of the sample
#'   skewness are computed.
#' @details See Section 2.3 of the
#' \href{https://moodle.ucl.ac.uk/course/view.php?id=8579&section=3}{STAT002 notes.}
#'
#' \emph{Sample quartile skewness}.
#' Let \eqn{q_L}, \eqn{m} and \eqn{q_U} be the sample lower quartile,
#'   mean and upper quartile respectively.  A measure of skewness often called
#'   the \emph{quartile skewness} is given by
#'   \deqn{[ (q_U - m) - (m - qL) ] / (q_U - q_L).}
#'
#' \emph{Standardized sample skewness}.
#' Denote a vector of data by \eqn{(x_1, ..., x_n)} and let \eqn{xbar} and
#' \eqn{s} be the sample mean and sample standard deviation respectively.
#' The standardized sample skewness is given by
#' \deqn{(1 / n) \sum (x_i - xbar) ^ 3 / s ^ 3,}
#' where the summation \eqn{\sum} is over \eqn{i = 1, ..., n}.
#'
#' @return A numeric scalar (if the input was a vector) or vector (if the input
#'   was a matrix).
#' @seealso \code{\link[stats]{quantile}} for calculating sample quantiles.
#' @seealso \code{\link{mean}} for the sample mean.
#' @seealso \code{\link[stats]{sd}} for the sample standard deviation.
#' @examples
#' birth_times <- ox_births[, "time"]
#' skew(birth_times)
#' q_skew(birth_times)
#' @name skewness
NULL
## NULL

# =========================== quartile_skewness ===========================

#' @rdname skewness
#' @export
q_skew <- function(x, type = 6, na.rm = FALSE) {
  q_skew_vec <- function(x, type, na.rm) {
    qq <- stats::quantile(x, type = type, probs=c(0.25, 0.5, 0.75),
                          na.rm = na.rm)
    return(((qq[3] - qq[2]) - (qq[2] - qq[1]))/(qq[3] - qq[1]))
  }
  x <- apply(cbind(x), 2, q_skew_vec, type = type, na.rm = na.rm)
  names(x) <- NULL
  return(x)
}

# ======================== standardized sample skewness =======================

#' @rdname skewness
#' @export
skew <- function(x, na.rm = FALSE) {
  skew_vec <- function(x, na.rm) {
    s <- stats::sd(x, na.rm = na.rm)
    m <- mean(x, na.rm = na.rm)
    m3 <- mean((x - m) ^ 3, na.rm = na.rm)
    return(m3 / s ^ 3)
  }
  x <- apply(cbind(x), 2, skew_vec, na.rm = na.rm)
  names(x) <- NULL
  return(x)
}
