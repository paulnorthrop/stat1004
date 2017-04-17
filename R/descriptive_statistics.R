# =========================== five_number ===========================

#' Five number summary
#'
#' Calculates the five number summary of a vector of data or of each column
#' of a matrix of data, using the estimators of the lower quartile,
#' median and upper quartile in the STAT1004 notes.
#'
#' @param x A numeric vector or matrix.
#' @param type Argument \code{type} used in the call to \code{quantile} to
#'   estimate the 25\%, 50\% and 75\% quantiles.
#' @param na.rm A logical scalar.  If true, any \code{\link{NA}} and NaN's
#'   are removed from \code{x} before the quantiles are computed.
#' @return A numeric vector (if the input was a vector) or matrix (if the input
#'   was a matrix).
#' @details The five number summary contains the sample minimum and maximum and
#'   estimates of the lower quartile, median and upper quartile, i.e. the
#'   25\%, 50\% and 75\% quantiles.  These quantiles are estimated using the
#'   \code{\link{quantile}} function.  By default, \code{type = 6} is used in
#'   the call to \code{quantile} in order to use the estimator defined in the
#'   STAT1004 notes.
#' @examples
#' birth_times <- ox_births[, "time"]
#' summary(birth_times)
#' @seealso \code{\link{quantile}} for calculating sample quantiles.
five_number <- function(x, type = 6, na.rm = FALSE) {
  five_number_vec <- function(x, type, na.rm) {
    c(min(x, na.rm = na.rm), quantile(x, probs = c(0.25, 0.5, 0.75),
                                      type = type, na.rm = na.rm),
      max(x, na.rm = na.rm))
  }
  x <- apply(cbind(x), 2, five_number_vec, type = type, na.rm = na.rm)
  rownames(x)[c(1, 5)] <- c("min", "max")
  if (ncol(x) == 1) {
    x <- x[, 1]
  }
  return(x)
}

# =========================== quartile_skewness ===========================

#' Quartile skewness
#'
#' Calculates the sample quartile skewness of a vector of data or of each column
#' of a matrix of data, based on the estimators of the lower quartile,
#' median and upper quartile in the STAT1004 notes.
#'
#' @param x A numeric vector or matrix.
#' @param type Argument \code{type} used in the call to \code{quantile} to
#'   estimate the 25\%, 50\% and 75\% quantiles.
#' @param na.rm A logical scalar.  If true, any \code{\link{NA}} and NaN's
#'   are removed from \code{x} before the quantiles are computed.
#' @return A numeric scalar (if the input was a vector) or vector (if the input
#'   was a matrix).
#' @details Let \eqn{q_L}, \eqn{m} and \eqn{q_U} be the sample lower quartile,
#'   mean and upper quartile respectively.  A measure of skewness often called
#'   the \emph{quartile skewness} is given by
#'   \deqn{[ (q_U - m) - (m - qL) ] / (q_U - q_L).}
#' @examples
#' birth_times <- ox_births[, "time"]
#' q_skew(birth_times)
#' @seealso \code{\link{quantile}} for calculating sample quantiles.
q_skew <- function(x, type = 6, na.rm = FALSE) {
  q_skew_vec <- function(x, type, na.rm) {
    qq <- quantile(x, type = type, probs=c(0.25, 0.5, 0.75), na.rm = na.rm)
    return(((qq[3] - qq[2]) - (qq[2] - qq[1]))/(qq[3] - qq[1]))
  }
  x <- apply(cbind(x), 2, q_skew_vec, type = type, na.rm = na.rm)
  names(x) <- NULL
  return(x)
}

# ======================== standardized sample skewness =======================

#' Standardized sample skewness
#'
#' Calculates the standardized sample skewness of a vector of data or of each
#' column of a matrix of data, based on the estimators of the lower quartile,
#' median and upper quartile in the STAT1004 notes.
#'
#' @param x A numeric vector or matrix.
#' @param na.rm A logical scalar.  If true, any \code{\link{NA}} and NaN's
#'   are removed from \code{x} before the quantiles are computed.
#' @return A numeric scalar (if the input was a vector) or vector (if the input
#'   was a matrix).
#' @details See Section 2.3 of the
#' \href{https://moodle.ucl.ac.uk/pluginfile.php/3054127/mod_resource/content/22/STAT1004notes-2x1.pdf}{STAT1004 notes.}
#' @examples
#' birth_times <- ox_births[, "time"]
#' skew(birth_times)
#' @seealso \code{\link{quantile}} for calculating sample quantiles.
skew <- function(x, type = 6, na.rm = FALSE) {
  skew_vec <- function(x, type, na.rm) {
    s <- sd(x, na.rm = na.rm)
    m <- mean(x, na.rm = na.rm)
    m3 <- mean((x - m) ^ 3, na.rm = na.rm)
    return(m3 / s ^ 3)
  }
  x <- apply(cbind(x), 2, skew_vec, type = type, na.rm = na.rm)
  names(x) <- NULL
  return(x)
}
