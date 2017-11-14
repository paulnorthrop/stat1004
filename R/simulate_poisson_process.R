#' Simulation of a Poisson process
#'
#' Simulates event times from a (one-dimensional, homogeneous) Poisson process
#' of rate \eqn{\lambda} per hour.  The user has the options to simulate events
#' over a fixed time period of \code{hours} hours or to simulate a fixed number
#' \code{n_events} of events.
#'
#' @param lambda A positive numeric scalar.  The rate of the Poisson process.
#' @param hours A positive integer scalar.  The number of hours for which to
#'   simulate a Poisson process of rate \eqn{lambda} events per hour.
#' @param n_events A positive integer scalar.  The number of events to
#'   simulate.
#' @details If \code{n_events} is supplied then exactly \code{n_events} are
#'   simulated and \code{hours} has no effect.  If \code{n_events} is not
#'   supplied then events are simulated over the time interval
#'   (0, \code{hours}).  If no events occur in (0, \code{hours}) then
#'   the value -1 is returned.
#' @return A numeric vector containin the times at which the events occur.
simulate_poisson_process <- function(lambda = 1, hours = 24, n_events = NULL) {
  if (is.null(n_events)) {
    # Simulate the total number of events in hours hours
    total_count <- stats::rpois(1, lambda = lambda * hours)
    if (total_count == 0) {
      return(-1L)
    }
    # Simulate the times at which the events occur given the total number
    event_times <- stats::runif(total_count, 0, hours)
  } else {
    # Simulate the times between events from an exponential(lambda) distribution
    inter_event_times <- stats::rexp(n = n_event, rate = lambda)
    event_times <- cumsum(inter_event_times)
  }
  return(event_times)
}
