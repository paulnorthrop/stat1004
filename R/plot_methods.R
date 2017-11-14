# ============================= plot.poisson_process =============================

#' Plot diagnostics a \code{poisson_process} object
#'
#' \code{plot} method for class "poisson_process".
#'
#' @param x an object of class "poisson_process", a result of a call to
#'   \code{\link{simulate_poisson_process}}.
#' @param y Not used.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}
#' @return Nothing is returned.
#' @examples
#' sim1 <- simulate_poisson_process(lambda = 2, hours = 24)
#' plot(sim1)
#' sim2 <- simulate_poisson_process(lambda = 2, n_events = 50)
#' plot(sim2, pch = 4, xlab = "event times")
#' @seealso \code{\link{simulate_poisson_process}}.
#' @export
plot.poisson_process <- function(x, y, ...) {
  if (!inherits(x, "poisson_process")) {
    stop("use only with \"poisson_process\" objects")
  }
  event_times <- as.numeric(x)
  total_count <- length(event_times)
  events <- rep(0, total_count)
  graphics::plot(event_times, events, ylim = c(-1, 1),
                 axes = FALSE, ...)
  graphics::axis(1, at = c(0, pretty(event_times), ceiling(max(event_times))))
  return(invisible())
}
