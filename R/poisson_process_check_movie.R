# ============================ poisson_process_check ==========================

#' Graphical checking of Poisson process properties
#'
#' A movie to perform informal graphical checks of whether event arrival times
#' are consistent with arising from a (one-dimensional, homogeneous)
#' Poisson process.
#'
#' @param user_data A numeric vector of arrival times of events.
#' @param total_time A positive numeric scalar.  The number of time units
#'   (for example hours) over which the events in \code{user_data}
#'   were recorded.  Must be no smaller than the largest value in
#'   \code{user_data}.
#' @param intervals An integer scalar.  Must not be smaller than 1.
#'   The number of intervals into which to split the time interval
#'   (0, \code{total_time}) for the purposes of checking the property
#'   that the number of events that occur in an interval of fixed width
#'   has a Poisson distribution.  If \code{intervals} is not an integer
#'   then it will be rounded to the nearest integer.
#' @param unif_bins A positive integer scalar.  The number of bins to use in
#'   the histogram in the "times at which events occur" described in
#'   \strong{Details}.  If this is not supplied then the default
#'   number of bins used in \code{\link[graphics]{hist}} is used.
#' @param exp_bins A positive integer scalar.  The number of bins to use in
#'   the histogram in the "times between events" described in
#'   \strong{Details}.  If this is not supplied then the default
#'   number of bins used in \code{\link[graphics]{hist}} is used.
#' @details This movie contains two displays of plots: one plot on the top and
#'   one of three other plots on the bottom.  The rate, \eqn{\lambda} say, of
#'   the supposed Poisson process is estimated by the sample mean number of
#'   events over the \code{n_intervals}, that is,
#'   \code{length(user_data)} / \code{intervals}.  Let's call this estimate
#'   \eqn{m}.
#'
#'   The type of plot that appears in the bottom of the display depends
#'   on the radio click by the user. The choices are
#'   \itemize{
#'   \item{"none" }{Nothing is plotted}
#'   \item{"numbers of events in each hour" }{A barplot (the red bars) giving
#'     the proportions of the hours for which there are 0, 1 , 2, ... events.
#'     Also included are black bars showing the p.m.f. of a
#'     Poisson(\eqn{m}) random variable.}
#'   \item{"times between events" }{A histogram (with red rectangles) of
#'     the sample times between events, with the p.d.f. of an
#'     exponential(\eqn{m}) random variable superimposed.}
#'   \item{"times at which events occur" }{A histogram (with red rectangles)
#'     of the simulated event times, with the p.d.f. of a
#'     uniform(0, \code{hours}) random variable superimposed.}
#'   }
#'   The type of plot can be changed by clicking on the appropriate radio
#'   button.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @seealso \code{\link{poisson_process_movie}}: for similar plots based on
#'   data simulated from a Poisson process.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' # Data in which an event occurs exactly on the hour
#' \dontrun{
#' poisson_process_check(user_data = (1:24), 24, 24)
#' poisson_process_check(user_data = (1:24), 24, 24, exp_bins = 100)
#' }
#' @export
poisson_process_check <- function(user_data = NULL, total_time = NULL,
                                  intervals = 1, unif_bins = NULL,
                                  exp_bins = NULL) {
  if (is.null(user_data)) {
    stop("user_data must be supplied")
  }
  if (is.null(total_time)) {
    stop("total_time must be supplied")
  }
  if (total_time <= 0) {
    stop("total_time must be positive")
  }
  if (any(user_data > total_time)) {
    stop("total_time must be larger than max(user_data)")
  }
  if (intervals < 1) {
    stop("intervals must not be smaller than 1")
  }
  # Make intervals an integer, just in case the user didn't
  intervals <- round(intervals, 0)
  # Sort the data, just in case the user didn't
  event_times <- sort(user_data)
  inter_event_times <- c(event_times[1], diff(event_times))
  # Give the data class "poisson_process" so that its plot method can be used
  class(event_times) <- "poisson_process"
  # Estimate lambda
  lambda <- length(user_data) / intervals
  # Allocate the events to the correct hour
  alloc_fun <- function(x) {
    return(sum(event_times > x & event_times <= x + 1L))
  }
  # Extract features of the data for storage
  # Numbers of events in each hour
  n_events <- vapply(0:(total_time - 1L), alloc_fun, 0)
  # Create buttons for movie
  ppc_panel <- rpanel::rp.control("Data information",
                                  event_times = event_times,
                                  total_time = total_time,
                                  intervals = intervals,
                                  n_events = n_events,
                                  inter_event_times = inter_event_times,
                                  lambda = lambda, unif_bins = unif_bins,
                                  exp_bins = exp_bins)
  data_type <- "none"
  rpanel::rp.radiogroup(ppc_panel, data_type,
                c("none", "numbers of events in each hour",
                  "times between events", "times at which events occur"),
                action = poisson_process_check_plot, title = "Data type")
  rpanel::rp.do(ppc_panel, poisson_process_check_plot)
  return(invisible())
}

# Function to be called by poisson_process_check_plot().

poisson_process_check_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    graphics::layout(matrix(c(1,2), 2, 1), heights = c(1, 2))
    graphics::par(oma = c(0, 0, 0, 0), mar = c(4, 1, 2, 2) + 0.1)
    # Produce the top plot
    plot(event_times, ylim = c(-1, 1), ann = FALSE, pch = "", cex = 2,
         xlim = c(0, total_time))
    graphics::rug(event_times, pos = 0.2, ticksize = 0.15, lwd = 2,
                  col = "blue", quiet = TRUE)
    graphics::axis(1, at = 0:total_time)
    graphics::abline(v = 0:total_time, lty = 2, col = "grey")
    graphics::title(xlab = "time (hours)", line = 2.5)
    if (panel$data_type == "numbers of events in each hour") {
      graphics::text((1:total_time) - 0.5, rep(-0.7, total_time),
                     labels = n_events)
    }
    graphics::title(main=bquote(paste(estimate~of~lambda~"="~m == .(lambda))))
    #
    all_counts <- n_events
    all_inter_event_times <- inter_event_times
    all_event_times <- event_times
    #
    # Produce the bottom plot
    graphics::par(mar = c(4, 4, 2, 2) + 0.1)
    if (panel$data_type == "numbers of events in each hour") {
      small_pois_quantile <- qpois(0.0001, lambda = lambda)
      large_pois_quantile <- qpois(0.9999, lambda = lambda)
      smallest_count <- min(min(all_counts), small_pois_quantile)
      largest_count <- max(max(all_counts), large_pois_quantile)
      count_range <- smallest_count:largest_count
      temp_counts <- c(all_counts, count_range)
      pmf <- (table(temp_counts) - 1) / length(all_counts)
      poisson_pmf <- stats::dpois(count_range, lambda)
      # If the largest count in the plot has occurred in the data then add
      # another category that will include an observed proportion of zero
      if (largest_count ==  max(all_counts)) {
        pmf <- c(pmf, 0)
        poisson_pmf <- c(poisson_pmf, stats::ppois(largest_count,
                                                   lambda = lambda,
                                                   lower.tail = FALSE))
        largest_count <- largest_count + 1L
      } else {
        n_p <- length(poisson_pmf)
        poisson_pmf[n_p] <- stats::ppois(largest_count - 1L, lambda = lambda,
                                         lower.tail = FALSE)
      }
      names(pmf)[length(pmf)] <- paste(">", largest_count - 1L)
      # If the smallest count in the plot has occurred in the data, and this
      # count is not zero, then add another category that will include an
      # observed proportion of zero
      if (smallest_count > 0L) {
        if (smallest_count ==  min(all_counts)) {
          pmf <- c(0, pmf)
          smallest_count <- smallest_count - 1L
          poisson_pmf <- c(poisson_pmf, stats::ppois(smallest_count,
                                                     lambda = lambda))
        } else {
          poisson_pmf[1] <- stats::ppois(smallest_count, lambda = lambda)
        }
        names(pmf)[1] <- paste("<", smallest_count + 1)
      }
      pmf_mat <- rbind(pmf, poisson_pmf)
      if (ncol(pmf_mat) > 20) {
        cex.names <- 0.7
      } else if (ncol(pmf_mat) > 10) {
        cex.names <- 0.85
      } else {
        cex.names <- 1
      }
      graphics::barplot(pmf_mat, las = 1, width = 0.5, beside = TRUE,
                        col = 2:1, cex.names = cex.names)
      title(ylab = "p.m.f.", xlab = "number of events in one hour")
      poisson_text <- expression(Poisson*(m)*~pmf)
      legend("topright", col = 2:1, fill = 2:1, yjust = 0,
             legend = c("sample proportions", poisson_text))
    } else if (panel$data_type == "times between events") {
      if (is.null(exp_bins)) {
        number_of_classes <- grDevices::nclass.Sturges(all_event_times)
      } else {
        number_of_classes <- exp_bins
      }
      to <- max(max(all_inter_event_times), stats::qexp(0.99, rate = lambda))
      breaks <- seq(0, to, len = number_of_classes + 1)
      graphics::hist(all_inter_event_times, probability = TRUE, col = 2,
                     main = "", ylim = c(0, lambda), xlim = c(0, to),
                     xlab = "", axes = FALSE, breaks = breaks)
      graphics::axis(1, pos = 0)
      graphics::axis(2)
      title(xlab = "times between events")
      graphics::curve(stats::dexp(x, rate = lambda), from = 0, to = to,
                      n = 500, lwd = 2, add = TRUE)
      exp_text <- expression(exponential*(m)*~pdf~" ")
      legend("topright", col = 2:1, fill = c(2, NA), lty = c(-1, 1),
             lwd = c(0, 2), yjust = 0, border = c(1, 0),
             legend = c("sample data", exp_text))
    } else if (panel$data_type == "times at which events occur") {
      if (is.null(unif_bins)) {
        number_of_classes <- grDevices::nclass.Sturges(all_event_times)
      } else {
        number_of_classes <- unif_bins
      }
      breaks <- seq(0, total_time, len = number_of_classes + 1)
      graphics::hist(all_event_times, probability = TRUE, col = 2, main = "",
                     xlim = c(0, total_time), xlab = "", breaks = breaks,
                     axes = FALSE)
      graphics::axis(1, pos = 0)
      graphics::axis(2)
      title(xlab = "times at which events occur")
      graphics::curve(stats::dunif(x, min = 0, max = total_time), from = 0,
                      to = total_time, n = 500, lwd = 4, add = TRUE)
      unif_text <- expression(uniform*(0*","*total_time)*~pdf)
      u <- par("usr")
      legend(0, 0, col = 2:1, fill = c(2, NA),
             lty = c(-1, 1), lwd = c(0, 4), yjust = 0, xjust = 0,
             border = c(1, 0), legend = c("sample data", unif_text),
             xpd = TRUE)
    } else {
      plot(1:10, 1:10, type = "n", axes = FALSE, ann = FALSE)
    }
    graphics::par(old_par)
  })
  return(invisible(panel))
}

