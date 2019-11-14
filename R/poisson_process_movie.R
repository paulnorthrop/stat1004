# ============================ poisson_process_movie ==========================

#' Poisson process movie
#'
#' A movie to illustrate that if events arrive in a
#' (one-dimensional, homogeneous) Poisson process of
#' rate \eqn{\lambda} per hour then (a) the number of events that arrive
#' during a fixed interval of length \eqn{t} hours has a Poisson
#' distribution with mean \eqn{\lambda t}; (b) the time between successive
#' events has an exponential distribution with mean \eqn{1/\lambda}; (c)
#' the arrival times are an (ordered) random sample from a uniform
#' distribution on the interval \eqn{(0,t)}.  Data are simulated from
#' a Poisson process to illustrate this.  Alternatively, the use may supply
#' their own arrival time data in order to assess, informally using graphs,
#' whether or not these data seem consistent with arising from a Poisson
#' process.
#'
#' @param lambda A positive numeric scalar.  The rate of the Poisson process.
#'   [\code{lambda} must not exceed 200 because the plots in the movie
#'   are not designed to work for larger values of \code{lambda}.]
#' @param hours A positive integer scalar.  The number of hours for which to
#'   simulate a Poisson process of rate \code{lambda} events per hour.
#'   For the purposes of this movie \code{hours} must not be smaller than 1.
#' @param pos A numeric integer.  Used in calls to \code{\link{assign}}
#'   to make information available across successive frames of a movie.
#'   By default, uses the current environment.
#' @param envir An alternative way (to \code{pos}) of specifying the
#'   environment. See \code{\link{environment}}.
#' @details This movie contains two displays of plots: one plot on the top and
#'   one of three other plots on the bottom.
#'
#'   Data are (repeatedly) simulated from a Poisson process of rate
#'   \eqn{\lambda} events per hour occur during the time interval
#'   (0, \code{hours}) hours.  The total numbers of events that occur in each
#'   hour are also displayed on the plot.  Each time the button "simulate
#'   another sequence of events" is clicked a new set of simulated events
#'   is produced.
#'
#'   The type of plot that appears in the bottom of the display depends
#'   on the radio button clicked by the user. The choices are
#'   \itemize{
#'   \item{"none" }{Nothing is plotted}
#'   \item{"numbers of events in each hour" }{A barplot (the red bars) giving
#'     the proportions of the hours for which there are 0, 1 , 2, ... events.
#'     Also included are black bars showing the p.m.f. of a
#'     Poisson(\eqn{\lambda}) random variable.}
#'   \item{"times between events" }{A histogram (with red rectangles) of
#'     the simulated times between events, with the p.d.f. of an
#'     exponential(\eqn{\lambda}) random variable superimposed.}
#'   \item{"times at which events occur" }{A histogram (with red rectangles)
#'     of the simulated event times, with the p.d.f. of a
#'     uniform(0, \code{hours}) random variable superimposed.}
#'   }
#'   Each time the "simulate another sequence of events" button is clicked
#'   [or the currently-selected radio button is clicked again] then a new
#'   set of events is simulated and these event are \strong{added} to
#'   the current collection of simulated events.
#'
#'   \strong{Note:} During the early stages of the simulations the
#'     heights of the black bars in the "numbers of events in each hour"
#'     plot may be incorrect because they extend beyond the plot region.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @seealso \code{\link{poisson_process_check}}: for similar plots based on
#'   data supplied by a user.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' \dontrun{
#' poisson_process_movie(lambda = 2)
#' poisson_process_movie(lambda = 10)
#' poisson_process_movie(lambda = 0.5)
#' }
#' @export
poisson_process_movie <- function(lambda = 1, hours = 24, pos = 1,
                                  envir = as.environment(pos)) {
  if (lambda <= 0) {
    stop("lambda must be positive")
  }
  if (lambda > 200) {
    stop("The value of lambda is too large.  See the help file for details.")
  }
  hours <- round(hours)
  if (hours < 1) {
    stop("hours must not be smaller than 1")
  }
  # Assign variables to an environment so that they can be accessed inside
  # poisson_process_movie_plot()
  lambda_vec <- c(floor(lambda), ceiling(lambda))
  ytop <- max(stats::dpois(lambda_vec, lambda)) * 1.5
  assign("lambda", lambda, envir = envir)
  assign("hours", hours, envir = envir)
  assign("ytop", ytop, envir = envir)
  assign("old_type", "none", envir = envir)
  event_times <- NULL
  assign("event_times", event_times, envir = envir)
  all_counts <- NULL
  assign("all_counts", all_counts, envir = envir)
  all_inter_event_times <- NULL
  assign("all_inter_event_times", all_inter_event_times, envir = envir)
  all_event_times <- NULL
  assign("all_event_times", all_event_times, envir = envir)
  data_type <- "none"
  # Create buttons for movie
  ppm_panel <- rpanel::rp.control("Poisson process information",
                                  lambda = lambda, hours = hours,
                                  envir = envir)
  rpanel::rp.button(ppm_panel, repeatinterval = 20,
                    title = "simulate another sequence of events",
                    action = poisson_process_movie_plot)
  rpanel::rp.radiogroup(ppm_panel, data_type,
                c("none", "numbers of events in each hour",
                  "times between events", "times at which events occur"),
                action = poisson_process_movie_plot, title = "Data type")
  rpanel::rp.do(ppm_panel, poisson_process_movie_plot)
  return(invisible())
}

# Function to be called by poisson_process_movie().

poisson_process_movie_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    graphics::layout(matrix(c(1,2), 2, 1), heights = c(1, 2))
    graphics::par(oma = c(0, 0, 0, 0), mar = c(4, 1, 2, 2) + 0.1)
    assign("lambda", lambda, envir = envir)
    assign("hours", hours, envir = envir)
    assign("ytop", ytop, envir = envir)
    if (data_type == old_type) {
      # Simulate events from a Poisson process of rate lambda per hour
      # over the time interval (0, h) hours
      event_times <- poisson_process_sim(lambda = lambda, hours = hours)
      assign("event_times", event_times, envir = envir)
    }
    # Allocate the events to the correct hour
    alloc_fun <- function(x) {
      return(sum(event_times > x & event_times <= x + 1L))
    }
    # Extract features of the data for storage
    n_events <- vapply(0:(hours - 1L), alloc_fun, 0)
    if (data_type == old_type) {
      # Numbers of events in each hour
      all_counts <- c(all_counts, n_events)
      assign("all_counts", all_counts, envir = envir)
      # Times between events
      inter_event_times <- c(event_times[1], diff(event_times))
      all_inter_event_times <- c(all_inter_event_times, inter_event_times)
      assign("all_inter_event_times", all_inter_event_times, envir = envir)
      # Times at which events occur
      all_event_times <- c(all_event_times, event_times)
      assign("all_event_times", all_event_times, envir = envir)
    }
    assign("old_type", data_type, envir = envir)
    # Produce the top plot
    plot(event_times, ylim = c(-1, 1), ann = FALSE, pch = "", cex = 2,
         xlim = c(0, hours))
    graphics::rug(event_times, pos = 0.2, ticksize = 0.15, lwd = 2,
                  col = "blue", quiet = TRUE)
    graphics::axis(1, at = 0:hours)
    graphics::abline(v = 0:hours, lty = 2, col = "grey")
    graphics::title(xlab = "time (hours)", line = 2.5)
    if (panel$data_type == "numbers of events in each hour") {
      graphics::text((1:hours) - 0.5, rep(-0.7, hours), labels = n_events)
    }
    graphics::title(main=bquote(paste(lambda == .(lambda))))
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
                        ylim = c(0, ytop), col = 2:1, cex.names = cex.names)
      title(ylab = "p.m.f.", xlab = "number of events in one hour")
      poisson_text <- expression(Poisson*(lambda)*~pmf)
      legend("topright", col = 2:1, fill = 2:1, yjust = 0,
             legend = c("simulated proportions", poisson_text))
    } else if (panel$data_type == "times between events") {
      to <- max(max(all_inter_event_times), stats::qexp(0.99, rate = lambda))
      graphics::hist(all_inter_event_times, probability = TRUE, col = 2,
                     main = "", ylim = c(0, lambda), xlim = c(0, to),
                     xlab = "", axes = FALSE)
      graphics::axis(1, pos = 0)
      graphics::axis(2)
      title(xlab = "times between events")
      graphics::curve(stats::dexp(x, rate = lambda), from = 0, to = to,
                      n = 500, lwd = 2, add = TRUE)
      exp_text <- expression(exponential*(lambda)*~pdf~" ")
      legend("topright", col = 2:1, fill = c(2, NA), lty = c(-1, 1),
             lwd = c(0, 2), yjust = 0, border = c(1, 0),
             legend = c("simulated data", exp_text))
    } else if (panel$data_type == "times at which events occur") {
      number_of_classes <- grDevices::nclass.Sturges(all_event_times)
      breaks <- seq(0, hours, len = number_of_classes)
      graphics::hist(all_event_times, probability = TRUE, col = 2, main = "",
                     xlim = c(0, hours), xlab = "", breaks = breaks,
                     axes = FALSE)
      graphics::axis(1, pos = 0)
      graphics::axis(2)
      title(xlab = "times at which events occur")
      graphics::curve(stats::dunif(x, min = 0, max = hours), from = 0,
                      to = hours, n = 500, lwd = 2, add = TRUE)
      unif_text <- expression(uniform*(0*","*hours)*~pdf)
      u <- par("usr")
      legend(0, 0, col = 2:1, fill = c(2, NA),
             lty = c(-1, 1), lwd = c(0, 2), yjust = 0, xjust = 0,
             border = c(1, 0), legend = c("simulated data", unif_text),
             xpd = TRUE)
    } else {
      plot(1:10, 1:10, type = "n", axes = FALSE, ann = FALSE)
    }
    graphics::par(old_par)
  })
  return(invisible(panel))
}

