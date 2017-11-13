# ============================ poisson_process_movie ==========================

#' Poisson process movie
#'
#' A movie to illustrate that if events arrive in a Poisson process of
#' rate \eqn{\lambda} per hour then the number of events that arrive
#' during a fixed interval of length \eqn{t} hours has a Poisson
#' distribution with mean \eqn{\lambda t}.
#'
#' @param lambda A numeric scalar.  The rate parameter of the exponential
#'   distribution from which data are to be simulated using \code{\link{rexp}}.
#' @param hours A positive integer scalar.  The number of hours for which to
#'   simulate a Poisson process of rate \eqn{lambda} events per hour.
#' @param pos A numeric integer.  Used in calls to \code{\link{assign}}
#'   to make information available across successive frames of a movie.
#'   By default, uses the current environment.
#' @param envir An alternative way (to \code{pos}) of specifying the
#'   environment. See \code{\link{environment}}.
#' @details This movie consists of two plots: one on the top of the display
#'   the other on the bottom.
#'
#'   The top plot displays the times at which events simulated from a
#'   Poisson process of rate \eqn{\lambda} events per hour occur during
#'   the time intervals (0, \code{hours} hours.  The total numbers of events
#'   that occur in each hour are also displayed on the plot.  Each time the
#'   button "simulate another sequence of events" is clicked a new set of
#'   simulated events is produced.
#'
#'   The bottom plot contains a barplot (the black bars) summarising how
#'   often the an hour contains 0, 1, 2, ... events.  Specifically, the
#'   proportions of the hours for which there are 0, 1 ,2, ... events is
#'   plotted on the vertical axis.  This is an estimate of the probability
#'   mass function of the random variable defined as the number of events
#'   that occur in a one hour period, when those event occur in a Poisson
#'   process of rate \eqn{\lambda} per hour.  Each time a set of events
#'   is simulated the totals in each hour are \strong{added} to the
#'   current collection of totals.
#'
#'   Also included in the bottom plot is a barplot (the red bars) showing
#'   the p.m.f. of a Poisson(\eqn{\lambda}) random variable.  As the number
#'   of totals added to plot increasing the black bars become closer and
#'   closer to the red bars.
#'
#'   Note also that the independence assumption underlying the Poisson
#'   process means that the numbers of events occurring in different
#'   hours are independent.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about STAT1004 movies.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' # Produce movie using values from the Aussie births data
#' \dontrun{
#' poisson_process_movie(lambda = 2, hours = 24)
#' }
#' @export
poisson_process_movie <- function(lambda = 1, hours = 24, pos = 1,
                                  envir = as.environment(pos), ...) {
  if (lambda <= 0L) {
    stop("lambda must be positive")
  }
  hours <- round(hours)
  if (hours < 1) {
    stop("hours must not be smaller than 1")
  }
  # Assign variables to an environment so that they can be accessed inside
  # clt_normal_movie_plot()
  lambda_vec <- c(floor(lambda), ceiling(lambda))
  ytop <- max(stats::dpois(lambda_vec, lambda)) * 1.25
  assign("lambda", lambda, envir = .GlobalEnv)
  assign("hours", hours, envir = .GlobalEnv)
  assign("ytop", ytop, envir = .GlobalEnv)
  all_counts <- NULL
  assign("all_counts", all_counts, envir = .GlobalEnv)
  # Create buttons for movie
  clt_panel <- rp.control("Poisson proces information", lambda = lambda,
                          hours = hours)
  rp.button(clt_panel, repeatinterval = 20,
            title = "simulate another sequence of events",
            action = poisson_process_movie_plot)
  rp.do(clt_panel, poisson_process_movie_plot)
  return(invisible())
}

# Function to be called by poisson_process_movie().

poisson_process_movie_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    graphics::par(mfrow = c(2, 1), oma = c(0, 0, 0, 0),
                  mar = c(4, 1, 2, 2) + 0.1)
    assign("lambda", lambda, envir = .GlobalEnv)
    assign("hours", hours, envir = .GlobalEnv)
    assign("ytop", ytop, envir = .GlobalEnv)
    # Simulate the total number of events in hours hours
    # [We could also have done this by simulating the times between events
    # from an exponential(lambda) distribution]
    total_count <- stats::rpois(1, lambda = lambda * hours)
    # Simulate the times at which the events occur given the total number
    event_times <- stats::runif(total_count, 0, hours)
    # Allocate the events to the correct hour
    alloc_fun <- function(x) {
      return(sum(event_times > x & event_times <= x + 1L))
    }
    # Produce the top plot
    n_events <- vapply(0:(hours - 1L), alloc_fun, 0)
    graphics::plot(event_times, rep(0.3, total_count), ylim = c(-1, 1),
                   axes = FALSE, ann = FALSE, pch = "", cex = 2,
                   xlim = c(0, hours))
    graphics::rug(event_times, pos = 0.2, ticksize = 0.15, lwd = 1.75,
                  col = "blue")
    graphics::axis(1, at = 0:hours)
    graphics::abline(v = 0:hours, lty = 2, col = "grey")
    graphics::title(xlab = "time (hours)")
    graphics::points((1:hours) - 0.5, rep(-0.7, hours), col = "blue",
                     pch = as.character(n_events))
    graphics::title(main=bquote(paste(lambda == .(lambda))))
    # Produce the bottom plot
    graphics::par(mar = c(4, 4, 2, 2) + 0.1)
    all_counts <- c(all_counts, n_events)
    assign("all_counts", all_counts, envir = .GlobalEnv)
    count_range <- min(all_counts):max(all_counts)
    temp_counts <- c(all_counts, count_range)
    pmf <- (table(temp_counts) - 1) / length(all_counts)
    poisson_pmf <- stats::dpois(count_range, lambda)
    pmf_mat <- rbind(pmf, poisson_pmf)
    graphics::barplot(pmf_mat, las = 1, width = 0.5, beside = TRUE,
                      ylim = c(0, ytop), col = 1:2)
    title(ylab = "p.m.f.", xlab = "number of events in one hour")
    pjn <- paste("Poisson", expression(lambda))
    pjn <- expression(Poisson*(lambda)*~pmf)
    legend("topright", col = 1:2, fill = 1:2, yjust = 0,
           legend = c("observed proportion", pjn))
    graphics::par(old_par)
  })
  return(invisible(panel))
}
