# =========================== shuttle_movie ===========================

#' Challenger Space Shuttle Disaster Movie
#'
#' A movie to illustrate uncertainty in the linear logistic regression curves
#' fitted to the real space shuttle data.
#'
#' @param n_reps An integer scalar.  Relevant to \code{shuttle_movie} only.
#'   The number of flights to simulate
#'   for each of the 23 (pre-disaster) temperatures in the real dataset.
#'   For example, \code{n_reps = 10} means that we simulate a dataset of
#'   size 230.
#' @param pos A numeric integer.  Used in calls to \code{\link{assign}}
#'   to make information available across successive frames of a movie.
#'   By default, uses the current environment.
#' @param envir An alternative way (to \code{pos}) of specifying the
#'   environment. See \code{\link{environment}}.
#' @details
#' \code{shuttle_movie} produces an
#' animated version of the plot produced by \code{\link{shuttle_sim_plot}}.
#' Calling `shuttle_movie` creates a plot containing the real data
#' and the logistic curve fitted to the real data and opens a
#' parameter window containing buttons labelled + and -.
#' Clicking + adds to the plot a new simulated dataset and the logistic curve
#' fitted to this dataset.
#' Clicking - removes the mostly recently added dataset.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about STAT1004 movies.
#' @seealso \code{\link{shuttle_sim}} and \code{\link{shuttle_sim}} simulation
#'   of fake space shuttle data (from a linear logistic regression model)
#'   and plotting these data, respectively.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' # Movie based on datasets of size 23
#' shuttle_movie()
#'
#' # Movie based on datasets of size 230
#' shuttle_movie(n_reps = 10)
#' @export
shuttle_movie <- function(n_reps = 1, pos = 1, envir = as.environment(pos)) {
  n_sim <- 0
  # Set up vectors in which to store estimates from the logistic regression.
  asim <- NULL
  bsim <- NULL
  ysim_mat <- matrix(NA, nrow = 23 * n_reps, ncol = 1000)
  # Assign them to an environment so that they can be accessed inside
  # shuttle_sim_rpanel_plot()
  assign("asim", asim, envir = envir)
  assign("bsim", bsim, envir = envir)
  assign("ysim_mat", ysim_mat, envir = envir)
  # Do this also with n_reps
  assign("n_reps", n_reps, envir = envir)
  # Initialize n_sim_max, which keeps track of the number of fake datasets that
  # have been simulated.
  n_sim_max <- 0
  assign("n_sim_max", n_sim_max, envir = envir)
  shuttle_panel <- rpanel::rp.control("Simulate new fake data", envir = envir)
  #
  rpanel::rp.doublebutton(panel = shuttle_panel, variable = n_sim, step = 1,
                          range=c(0, 1000), repeatinterval = 20, initval = 0,
                          title = "number of simulations:", action = shuttle_movie_plot)
  rpanel::rp.do(shuttle_panel, shuttle_movie_plot)
  invisible()
}

# Function to be called by shuttle_movie().

shuttle_movie_plot <- function(panel){
  with(panel, {

    # Set up the data ----------
    #
    temperature <- stat1004::shuttle[1:23, 4]
    # Replicate the temperature data n_reps times.
    temperature <- rep(temperature, times = n_reps)
    n_temps <- length(temperature)
    #
    # Extract the real data from the (pre-disaster) flights.
    y <- cbind(stat1004::shuttle[1:23, 3], 6 - stat1004::shuttle[1:23, 3])
    x <- stat1004::shuttle[1:23, 4]
    #
    # Fit the logistic regression model ----------
    #
    shuttle_fit <- stats::glm(y ~ x, family = stats::binomial(link = "logit"))
    # Extract the estimated regression coefficients.
    alpha_hat <- shuttle_fit$coefficients[1]
    beta_hat <- shuttle_fit$coefficients[2]
    # Create a vector of estimated probabilities for the temperatures in
    # the vector temperatures.
    linear_predictor <- alpha_hat + beta_hat * temperature
    fitted_probs <- exp(linear_predictor) / (1 + exp(linear_predictor))
    #
    # Set up a plot area for the real data and the fitted logistic curve --------
    #
    new_damaged <- stat1004::shuttle[, 3]
    new_damaged[c(11, 13, 17, 22)] <- new_damaged[c(11, 13, 17, 22)] + 0.2
    new_damaged[15] <- new_damaged[15] - 0.2
    graphics::plot(stat1004::shuttle[, 4], new_damaged / 6, ann = FALSE,
                   ylim = c(0, 1), pch = 16, type = "n")
    graphics::title(xlab = "temperature (deg F)",
                    ylab = "proportion of distressed O-rings")
    temp <- seq(from = 30, to = 85, by = 0.1)
    linear_predictor <- alpha_hat + beta_hat * temp
    fitted_curve <- exp(linear_predictor) / (1 + exp(linear_predictor))
    #
    # Simulate a new dataset from the fitted logistic regression model
    # ... but only if dataset number n_sim has yet to be simulated.
    #
    if (n_sim > 0 & n_sim > n_sim_max) {
      y_sim <- stats::rbinom(n_temps, size = 6, prob = fitted_probs)
      ysim_mat[, n_sim] <- y_sim
      n_sim_max <- n_sim
      if (sum(y_sim) == 0){
        p <- rep(0, length(temp))
        asim[n_sim] <- -Inf
        bsim[n_sim] <- 0
      } else {
        y_sim <- cbind(y_sim, 6 - y_sim)
        shuttle_fit <- stats::glm(y_sim ~ temperature,
                                  family = stats::binomial(link = "logit"))
        asim[n_sim] <- shuttle_fit$coefficients[1]
        bsim[n_sim] <- shuttle_fit$coefficients[2]
        linear_predictor <- asim[n_sim] + bsim[n_sim] * temp
        sim_curve <- exp(linear_predictor) / (1 + exp(linear_predictor))
      }
      #
    }
    assign("asim", asim, envir = envir)
    assign("bsim", bsim, envir = envir)
    assign("ysim_mat", ysim_mat, envir = envir)
    assign("n_sim_max", n_sim_max, envir = envir)
    #
    if (n_sim > 1) {
      for (i in 1:n_sim){
        linear_predictor <- asim[i] + bsim[i] * temp
        sim_curve <- exp(linear_predictor) / (1 + exp(linear_predictor))
        graphics::lines(temp, sim_curve, lty = 1, col = "dark gray")
        if (i == n_sim) {
          graphics::lines(temp, sim_curve, lty = 1, col = "red", lwd = 2.5)
          graphics::points(temperature, ysim_mat[, n_sim] / 6, pch = 16,
                           col = "red")
        }
      }
      legend("topright", legend = c("estimated real curve", "simulated data",
                                    "simulated curve", "old simulated curves"),
             pch = c(-1, 16, -1, -1), lty = c(1, -1, 1, 1),
             lwd = c(2.5, -1, 2.5, 1),
             col = c("black", "red", "red", "dark gray"))
    } else if (n_sim == 1) {
      linear_predictor <- asim[1] + bsim[1] * temp
      sim_curve <- exp(linear_predictor) / (1 + exp(linear_predictor))
      graphics::lines(temp, sim_curve, lty = 1, col = "red", lwd = 2.5)
      graphics::points(temperature, ysim_mat[, n_sim] / 6, pch = 16,
                       col = "red")
      legend("topright", legend = c("estimated real curve", "simulated data",
                                    "simulated curve"),
             pch = c(-1, 16, -1), lty = c(1, -1, 1), lwd = c(2.5, -1, 2.5),
             col = c("black", "red", "red"))
    } else {
      graphics::points(stat1004::shuttle[, 4], new_damaged / 6, pch = 16)
      legend("topright", legend = c("estimated real curve", "real data"),
             pch = c(-1, 16), lty = c(1, -1), lwd = c(2.5, -1), col = "black")
    }
    graphics::lines(temp, fitted_curve, col = "black", lwd = 2.5)
    title(paste("Dataset of size", n_temps))

  })
  invisible(panel)
}
