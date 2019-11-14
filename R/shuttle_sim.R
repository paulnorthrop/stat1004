# =========================== shuttle_sim ===========================

#' Simulate fake space shuttle data
#'
#' Simulates fake O-ring thermal distress data for Challenger Space Shuttle
#' launches at different launch temperatures.  The simulated data are based
#' on a linear logistic regression model fitted to the real data from the 23
#' (pre-disaster) launches.
#'
#' @param n_sim A integer scalar. The number of fake datasets to simulate.
#' @param temperature A numeric vector of launch temperatures.
#'
#'   If \code{temperature} is not supplied then the 23 launch temperatures
#'   from the real dataset are used.
#'
#'   If \code{temperature} is supplied then it must be a vector of length one
#'   (i.e. a scalar). In this event \code{n_sim} gives the number of
#'   simulated numbers of damaged O-rings at launch temperature
#'   \code{temperature}.  If \code{temperature} has length greater than one
#'   then only the first element of \code{temperature} is used and a warning
#'   is given.
#'
#' @details The data are simulated from a linear logistic regression model
#'   fitted to the real (pre-disaster) O-ring distress and launch
#'   temperature data.  For a given launch temperature \eqn{t} this model
#'   provides an estimate, \eqn{phat(t)} say, of the probability that an
#'   O-ring suffers thermal distress.  Then the number of the 6 O-rings
#'   that suffers from thermal distress is simulated from a
#'   binomial(6, \eqn{phat(t)}) distribution, under an assumption that
#'   the fates of the O-rings are independent.  This is repeated for each of
#'   the launch temperatures in \code{temperatures}.
#'   For further details see the
#'   \href{../doc/stat1004-shuttle-vignette.html}{Challenger Space Shuttle Disaster}
#'   vignette.
#'
#' @return The output depends on whether or not \code{temperature} is supplied by
#' the user.
#'
#' If \code{temperature} \emph{is} supplied then \code{shuttle_sim}
#' returns a dataframe with 2 + \code{n_sim} columns.
#' Column 1 contains the launch temperatures, column 2 contains the numbers
#' of distressed O-rings in the real data and columns 3 to
#' 2 + \code{n_sim} the \code{n_sim} simulated datasets.
#'
#' If \code{temperature} \emph{is not} supplied then \code{shuttle_sim}
#' returns a vector of length \code{n_sim}.
#'
#' @examples
#' # Simulate 10 fake datasets of size 23, using the real temperatures.
#' res <- shuttle_sim(n_sim = 10)
#' res
#' # Simulate the number of distressed O-rings for 1000 launches at 31 deg F.
#' res <- shuttle_sim(n_sim = 1000, temperature = 31)
#' res[1:100]
#' table(res)
#' @seealso The \href{../doc/stat1004-shuttle-vignette.html}{Challenger Space Shuttle Disaster}
#'   vignette.
#' @seealso \code{\link{shuttle_sim_plot}} for assessing uncertainty concerning
#'   the modelling of the space shuttle data using simulation.
#' @export
shuttle_sim <- function(n_sim = 1, temperature = NULL) {
  #
  # Set up the data ----------
  #
  # If temperature is NULL (i.e. not supplied by the user) then use
  # the temperatures from the real dataset.
  #
  # We create a logical scalar, using_real_temps, to indicate whether or
  # not we are using the temperatures from the real dataset.
  if (is.null(temperature)) {
    temperature <- stat1004::shuttle[1:23, 4]
    using_real_temps <- TRUE
  } else {
    if (length(temperature) != 1) {
      warning("Only the first element of ``temperature'' has been used.")
      temperature <- temperature[1]
    }
    using_real_temps <- FALSE
  }
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
  # Simulate data from the fitted logistic regression model ----------
  #
  # Note that if n_sim > 1 then the argument n to rbinom() will be
  # a vector with a length that is n_sim times the length of the
  # argument prob.  In this event the rbinom() function recycles the
  # values in prob to create a vector that is the same length as n.
  y_sim <- stats::rbinom(n = length(x) * n_sim, size = 6, prob = fitted_probs)
  # Reshape the simulated data so that column 1 contains the 1st set of
  # simulated data, column 2 contains the 2nd set etc.
  y_sim <- matrix(y_sim, ncol = n_sim, nrow = length(x), byrow = FALSE)
  #
  # Format the output depending the value of using_real_temps ----------
  #
  if (using_real_temps) {
    # Number of missing value codes NA which which the pad column 2.
    na_pad <- length(temperature) - 23
    ret_val <- data.frame(temps = temperature,
                          real = c(stat1004::shuttle[1:23, 3], rep(NA, na_pad)),
                          y_sim)
    colnames(ret_val)[-(1:2)] <- paste("sim", 1:n_sim, sep = "")
  } else {
    ret_val <- y_sim
  }
  return(ret_val)
}

# ========================= shuttle_sim_plot ========================

#' Space shuttle: uncertainty in fitted linear logistic regression curve
#'
#' Illustrates the uncertainty in the fitted values of the probability
#' of O-ring distress for different values of temperature, based on the
#' linear logistic regression model fitted to the real data.
#' Fake data from this fitted model are simulated repeatedly using
#' the function \code{\link{shuttle_sim}}.  The linear logistic model
#' is fitted to each of these simulated datasets.
#' A plot is produced containing the real proportions of distressed
#' O-rings, the logistic curve fitted to the real data and each of the
#' logistic curves fitted to the fake data.
#'
#' @param n_sim An integer scalar.  The number of fake datasets to simulate,
#'   and hence the number of curves from simulated data to appear in the plot.
#' @param plot_real_data A logical scalar.  Should we add to the plot the
#'   real data and the linear logistic curve fitted to the real data?
#'   \code{real_data = TRUE} for ``yes'' and \code{real_data = FALSE}
#'   for ``no''.
#' @param n_reps An integer scalar.  The number of flights to simulate
#'   for each of the 23 (pre-disaster) temperatures in the real dataset.
#'   For example, \code{n_reps = 10} means that we simulate a dataset of
#'   size 230.
#' @param plot A logical scalar indicating whether or not to produce the plot.
#' @param ... Further arguments to be passed to the \code{lines} function
#'   used to draw the curves for the simulated datasets.
#' @details For details of the linear logistic model see
#'   \href{../doc/stat1004-shuttle-vignette.html}{Challenger Space Shuttle Disaster}
#'   vignette and for simulation from this model see
#'   \code{\link{shuttle_sim}}.
#'
#' @return A numeric matrix with 2 columns and \code{n_sim} rows.
#'   Each row contains the estimates of the parameters of the linear
#'   logistic regression model fitted to a simulated dataset.
#'   The first column, \code{alpha_hat}, contains the estimates of the
#'   intercept parameter, the second column, \code{beta_hat},
#'   the estimates of the slope parameter.
#' @examples
#' shuttle_sim_plot(n_sim = 50)
#' @seealso The \href{../doc/stat1004-shuttle-vignette.html}{Challenger Space Shuttle Disaster}
#'   vignette.
#' @seealso \code{\link{shuttle_sim}} for simulating fake space shuttle data.
#' @seealso \code{\link{lines}} for arguments that can be supplied in ....
#' @export
shuttle_sim_plot <- function(n_sim = 50, plot_real_data = TRUE, n_reps = 1,
                             plot = TRUE, ...) {
  #
  # Extract user-supplied arguments in ... (if any) that will apply to the
  # curves for the simulated data.
  for_lines <- list(...)
  # If user hasn't specified the line type set it to lty = 2.
  if (is.null(for_lines$lty)) {
    for_lines$lty <- 2
  }
  # Similarly set line colour to "black".
  if (is.null(for_lines$col)) {
    for_lines$col <- "black"
  }
  # ... and set line width to 2.
  if (is.null(for_lines$lw)) {
    for_lines$lwd <- 1.5
  }
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
  if (plot) {
    graphics::plot(stat1004::shuttle[, 4], new_damaged / 6, ann = FALSE,
                   ylim = c(0, 1), pch = 16, type = "n")
    graphics::title(xlab = "temperature (deg F)",
                    ylab = "proportion of distressed O-rings")
  }
  temp <- seq(from = 30, to = 85, by = 0.1)
  linear_predictor <- alpha_hat + beta_hat * temp
  fitted_curve <- exp(linear_predictor) / (1 + exp(linear_predictor))
  #
  # Simulate data from the fitted logistic regression model ----------
  #
  sim_pars <- matrix(NA, ncol = 2, nrow = n_sim)
  colnames(sim_pars) <- c("alpha_hat", "beta_hat")
  for (i in 1:n_sim){
    y_sim <- stats::rbinom(n_temps, size = 6, prob = fitted_probs)
    if (sum(y_sim) == 0){
      p <- rep(0, length(temp))
      alpha_hat <- -Inf
      beta_hat <- 0
    } else {
      y_sim <- cbind(y_sim, 6 - y_sim)
      shuttle_fit <- stats::glm(y_sim ~ temperature,
                                family = stats::binomial(link = "logit"))
      alpha_hat <- shuttle_fit$coefficients[1]
      beta_hat <- shuttle_fit$coefficients[2]
      linear_predictor <- alpha_hat + beta_hat * temp
      sim_curve <- exp(linear_predictor) / (1 + exp(linear_predictor))
    }
    sim_pars[i,] <- c(alpha_hat, beta_hat)
    lines_args <-c(list(x = temp, y = sim_curve), for_lines)
    if (plot) {
      do.call(graphics::lines, lines_args)
    }
  }
  if (!plot) {
    return(invisible(sim_pars))
  }
  # Plot again the real data and the fitted curve to avoid them being hidden.
  if (plot_real_data) {
    graphics::lines(temp, fitted_curve, col = "blue", lwd = 2.25)
    graphics::points(stat1004::shuttle[, 4], new_damaged / 6, pch = 16)
  }
  if (plot_real_data) {
    graphics::legend("topright", legend =
                       c("real data", "curve estimated from real data",
                         paste(n_sim, "curves estimated from simulated data")),
                     pch = c(16, -1, -1), lty = c(-1, 1, for_lines$lty),
                     lwd = c(-1, 2.25, for_lines$lwd),
                     col = c("black", "blue", for_lines$col), cex = 0.8)
  }
  invisible(sim_pars)
}

# ========================= shuttle_sim_hists ========================

#' Space shuttle: uncertainty in estimated probability of O-ring damage
#'
#' Illustrates the uncertainty in the estimated probability of an O-ring
#' suffering thermal distress for a given launch temperature.
#'
#' @param x A 2-column matrix returned from a call to
#'   \code{\link{shuttle_sim_plot}}
#' @param temps A numeric vector of temperatures, in degrees Fahrenheit.
#' @param ... Further arguments to be passed to \code{hist}. [Apart from
#'  \code{probability}, \code{xlab}, \code{ylab} and \code{main},
#'  which are set inside \code{shuttle_sim_hists}.]
#' @details
#'
#' For details of the linear logistic model see
#'   \href{../doc/stat1004-shuttle-vignette.html}{Challenger Space Shuttle Disaster}
#'   vignette and for simulation from this model see
#'   \code{\link{shuttle_sim}}.
#'
#' @return Nothing, just the plot.
#' @examples
#' x <- shuttle_sim_plot(n_sim = 1000, plot = FALSE)
#' shuttle_sim_hists(x, temps = c(31, 50, 65, 80), col = 8)
#' @seealso \code{\link{shuttle_sim_plot}} and \code{\link{shuttle_sim}}.
#' @seealso The \href{../doc/stat1004-shuttle-vignette.html}{Challenger Space Shuttle Disaster}
#'   vignette.
#' @export
shuttle_sim_hists <- function(x, temps, ...) {
  # save default, for resetting...
  def.par <- graphics::par(no.readonly = TRUE)
  # Sort temperatures into decreasing order
  temps <- sort(temps, decreasing = TRUE)
  ntemps <- length(temps)
  graphics::par(mfrow=c(ntemps, 1), oma = c(0, 0, 0, 0), font.main = 1,
                mar = c(4, 4, 2, 1))
  for (i in 1:ntemps) {
    linear_predictor <- x[, 1] + x[, 2] * temps[i]
    fitted_probs <- exp(linear_predictor) / (1 + exp(linear_predictor))
    graphics::hist(fitted_probs, prob = TRUE, main = "", xlim = c(0,1),
                   xlab = "", ylab = "", ...)
    graphics::title(xlab = paste("proportion of damaged O-rings at",
                                 temps[i], "deg F"), ylab = "density")
  }
  #- reset to default
  graphics::par(def.par)
  invisible()
}
