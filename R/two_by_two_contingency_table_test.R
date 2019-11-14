# ============================== two_by_two_movie =============================

#' Test for lack of association in a 2 by 2 contingency table
#'
#' A movie to study the distribution of the Pearson chi-squared test statistic
#' used to test for lack of association in a 2 by 2 contingency table.
#'
#' @param data A numeric 2 by 2 matrix, giving the observed frequencies of
#'   a 2 by 2 contingency table.
#' @param bin_width A numeric scalar.  The width of the bins in the histogram
#'   of the test statistics plotted on the bottom on the movie.
#' @param pos A numeric integer.  Used in calls to \code{\link{assign}}
#'   to make information available across successive frames of a movie.
#'   By default, uses the current environment.
#' @param envir An alternative way (to \code{pos}) of specifying the
#'   environment. See \code{\link{environment}}.
#' @details The movie is split into three sections.
#'   In the top left is a table displaying the contingency table based on
#'   the frequencies in \code{data}, with row totals, column totals and the
#'   grand total added.  If \code{data} has row and column names then
#'   only the first letters of these are added to the table.
#'   In the top right is a similar table containing frequencies based on
#'   simulated data.  The simulated data has the same grand total as
#'   \code{data}.  The data are simulated under the assumption that the
#'   value of the variable in the row of the table is not associated with
#'   the value of the variable in the column of the table.
#'   See Section 7.1.2 of the STAT0002 notes for details.
#'
#'   Under each of these tables the calculation of the Pearson
#'   chi-squared test statistic is given.  Every time a new simulated dataset
#'   is produced the value of the test statistic is added to a histogram
#'   containing all the test statistics of simulated data produced.
#'   The most recent simulated test statistic is indicated in this plot
#'   with a red circle.
#'   The test statistic produced from the real data is indicated in this plot
#'   with a blue circle.
#'   The p.d.f. of a chi-squared random variable with
#'   one degree of freedom is superimposed on the plot.
#'   If the expected frequencies based on the real data are sufficiently
#'   large then the distribution of the test statistic under the null
#'   hypothesis of no association has approximately this distribution.
#'
#'   Three radio buttons enable the user to choose whether to simulate
#'   1, 100 or 1000 datasets.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' \dontrun{
#' # Ignore department
#' sex_outcome <- apply(UCBAdmissions, 2:1, FUN = sum)
#' colnames(sex_outcome) <- c("A", "R")
#' rownames(sex_outcome) <- c("M", "F")
#' two_by_two_movie(data = sex_outcome)
#'
#' # Conditon on department 1
#' sex_outcome_1 <- UCBAdmissions[, , 1]
#' colnames(sex_outcome_1) <- c("A", "R")
#' rownames(sex_outcome_1) <- c("M", "F")
#' two_by_two_movie(data = sex_outcome_1)
#'
#' # Conditon on department 2
#' sex_outcome_2 <- UCBAdmissions[, , 2]
#' colnames(sex_outcome_2) <- c("A", "R")
#' rownames(sex_outcome_2) <- c("M", "F")
#' two_by_two_movie(data = sex_outcome_2)
#' }
#' @export
two_by_two_movie <- function(data, bin_width = 0.25,
                                 pos = 1, envir = as.environment(pos)) {
  if (is.null(data)) {
    stop("data must be supplied")
  }
  if (any(data < 0) || anyNA(data)) {
    stop("all entries of data must be non-negative and finite")
  }
  if (sum(data) == 0) {
    stop("at least one entry of data must be positive")
  }
  if (!is.matrix(data)) {
    stop("data must be a matrix")
  }
  if (nrow(data) != 2 || ncol(data) != 2) {
    stop("data must be a matrix with 2 rows and 2 columns")
  }
  # Performs chi-squared test on the real data
  correct <- FALSE
  real_test_res <- suppressWarnings(stats::chisq.test(data, correct = correct))
  if (!is.null(colnames(data))) {
    colnames(data) <- substr(colnames(data), 1, 1)
  }
  if (!is.null(rownames(data))) {
    rownames(data) <- substr(rownames(data), 1, 1)
  }
  # Add row and column sums and the total frequency
  real_data <- add_sums(data)
  # Sample size
  n <- sum(data)
  # Estimate the probabilities of being in each of the 4 cells in the table
  p_hat <- (rowSums(data) %o% colSums(data)) / n ^ 2
  sim_test_stats <- NULL
  assign("sim_test_stats", sim_test_stats, envir = envir)
  tbt_panel <- rpanel::rp.control("2 x 2 contingency table simulation",
                                  real_data = real_data,
                                  real_test_res = real_test_res,
                                  n = n, p_hat = p_hat,
                                  bin_width = bin_width, correct = correct,
                                  envir = envir)
  nsim <- 1
  rpanel::rp.radiogroup(tbt_panel, nsim, c("1", "100", "1000"),
                        action = two_by_two_plot,
                        title = "Choose the number of 2 x 2 tables to simulate")
  rpanel::rp.do(tbt_panel, two_by_two_plot)
  return(invisible())
}

# Function to add row, column and total sums to a contingency table

add_sums <- function(x) {
  r_sums <- rowSums(x)
  c_sums <- colSums(x)
  t_sum <- sum(x)
  x <- cbind(x, total = r_sums)
  x <- rbind(x, total = c(c_sums, t_sum))
  return(x)
}

# Function to add chi-squared test statistic calculation

add_chi_squared_calc <- function(x_loc, y_loc, x) {
  my_cex <- 1.35
  real_obs <- c(t(x$observed))
  real_exp <- c(round(t(x$expected), 1))
  o_val <- real_obs
  e_val <- real_exp
  my_text <- substitute(frac((a1 - b1) ^ 2, b1) +
                          frac((a2 - b2) ^ 2, b2),
                        list(a1 = o_val[1], b1 = e_val[1],
                             a2 = o_val[2], b2 = e_val[2]))
  graphics::text(x_loc, y_loc, my_text, cex = my_cex, pos = 4)
  my_text <- substitute(+ frac((a3 - b3) ^ 2, b3) +
                          frac((a4 - b4) ^ 2, b4) == test_stat,
                        list(a3 = o_val[3], b3 = e_val[3],
                             a4 = o_val[4], b4 = e_val[4],
                             test_stat = round(x$statistic, 2)))
  graphics::text(x_loc, y_loc - 0.25, my_text, cex = my_cex, xpd = TRUE, pos = 4)
  return(invisible())
}

# Function to be called by two_by_two_sim_movie().

two_by_two_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    # Create layout of
    # 1. contingency table of real data on the top left
    # 2. contingency table of simulated data on the top right
    # 3. histogram and chi-squared density on the bottom
    graphics::layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE),
                     widths = c(1, 1), heights = c(1, 1))
    graphics::par(oma = c(0, 0, 0, 0), mar = c(2.5, 2, 1, 2) + 0.1)
    # 1. Produce the table on the top left
    dum_x <- c(0, 0, 1, 1)
    dum_y <- c(0, 1, 0, 1) # summary data for plot
    graphics::plot(dum_x, dum_y, type = "n", ann = FALSE, axes = FALSE)
    my_table <- real_data
    plotrix::addtable2plot(0.5, 0.8, my_table, cex = 1.5, bty = "n",
                           display.rownames = TRUE, display.colnames = TRUE,
                           hlines = TRUE, vlines = TRUE, title = "",
                           xpad = 0.5, ypad = 1.2, xjust = 0.5, yjust = 0.5,
                           text.col = 1:5)
    add_chi_squared_calc(0., 0.275, real_test_res)
    # 2. Produce the table on the top right
    # Simulate nsim 2 x 2 tables under the null hypothesis that the margins
    # are independent
    chisq_test_fun <- function(x) {
      temp <- suppressWarnings(stats::chisq.test(matrix(x, nrow = 2),
                                                 correct = correct)$statistic)
      return(temp)
    }
    full_chisq_test_fun <- function(x) {
      temp <- suppressWarnings(stats::chisq.test(matrix(x, nrow = 2),
                                                 correct = correct))
      return(temp)
    }
    big_sim_data <- stats::rmultinom(nsim, n, p_hat)
    sim_data <- matrix(big_sim_data[, ncol(big_sim_data)], nrow = 2)
    add_sim_test_stats <- apply(big_sim_data, 2, chisq_test_fun)
    # Store the new values of the test statistics
    sim_test_stats <- c(sim_test_stats, add_sim_test_stats)
    # Repeat the most recent test for displaying on the plot
    sim_test_res <- suppressWarnings(stats::chisq.test(sim_data))
    assign("sim_test_stats", sim_test_stats, envir = envir)
    graphics::plot(dum_x, dum_y, type = "n", ann = FALSE, axes = FALSE)
    graphics::title(main = "simulated 2 x 2 table", line = -0.25)
    # Add row and column sums and the total frequency
    my_table <- add_sums(sim_data)
    plotrix::addtable2plot(0.5, 0.725, my_table, cex = 1.5, bty = "n",
                           display.rownames = FALSE, display.colnames = FALSE,
                           hlines = TRUE, vlines = TRUE, title = "",
                           xpad = 0.5, ypad = 1.2, xjust = 0.5, yjust = 0.5,
                           text.col = 1:5)
    # Performs chi-squared test on the real data
    add_chi_squared_calc(0, 0.275, sim_test_res)
    # 3. Produce the bottom plot
    big_val <- max(10, ceiling(max(sim_test_stats)))
    my_breaks <- seq(0, big_val, by = bin_width)
    max_dens <- max(graphics::hist(sim_test_stats, plot = FALSE,
                                   breaks = my_breaks)$density)
    max_y <- max(max_dens, 1.1)
    graphics::hist(sim_test_stats, probability = TRUE, col = 8,
                   xlim = c(0, big_val), ylim = c(0, max_y), main = "",
                   breaks = my_breaks,
                   axes = FALSE, ann = FALSE)
    graphics::axis(1, pos = 0, mgp = c(3, 0.5, 0))
    graphics::axis(2, pos = 0)
    graphics::title(xlab = "sum of squared Pearson residuals (test statistic)",
                    line = 1.2, cex.lab = 1.5)
    graphics::title(ylab = "density", line = 0.6, cex.lab = 1.5)
    graphics::curve(stats::dchisq(x, df = 1), from = 0, to = big_val,
                    n = 500, lty = 1, lwd = 2, add = TRUE)
    if (real_test_res$statistic > big_val) {
      graphics::legend("topright",
                       legend = c(expression(paste(chi[1]^2," density")),
                                  "last simulated", "real (off the scale!)"),
                       lty = c(1, -1, -1), lwd = c(2, 0, 0),
                       pch = c(-1, 16, 16), col = c("black", "red", "blue"),
                       pt.cex = 2, cex = 1.5)
    } else {
      graphics::legend("topright",
                       legend = c(expression(paste(chi[1]^2," density")),
                                  "last simulated", "real"),
                       lty = c(1, -1, -1), lwd = c(2, 0, 0),
                       pch = c(-1, 16, 16), col = c("black", "red", "blue"),
                       pt.cex = 2, cex = 1.5)
    }
    graphics::points(sim_test_res$statistic, 0, pch = 16, cex = 2,
                     col = "red")
    graphics::points(real_test_res$statistic, 0, pch = 16, cex = 2,
                     col = "blue")
    graphics::par(old_par)
  })
  return(invisible(panel))
}

