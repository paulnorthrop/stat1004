# ================================= lin_reg_movie =============================

#' Simple linear regression movie
#'
#' A movie to help to visualize the fitting of a regression line using
#' least squares in the case of a simple linear regression, that is,
#' a linear regression of one response variable on one explanatory variable.
#'
#' @param data A data frame with two variables or a numeric matrix with
#'   2 columns.  The first column must contain the response data, the second
#'   column the explanatory data.
#' @param delta_alpha,delta_beta Numeric scalars.  The respective amounts by
#'   which the values of the intercept and gradient of the line are
#'   increased/decreased after one click of the +/- button.
#'   The default values are set with the \code{\link{hubble}} data used
#'   in the example below in mind.
#' @param ... Further arguments, such as graphical parameters
#'   (see \code{\link[graphics]{par}} to be passed to
#'   \code{\link[graphics]{plot}} when producing a scatter plot of the
#'   response data against the explanatory data.  For example, the
#'   plotting character used for the points can be chosen using \code{pch}.
#'   If \code{pch} has length greater than 1 then only the first element
#'   is used.  The default value of \code{pch} is 16 (filled circle).
#'   The labels on the horizontal and vertical axes can be specified
#'   using \code{xlab} and \code{ylab} respectively.
#' @details A scatter plot of response data against the explanatory data
#'   is produced.  On this plot is superimposed a dashed line that the user
#'   can move, by changing its intercept alpha and gradient beta
#'   using +/- buttons.  The initial value of alpha is the mean of the
#'   response data and the initial value of beta is 0.
#'   The sizes of the residuals are shown using red lines.
#'   One of the legends gives the current sum of squares residuals (SS).
#'
#'   Another +/- button allows the user to add the least squares regression
#'   line to the plot, and the associated residual sum of squares (RSS) to
#'   the legend, for comparison.
#'
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: general information about the movies.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' # Produce movie using values from the Aussie births data
#' \dontrun{
#' lin_reg_movie(hubble, pch = 16, xlab = "recession velocity (km/sec)",
#'               ylab = "distance (megaparsecs)")
#' }
#' @export
lin_reg_movie <- function(data, delta_alpha = 0.1, delta_beta = 0.0001, ...) {
  if (!is.data.frame(data) & !is.matrix(data)) {
    stop("data must be a data frame or a matrix")
  }
  if (ncol(data) != 2) {
    stop("data must have 2 columns")
  }
  user_args <- list(...)
  if (is.null(user_args$xlab)) {
    user_args$xlab <- "x"
  }
  if (is.null(user_args$ylab)) {
    user_args$ylab <- "y"
  }
  if (!is.null(user_args$pch)) {
    user_args$pch <- user_args$pch[1]
  } else {
    user_args$pch <- 16
  }
  if (!is.null(colnames(data))) {
    var_names <- colnames(data)
    y_name <- var_names[1]
    x_name <- var_names[2]
  } else {
    y_name <- "y"
    x_name <- "x"
  }
  init_alpha <- mean(data[, 1])
  ls_line <- 0
  alpha <- 1
  beta <- 0
  # Create buttons for movie
  lin_reg_panel <- rpanel::rp.control("parameters", y_data = data[, 1],
                                      x_data = data[, 2], alpha = 1, beta = 0,
                                      y_name = y_name, x_name = x_name,
                                      user_args = user_args)
  rpanel::rp.doublebutton(lin_reg_panel, alpha, delta_alpha,
                          range = c(-5, 5), repeatinterval = 20,
                          initval = init_alpha,
                          title = "intercept, alpha:", action = lin_reg_plot)
  rpanel::rp.doublebutton(lin_reg_panel, beta, delta_beta,
                          range = c(-1, 1), repeatinterval = 20, initval = 0,
                          title = "gradient, beta:", action = lin_reg_plot)
  rpanel::rp.doublebutton(lin_reg_panel, ls_line, 1, range = c(0, 1),
                          repeatinterval = 20, initval = 0,
                          title = "Add least squares (LS) regression line?",
                          action = lin_reg_plot)
  rpanel::rp.radiogroup(lin_reg_panel, reg_line_only, c("no", "yes"),
                        action = lin_reg_plot,
                        title = "LS line only?")
  rpanel::rp.do(lin_reg_panel, lin_reg_plot)
  return(invisible())
}

# Function to be called by lin_reg_movie().

lin_reg_plot <- function(panel){
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    z <- stats::lm(y_data ~ x_data)
    alpha_hat <- z$coefficients[1]
    beta_hat <- z$coefficients[2]
    graphics::par(font.main = 1)
    if (!is.null(user_args$pch)) {
      leg_pch <- user_args$pch
    } else {
      leg_pch <- 16
    }
    do.call(graphics::plot, c(list(x_data, y_data), user_args))
    x <- seq(min(x_data), max(x_data), len = 100)
    #
    if (reg_line_only == "yes") {
      y <- alpha_hat + beta_hat * x
      current_line <- alpha_hat + beta_hat * x_data
      graphics::lines(x, y, lwd = 2, lty = 1)
    } else {
      y <- alpha + beta * x
      current_line <- alpha + beta * x_data
      graphics::lines(x, y, lwd = 2, lty = 2)
    }
    #
    current_rss <- round(sum((y_data - current_line) ^ 2), 3)
    rss <- round(sum(z$residuals ^ 2), 3)
    if (ls_line == 1){
      graphics::abline(a = z$coeff[1], b = z$coeff[2], lty = 1, lwd = 2)
    }
    for (i in 1:length(x_data)){
      xx <- c(x_data[i], x_data[i])
      yy <- c(y_data[i], current_line[i])
      graphics::lines(xx, yy, lwd = 2, col = "red")
    }
    if (beta_hat > 0) {
      legend_position <- "topleft"
      legend_position_2 <- "bottomright"
    } else {
      legend_position <- "bottomleft"
      legend_position_2 <- "topright"
    }
    if (ls_line == 1 || reg_line_only == "yes"){
      graphics::legend(legend_position,
                       legend = c("observations","current line","residuals",
                                  "LS line"),
                       pch = c(leg_pch, -1, -1, -1), lty = c(-1, 2, 1, 1),
                       lwd = c(-1, 2, 2, 2),
                       col = c(1, 1, "red", 1))
      graphics::legend(legend_position_2,
                       legend = c(paste("  SS = ", current_rss),
                                  paste("RSS = ", rss)),
                       pch = c(-1, -1), xjust = 0)
    } else {
      graphics::legend(legend_position,
                       legend = c("observations","current line","residuals",
                                  ""),
                       pch = c(leg_pch, -1, -1, -1), lty = c(-1, 2, 1, 1),
                       lwd = c(-1, 2, 2, 2),
                       col = c(1, 1, "red", 0))
      graphics::legend(legend_position_2,
                       legend = c(paste("  SS = ", current_rss),
                                  paste("RSS = ", "  ")),
                       pch = c(-1, -1), xjust = 0)
    }
    if (reg_line_only == "yes") {
      alpha_val <- alpha_hat
      beta_val <- beta_hat
    } else {
      alpha_val <- alpha
      beta_val <- beta
    }
    if (beta < 0) {
      graphics::title(main = paste("E(", y_name, ") = ", round(alpha_val, 3),
                                   " - ", abs(round(beta_val, 6)), " ", x_name,
                                   sep = ""))
    } else {
      graphics::title(main = paste("E(", y_name, ") = ", round(alpha_val, 3),
                                   " + ", round(beta_val, 6), " ", x_name,
                                   sep = ""))
    }
    graphics::par(old_par)
  })
  return(invisible(panel))
}

