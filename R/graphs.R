# ================================ box_plot ===============================

#' Box plots
#'
#' Produce box-and-whisker plot(s) of the given (grouped) values, with an
#' option (using the argument \code{type}) to change the estimator used
#' to estimate the quartiles.  The only argument described below is
#' \code{type}.  For details of the other arguments see the
#' \strong{Arguments} section of \code{\link[graphics]{boxplot}}.
#'
#' @section \strong{Usage}:
#'   See the \strong{Usage} section of the \code{\link[graphics]{boxplot}}
#'   documentation.
#'
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#'   algorithms detailed in the \strong{Details} section of the
#'   \code{\link[stats]{quantile}} documentation.
#' @details See the \strong{Details} section of the \code{\link{boxplot}}
#'   documentation.
#' @return A list with the same contents as described in the \strong{Value}
#'   section of the \code{\link{boxplot}} documentation.
#' @examples
#' birth_times <- ox_births[, "time"]
#' box_plot(birth_times)
#' box_plot(birth_times, horizontal = TRUE)
#' @name box_plot
NULL
## NULL

box_plot <- function(x, ...) {
  UseMethod("box_plot")
}

box_plot.default <- function (x, ..., range = 1.5, width = NULL,
                              varwidth = FALSE, notch = FALSE, outline = TRUE,
                              names, plot = TRUE, border = par("fg"),
                              col = NULL, log = "",
                              pars = list(boxwex = 0.8, staplewex = 0.5,
                                          outwex = 0.5), horizontal = FALSE,
                              add = FALSE, at = NULL, type = 6) {
  args <- list(x, ...)
  namedargs <- if (!is.null(attributes(args)$names))
    attributes(args)$names != ""
  else rep_len(FALSE, length(args))
  groups <- if (is.list(x))
    x
  else args[!namedargs]
  if (0L == (n <- length(groups)))
    stop("invalid first argument")
  if (length(class(groups)))
    groups <- unclass(groups)
  if (!missing(names))
    attr(groups, "names") <- names
  else {
    if (is.null(attr(groups, "names")))
      attr(groups, "names") <- 1L:n
    names <- attr(groups, "names")
  }
  cls <- sapply(groups, function(x) class(x)[1L])
  cl <- if (all(cls == cls[1L]))
    cls[1L]
  else NULL
  for (i in 1L:n) groups[i] <- list(boxplot_stats(unclass(groups[[i]]),
                                                  range, type = type))
  stats <- matrix(0, nrow = 5L, ncol = n)
  conf <- matrix(0, nrow = 2L, ncol = n)
  ng <- out <- group <- numeric(0L)
  ct <- 1
  for (i in groups) {
    stats[, ct] <- i$stats
    conf[, ct] <- i$conf
    ng <- c(ng, i$n)
    if ((lo <- length(i$out))) {
      out <- c(out, i$out)
      group <- c(group, rep.int(ct, lo))
    }
    ct <- ct + 1
  }
  if (length(cl) && cl != "numeric")
    oldClass(stats) <- cl
  z <- list(stats = stats, n = ng, conf = conf, out = out,
            group = group, names = names)
  if (plot) {
    if (is.null(pars$boxfill) && is.null(args$boxfill))
      pars$boxfill <- col
    do.call("bxp", c(list(z, notch = notch, width = width,
                          varwidth = varwidth, log = log, border = border,
                          pars = pars, outline = outline,
                          horizontal = horizontal, add = add, at = at),
                     args[namedargs]))
    invisible(z)
  }
  else z
}

box_plot.matrix <- function (x, use.cols = TRUE, ...) {
  groups <- if (use.cols) {
    split(c(x), rep.int(1L:ncol(x), rep.int(nrow(x), ncol(x))))
  }
  else split(c(x), seq(nrow(x)))
  if (length(nam <- dimnames(x)[[1 + use.cols]]))
    names(groups) <- nam
  invisible(box_plot(groups, ...))
}

box_plot.formula <- function (formula, data = NULL, ..., subset,
                              na.action = NULL) {
  if (missing(formula) || (length(formula) != 3L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m$na.action <- na.action
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  box_plot(split(mf[[response]], mf[-response]), ...)
}

boxplot_stats <- function (x, coef = 1.5, do.conf = TRUE, do.out = TRUE,
                           type = 6){
  if (coef < 0)
    stop("'coef' must not be negative")
  nna <- !is.na(x)
  n <- sum(nna)
  stats <- five_number(x, na.rm = TRUE, type = type)
  iqr <- diff(stats[c(2, 4)])
  if (coef == 0)
    do.out <- FALSE
  else {
    out <- if (!is.na(iqr)) {
      x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef * iqr)
    }
    else !is.finite(x)
    if (any(out[nna], na.rm = TRUE))
      stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
  }
  conf <- if (do.conf)
    stats[3L] + c(-1.58, 1.58) * iqr / sqrt(n)
  list(stats = stats, n = n, conf = conf,
       out = if (do.out) x[out & nna] else numeric())
}

# ================================ scatter ===============================

#' Scatter plot with five number summary
#'
#' Produces a scatter plot in which the axes are labelled with
#' the respective five number summaries of the horizontal and vertical
#' axis data.
#'
#' @param x The argument \code{x} of \code{\link[graphics]{plot}}.
#' @param y The argument \code{x} of \code{\link[graphics]{plot}}.
#' @param ndec A numeric vector.  The numbers of decimal places to
#'   which to round the five number summaries.  If \code{ndec} is
#'   a scalar then this value is used for both axes.
#' @param type Argument \code{type} used in the call to
#'   \code{\link{five_number}} to estimate the 25\%, 50\% and 75\% quantiles.
#' @param na.rm A logical scalar.  If true, any \code{\link{NA}} and NaN's
#'   are removed before the sample quantiles are computed.
#' @param ... Further arguments to be passed to \code{plot}.
scatter <- function(x, y, ndec = 1, type = 6, na.rm = FALSE, ...) {
  # Make sure that ndec has length 2
  ndec <- rep_len(ndec, 2)
  # Calculate five number summaries of x and y.
  fx <- five_number(x, type = type, na.rm = na.rm)
  fy <- five_number(y, type = type, na.rm = na.rm)
  # Round the five number summaries.
  lx <- round(fx, ndec)
  ly <- round(fy, ndec)
  # Produce the plot, but with no axes.
  plot(x, y, axes = FALSE, ...)
  # Add axes with only the five number summaries labelled.
  axis(1, at = fx, labels = lx)
  axis(2, at = fy, labels = ly)
}


# ================================ scatter ===============================

#' Scatter plot with marginal histograms
#'
#' Produces a scatter plot in which the axes are supplemented by
#' histograms of the marginal horizontal and vertical axis data.
#'
#' @param x The argument \code{x} of \code{\link[graphics]{plot}}.
#' @param y The argument \code{x} of \code{\link[graphics]{plot}}.
#' @param xbreaks A numeric vector.  Optional argument \code{breaks}
#'   to \code{\link[graphics]{hist}} when plotting the histogram
#'   on the horizontal axis.
#' @param ybreaks A numeric vector.  Optional argument \code{breaks}
#'   to \code{\link[graphics]{hist}} when plotting the histogram
#'   on the vertical axis.
#' @param ... Further arguments to be passed to \code{plot}.

scatter_hist <- function(x, y, xbreaks = NULL, ybreaks = NULL, ...) {
  # save default, for resetting...
  def.par <- par(no.readonly = TRUE)
  # Extract any arguments supplied in ....
  user_args <- list(...)
  # If the user wants a log-scale on an axis make a transformation of
  # the data so that the histogram matches the scatterplot.
  if (!is.null(user_args$log)) {
    if (user_args$log == "x") {
      xh <- log(x)
      yh <- y
    }
    if (user_args$log == "y") {
      yh <- log(y)
      xh <- x
    }
    if (user_args$log == "xy") {
      xh <- log(x)
      yh <- log(y)
    }
  } else {
    xh <- x
    yh <- y
  }
  # create a layout to contain the main scatter plot and the histograms.
  nf <- layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE), c(3, 1), c(1, 3),
               TRUE)
  par(mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 0, 0))
  # Produce the scatter plot.
  plot(x, y, ...)
  par(mar = c(0, 3, 1, 1))
  # Produce the histogram for the horizontal axis.
  if (is.null(xbreaks)) {
    xhist <- hist(xh, plot = FALSE)
  } else {
    xhist <- hist(xh, breaks = xbreaks, plot = FALSE)
  }
  barplot(xhist$count, axes = FALSE, space = 0)
  par(mar = c(3, 0, 1, 1))
  # Produce the histogram for the vertical axis.
  if (is.null(ybreaks)) {
    yhist <- hist(yh, plot = FALSE)
  } else {
    yhist <- hist(yh, breaks = ybreaks, plot = FALSE)
  }
  barplot(yhist$count, axes = FALSE, space = 0, horiz = TRUE)
  #- reset to default
  par(def.par)
  invisible()
}
