# ================================ box_plot ===============================

#' Box plots
#'
#' Produce box-and-whisker plot(s) of the given (grouped) values, with an
#' option (using the argument \code{type}) to change the estimator used
#' to estimate the quartiles.  The only argument described below is
#' \code{type}.  For details of the other arguments see the
#' \strong{Arguments} section of \code{\link{boxplot}}.
#'
#' @section \strong{Usage}:
#'   See the \strong{Usage} section of the \code{\link{boxplot}}
#'   documentation.
#'
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#'   algorithms detailed in the \strong{Details} section of the
#'   \code{\link{quantile}} documentation.
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
