#' @rdname geom_distribution
#' @format NULL
#' @usage NULL
#' @export
StatDistribution <- ggproto("StatDistribution", Stat,
   required_aes = character(0),

   compute_group = function (data, scales, fun, xlim = NULL, n = 101,
                             p_limit = 0.001, location = 0, args = list())
   {
     if (is.null(scales$x)) {
       range <- if (!is.null(xlim)) xlim else c(-10000, 10000)
       xseq <- seq(range[1], range[2], length.out = 1e6)
       dens <- do.call(fun, c(list(xseq), args))
       xseq <- xseq[dens >= p_limit]
       xseq <- seq(xseq[1], xseq[length(xseq)], length.out = n)
       x_trans <- xseq
     }
     else {
       range <- if (!is.null(xlim)) xlim else scales$x$dimension()
       xseq <- seq(range[1], range[2], length.out = n)
       if (scales$x$is_discrete()) {
         x_trans <- xseq
       }
       else {
         x_trans <- scales$x$trans$inverse(xseq)
       }
     }
     y_out <- do.call(fun, c(list(quote(x_trans)), args))
     if (!is.null(scales$y) && !scales$y$is_discrete()) {
       y_out <- scales$y$trans$transform(y_out)
     }
     data.frame(x = xseq + location, y = y_out, ymax = y_out, ymin = 0)
   }
)

#' @param fun Function to use. A quoted or character name referencing a probability
#' density function; see examples. Must be vectorised.
#' @param n Number of points to interpolate along the main body of the
#' probability density.
#' @param args List of additional arguments passed on to the function defined by `fun`.
#' @param xlim Optionally, restrict the range of the function to this range.
#' @param p_limit The lower probability density limit to plot.
#' @param location Where to shift the origin. For probability density functions
#' that do not inherently allow for changing their location, this allows that
#' shift.
#' @param fill Color of fill under the curve. No fill (`NA`) by default.
#' @param alpha Fill color transparency, if applicable.
#' @param color Color of the line.
#' @param size Size of the line.
#' @section Computed variables:
#' `stat_distribution()` computes the following variables:
#' \describe{
#'   \item{x}{x values along a grid where the probability density is at least `p_limit`}
#'   \item{y}{value of the probability density function evaluated at corresponding x}
#' }
#' @export
#' @rdname geom_distribution
stat_distribution <- function (mapping = NULL, data = NULL, geom = "ribbon",
          position = "identity", ..., fun, xlim = NULL, n = 500, p_limit = 0.001,
          location = 0, fill = NA, alpha = 0.3, outline.type = "upper",
          color = "black", size = 0.5, args = list(), na.rm = FALSE,
          show.legend = NA, inherit.aes = TRUE)
{
  if (is.null(data)) {
    data <- ensure_nonempty_data
  }
  layer(data = data, mapping = mapping, stat = StatDistribution,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(fun = fun, n = n, args = args, na.rm = na.rm,
                      xlim = xlim, p_limit = p_limit, location = location,
                      fill = fill, alpha = alpha, outline.type = outline.type,
                      color = color, size = size, ...))
}


# This is directly from the ggplot2 stat_function.R file
# with some changes to avoid other unexported functions
# Included here to avoid using `:::`
#
# Convenience function used by `stat_function()` and
# `geom_function()` to convert empty input data into
# non-empty input data without touching any non-empty
# input data that may have been provided.
ensure_nonempty_data <- function(data) {
  if (is.null(data) || class(data) == "waiver") {
    data.frame(list(group = 1), n = 1)
  } else {
    data
  }
}
