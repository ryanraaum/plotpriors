#' Quickly plot probability density function
#'
#' @param fun A probability density function (e.g. \code{dnorm}).
#' @param x (Optional) A sequence of values for the probability density
#' function. If not supplied, the function will be plotted for all points
#' with probability density greater than \code{p_limit} (default 0.001)
#' between the \code{lower_bound} (default -10000) and the \code{upper_bound}
#' (default 10000).
#' @param color The color of the plotted line (default "skyblue").
#' @param linetype The linetype (default "solid").
#' @param size The size of the line (default 1.5).
#' @param fill The color of the fill under the line (default NA = no fill)
#' @param alpha The alpha transparency for the fill (default 0.3).
#' @param location For probability density functions that do not have a
#' location parameter, this will shift the center (zero) of that distribution
#' to the specified location.
#' @param p_limit The probability density cutoff for plotting.
#' @param lower_bound The (potential) lower range of plotting subject to the
#' \code{p_limit} cutoff (default -10000).
#' @param upper_bound The (potential) upper range of plotting subject to the
#' \code{p_limit} cutoff (default 10000).
#' @param ... Any additional parameters will be passed to the probability
#' density function
#' @return A ggplot \code{gg} object.
#' @examples
#' plot_pdensity(dnorm)
#' plot_pdensity(dnorm, seq(-3, 3, length.out = 100))
#' plot_pdensity(dbeta, shape1 = 2, shape2 = 2)
#' @export
plot_pdensity <- function(fun, x=NULL, color = "skyblue", linetype = "solid",
                          size = 1.5, fill = NA, alpha = 0.3, location = 0,
                          p_limit = 0.001, lower_bound = -1e4, upper_bound = 1e4,
                          ...) {
  p <- ggplot() +
    stat_distribution(fun = fun,
                      args = list(...),
                      color = color,
                      size = size,
                      fill = fill,
                      alpha = alpha,
                      outline.type = "upper") +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.line.y = element_blank()
    )
  p
}
