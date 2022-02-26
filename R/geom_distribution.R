#' @rdname geom_distribution
#' @format NULL
#' @usage NULL
#' @export
GeomDistribution <- ggproto("GeomDistribution", GeomRibbon,
                            default_aes = aes(color = "black", fill = NA,
                                              alpha = 0.3, size = 0.5,
                                              linetype = 1)
)

#' Draw a probability density function as a continuous curve
#'
#' Computes and draws a probability density function as a continuous curve.
#'
#' @eval ggplot2:::rd_aesthetics("geom", "distribution")
#' @param data Ignored by `stat_distribution()`, do not use.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_ribbon
#' @examples
#'
#' # geom_distribution() is useful for overlaying probability distributions
#' set.seed(1234)
#' ggplot(data.frame(x = rnorm(100)), aes(x)) +
#'   geom_density() +
#'   geom_distribution(fun = dnorm, color = "red")
#'
#' @export
geom_distribution <- function (mapping = NULL, data = NULL, stat = "distribution",
                               position = "identity", ..., fill=NA,
                               outline.type = "upper", na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE)
{
  if (is.null(data)) {
    data <- ensure_nonempty_data
  }
  layer(data = data, mapping = mapping, stat = stat, geom = GeomDistribution,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, fill = fill, outline.type = "upper", ...))
}
