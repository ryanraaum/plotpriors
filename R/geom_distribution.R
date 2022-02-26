GeomDistribution <- ggproto("GeomDistribution", GeomRibbon,
                            default_aes = aes(color = "black", fill = NA, alpha = 0.3, size = 0.5, linetype = 1)
)

geom_distribution <- function (mapping = NULL, data = NULL, stat = "distribution", position = "identity",
                               ..., fill=NA, outline.type = "upper", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  if (is.null(data)) {
    data <- ggplot2:::ensure_nonempty_data
  }
  layer(data = data, mapping = mapping, stat = stat, geom = GeomDistribution,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, fill = fill, outline.type = "upper", ...))
}
