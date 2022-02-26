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

stat_distribution <- function (mapping = NULL, data = NULL, geom = "ribbon",
          position = "identity", ..., fun, xlim = NULL, n = 500, p_limit = 0.001,
          location = 0, fill = NA, alpha = 0.3, outline.type = "upper",
          color = "black", size = 0.5, args = list(), na.rm = FALSE,
          show.legend = NA, inherit.aes = TRUE)
{
  if (is.null(data)) {
    data <- ggplot2:::ensure_nonempty_data
  }
  layer(data = data, mapping = mapping, stat = StatDistribution,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(fun = fun, n = n, args = args, na.rm = na.rm,
                      xlim = xlim, p_limit = p_limit, location = location,
                      fill = fill, alpha = alpha, outline.type = outline.type,
                      color = color, size = size, ...))
}

