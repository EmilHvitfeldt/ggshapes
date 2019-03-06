#' Draw a rose or rhodonea curve
#'
#' This geom allows you to draw mathematical roses. A rose is a sinusoid
#' plotted in polar coordinates.
#'
#' the shape follows the following parameterized form
#'
#' \deqn{x = (cos(k\theta) + c)cos(\theta)}
#' \deqn{y = (cos(k\theta) + c)sin(\theta)}
#'
#' where
#'
#' \deqn{k = n / d}
#'
#' the rose will be a closed loop when k is rational. this can easily be
#' achived by keeping n and d whole numbers. c is the internal offset parameter
#' changing how much the flower spreads out.
#'
#' @references \url{https://en.wikipedia.org/wiki/Rose_(mathematics)}
#' @section Aesthetics:
#' geom_arc understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - **n**
#' - **d**
#' - **c**
#' - color
#' - fill
#' - size
#' - linetype
#' - alpha
#' - lineend
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The coordinates for the points along the rose curve}
#' }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#'
#' @param n_points The number of points to sample along the curve.
#'
#' @author Emil Hvitfeldt
#'
#' @name geom_rhodonea
#' @rdname geom_rhodonea
#'
#' @examples
#'
#' ggplot() +
#'   geom_rhodonea(aes(n = 2, d = 1, c = 0))
#'
#' ggplot() +
#'   geom_rhodonea(aes(n = 2, d = 8, c = 0))
#'
#' ggplot() +
#'   geom_rhodonea(aes(n = 5, d = 4, c = 4))
#'
#' ggplot() +
#'   geom_rhodonea(aes(n = 5, d = 4, c = 4), fill = "white")
NULL

#' @rdname ggshapes-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat aes
#' @export
StatRhodonea <- ggproto('StatRhodonea', Stat,
                        compute_layer = function(self, data, params, layout) {
                          if (is.null(data)) return(data)
                          data$group <- seq_len(nrow(data))
                          n_roses <- nrow(data)
                          data <- data[rep(seq_len(n_roses), each = params$n_points), ]
                          if (is.null(data$x0)) data$x0 <- 0
                          if (is.null(data$y0)) data$y0 <- 0

                          k <- data$n / data$d
                          theta <- seq(from = 0, to = 2 * pi * data$d[1], length.out = params$n_points)

                          data$x <- data$x0 + (cos(k * theta) + data$c) * cos(theta)
                          data$y <- data$y0 + (cos(k * theta) + data$c) * sin(theta)
                          data
                        },
                        required_aes = c('n', 'd', 'c'),
                        default_aes = aes(x0 = 0, y0 = 0),
                        extra_params = c('n_points', 'na.rm')
)
#' @rdname geom_rhodonea
#' @importFrom ggplot2 layer
#' @export
stat_rhodonea <- function(mapping = NULL, data = NULL, geom = "rhodonea",
                          position = "identity", n_points = 360, na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatRhodonea, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_points = n_points, ...)
  )
}

#' @rdname geom_rhodonea
#' @importFrom ggplot2 layer
#' @importFrom ggforce GeomShape GeomCircle
#' @export
geom_rhodonea <- function(mapping = NULL, data = NULL, stat = "rhodonea",
                          position = "identity", n_points = 360, na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomCircle,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n_points = n_points, na.rm = na.rm, ...))
}
