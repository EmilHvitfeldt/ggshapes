#' Draw a rose or hypocycloid curve
#'
#' This geom allows you to draw the hypocycloid curve.
#'
#' The curve follows the the parameterized form
#'
#' \deqn{x = sin(a\theta + \delta)}
#' \deqn{y = sin(b\theta)}
#'
#' these curves are closed when the radion \eqn{a / b} is rational. delta have
#' been scaled to be in the interval [0, 1].
#'
#' @references \url{https://en.wikipedia.org/wiki/Hypocycloid}
#' @section Aesthetics:
#' geom_arc understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - **a**
#' - **b**
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
#' @name geom_hypocycloid
#' @rdname geom_hypocycloid
#'
#' @examples
#' ggplot() +
#'   geom_hypocycloid(aes(a = 4, b = 1))
#'
#' ggplot() +
#'   geom_hypocycloid(aes(a = 8, b = 1))
#'
#'  ggplot() +
#'   geom_hypocycloid(aes(a = 2, b = 3, x0 = c(1, 4, 7)))
NULL

#' @rdname ggshapes-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat aes
#' @export
StatHypocycloid <- ggproto('StatHypocycloid', Stat,
                         compute_layer = function(self, data, params, layout) {
                           if (is.null(data)) return(data)
                           data$group <- seq_len(nrow(data))
                           n_roses <- nrow(data)
                           data <- data[rep(seq_len(n_roses), each = params$n_points), ]
                           if (is.null(data$x0)) data$x0 <- 0
                           if (is.null(data$y0)) data$y0 <- 0

                           t <- seq(from = 0, to = 2 * pi * max(data$a/data$b), length.out = params$n_points)

                           data$x <- data$x0 + (data$a - data$b) * cos(t) + data$b * cos((data$a - data$b) / data$b * t)
                           data$y <- data$y0 + (data$a - data$b) * sin(t) - data$b * sin((data$a - data$b) / data$b * t)
                           data
                         },
                         required_aes = c('a', 'b'),
                         default_aes = aes(x0 = 0, y0 = 0),
                         extra_params = c('n_points', 'na.rm')
)
#' @rdname geom_hypocycloid
#' @importFrom ggplot2 layer
#' @export
stat_hypocycloid <- function(mapping = NULL, data = NULL, geom = "hypocycloid",
                           position = "identity", n_points = 360, na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    stat = StatHypocycloid, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_points = n_points, ...)
  )
}

#' @rdname geom_hypocycloid
#' @importFrom ggplot2 layer
#' @export
geom_hypocycloid <- function(mapping = NULL, data = NULL, stat = "hypocycloid",
                           position = "identity", n_points = 360, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomShape,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n_points = n_points, na.rm = na.rm, ...))
}
