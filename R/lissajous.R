#' Draw a rose or lissajous curve
#'
#' This geom allows you to draw the lissajous curve.
#'
#' The curve follows the the parameterized form
#'
#' \deqn{x = sin(a\theta + \delta)}
#' \deqn{y = sin(b\theta)}
#'
#' these curves are closed when the radion \eqn{a / b} is rational. delta have
#' been scaled to be in the interval [0, 1].
#'
#' @references \url{https://en.wikipedia.org/wiki/Lissajous_curve}
#' @section Aesthetics:
#' geom_arc understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - **a**
#' - **b**
#' - **delta**
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
#' @name geom_lissajous
#' @rdname geom_lissajous
#'
#' @examples
#' ggplot() +
#'   geom_lissajous(aes(a = 5, b = 4, delta = 2))
#'
#'  ggplot() +
#'   geom_lissajous(aes(a = 2, b = 3, delta = 1))
#'
#'  ggplot() +
#'   geom_lissajous(aes(a = 1:3, b = 3, delta = 1, x0 = c(1, 4, 7)))
NULL

#' @rdname ggshapes-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat aes
#' @export
StatLissajous <- ggproto('StatLissajous', Stat,
                        compute_layer = function(self, data, params, layout) {
                          if (is.null(data)) return(data)
                          if (is.null(data$x0)) data$x0 <- 0
                          if (is.null(data$y0)) data$y0 <- 0
                          data$group <- seq_len(nrow(data))

                          data <- tidyr::nest(data, a, b, delta, x0, y0)
                          data$data <- lapply(data$data, lissajous_calc, params = params)
                          tidyr::unnest(data)
                        },
                        required_aes = c('a', 'b', 'delta'),
                        default_aes = aes(x0 = 0, y0 = 0),
                        extra_params = c('n_points', 'na.rm')
)

lissajous_calc <- function(data, params) {
  t <- seq(from = 0, to = 2 * pi, length.out = params$n_points)

  data.frame(
    x = data$x0 + sin(data$a * t + data$delta * pi),
    y = data$y0 + sin(data$b * t)
  )
}

#' @rdname geom_lissajous
#' @importFrom ggplot2 layer
#' @export
stat_lissajous <- function(mapping = NULL, data = NULL, geom = "lissajous",
                          position = "identity", n_points = 360, na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  layer(
    stat = StatLissajous, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_points = n_points, ...)
  )
}

#' @rdname geom_lissajous
#' @importFrom ggplot2 layer
#' @export
geom_lissajous <- function(mapping = NULL, data = NULL, stat = "lissajous",
                          position = "identity", n_points = 360, na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomShape,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n_points = n_points, na.rm = na.rm, ...))
}
