#' Draw a rose or hypocycloid curve
#'
#' This geom allows you to draw the hypocycloid curve.
#'
#' The curve follows the the parameterized form
#'
#' \deqn{x = (r_max - r_min) cos(\theta) + r_min * cos(\frac{r_max - r_min}{r_min} \theta)}
#' \deqn{x = (r_max - r_min) sin(\theta) + r_min * sin(\frac{r_max - r_min}{r_min} \theta)}
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
#' - **r_max**
#' - **r_min**
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
#'   geom_hypocycloid(aes(r_max = 4, r_min = 1))
#'
#' ggplot() +
#'   geom_hypocycloid(aes(r_max = 8, r_min = 1))
#'
#'  ggplot() +
#'   geom_hypocycloid(aes(r_max = c(4, 6, 8), r_min = 1))
#'
#'  ggplot() +
#'   geom_hypocycloid(aes(r_max = c(4, 6, 8), r_min = 1))
NULL

#' @rdname ggshapes-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat aes
#' @export
StatHypocycloid <- ggproto('StatHypocycloid', Stat,
                         compute_layer = function(self, data, params, layout) {
                           if (is.null(data)) return(data)
                           if (is.null(data$x0)) data$x0 <- 0
                           if (is.null(data$y0)) data$y0 <- 0
                           data$group <- seq_len(nrow(data))

                           data <- tidyr::nest(data, r_min, r_max, x0, y0)
                           data$data <- lapply(data$data, hypocycloid_calc, params = params)
                           tidyr::unnest(data)
                         },
                         required_aes = c('r_min', 'r_max'),
                         default_aes = aes(x0 = 0, y0 = 0),
                         extra_params = c('n_points', 'na.rm')
)

hypocycloid_calc <- function(data, params) {
  t <- seq(from = 0, to = 2 * pi * data$r_min, length.out = params$n_points)

  data.frame(
    x = data$x0 + (data$r_max - data$r_min) * cos(t) +
      data$r_min * cos((data$r_max - data$r_min) / data$r_min * t),
    y = data$y0 + (data$r_max - data$r_min) * sin(t) -
      data$r_min * sin((data$r_max - data$r_min) / data$r_min * t)
  )
}

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
