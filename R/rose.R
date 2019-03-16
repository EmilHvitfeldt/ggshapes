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
#' - **n**
#' - **d**
#' - **c**
#' - x0
#' - y0
#' - xscale
#' - yscale
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
#' @name geom_rose
#' @rdname geom_rose
#'
#' @examples
#'
#' ggplot() +
#'   geom_rose(aes(n = 2, d = 1, c = 0))
#'
#' ggplot() +
#'   geom_rose(aes(n = 2, d = 8, c = 0))
#'
#' ggplot() +
#'   geom_rose(aes(n = 5, d = 4, c = 4))
#'
#' # Rescaling
#' ggplot() +
#'   geom_rose(aes(n = 5, d = 4, c = 4, xscale = 6, yscale = 2))
#'
#' # Rotation
#' ggplot() +
#'   geom_rose(aes(n = 2, d = 1, c = 0, rotation = pi / 4))
#'
#' # Multiple roses
#' ggplot() +
#'   geom_rose(aes(n = 5:1, d = 1:5, c = 0, x0 = 1:5 * 3))
#'
#' ggplot() +
#'   geom_rose(aes(n = 5, d = 4, c = 4), fill = "white")
NULL

#' @rdname ggshapes-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat aes
#' @export
StatRose <- ggproto('StatRose', Stat,
                        compute_layer = function(self, data, params, layout) {
                          if (is.null(data)) return(data)
                          if (is.null(data$x0)) data$x0 <- 0
                          if (is.null(data$y0)) data$y0 <- 0
                          if (is.null(data$xscale)) data$xscale <- 1
                          if (is.null(data$yscale)) data$yscale <- 1
                          if (is.null(data$rotation)) data$rotation <- 0
                          data$group <- seq_len(nrow(data))

                          data <- tidyr::nest(data, n, d, c, x0, y0, xscale, yscale, rotation)
                          data$data <- lapply(data$data, rose_calc, params = params)
                          tidyr::unnest(data)
                        },
                        required_aes = c('n', 'd', 'c'),
                        default_aes = aes(x0 = 0, y0 = 0, xscale = 1, yscale = 1, rotation = 0),
                        extra_params = c('n_points', 'na.rm')
)

rose_calc <- function(data, params) {
  k <- data$n / data$d
  theta <- seq(from = 0, to = 2 * pi * data$d, length.out = params$n_points)

  out <- data.frame(
    x = data$x0 + data$xscale * (cos(k * theta) + data$c) * cos(theta) / (1 + data$c),
    y = data$y0 + data$yscale * (cos(k * theta) + data$c) * sin(theta) / (1 + data$c)
    )

  rotate_df(out, data$rotation)
}


#' @rdname geom_rose
#' @importFrom ggplot2 layer
#' @export
stat_rose <- function(mapping = NULL, data = NULL, geom = "rose",
                          position = "identity", n_points = 360, na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatRose, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_points = n_points, ...)
  )
}

#' @rdname geom_rose
#' @importFrom ggplot2 layer
#' @export
geom_rose <- function(mapping = NULL, data = NULL, stat = "rose",
                          position = "identity", n_points = 360, na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomShape,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n_points = n_points, na.rm = na.rm, ...))
}
