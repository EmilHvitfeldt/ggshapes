#' Draw a rose or Epitrochoid curve
#'
#' This geom allows you to draw the epitrochoid curve. A epitrochoid is a
#' curve traced by a point attached to a circle of radius r_min rolling around
#' the outside of a fixed circle of radius r_max, where the point is at a
#' distance h from the center of the interior circle.
#'
#' To unscale the curve, please set xscale and yscale to r_max + r_min + h.
#'
#' The curve follows the the parameterized form
#'
#' \deqn{x = (r_max + r_min) cos(\theta) - h * cos(\frac{r_max + r_min}{r_min} \theta)}
#' \deqn{x = (r_max + r_min) sin(\theta) - h * sin(\frac{r_max + r_min}{r_min} \theta)}
#'
#' these curves are closed when the radion \eqn{a / b} is rational. delta have
#' been scaled to be in the interval [0, 1].
#'
#' @references \url{http://mathworld.wolfram.com/Epitrochoid.html}
#'     \url{http://xahlee.info/SpecialPlaneCurves_dir/Epitrochoid_dir/epitrochoid.html}
#' @section Aesthetics:
#' geom_arc understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **r_max**
#' - **r_min**
#' - x0
#' - y0
#' - xscale
#' - yscale
#' - h
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
#' @name geom_epitrochoid
#' @rdname geom_epitrochoid
#' @seealso [geom_hypotrochoid()]
#'
#' @examples
#'
#' # When h is missing a hypocycloid is generated
#' ggplot() +
#'   geom_epitrochoid(aes(r_max = 4, r_min = 1))
#'
#' ggplot() +
#'   geom_epitrochoid(aes(r_max = 8, r_min = 1))
#'
#' ggplot() +
#'   geom_epitrochoid(aes(r_max = c(4, 6, 8), r_min = 1))
#'
#' # specifying h
#' ggplot() +
#'   geom_epitrochoid(aes(r_max = 4, r_min = 1, h = 4))
#'
#' ggplot() +
#'   geom_epitrochoid(aes(r_max = 4, r_min = 3, h = 20 / 13))
#'
#' # Changing the horizontal and vertical scale
#'
#' ggplot() +
#'   geom_epitrochoid(aes(r_max = 4, r_min = 3, xscale = 10, yscale = 3))
#'
#' # When things go wild
#' ggplot(expand.grid(seq(4, 20, by = 2), c(1, 3, 5, 9))) +
#'   geom_epitrochoid(aes(r_max = Var1, r_min = Var2, color = Var1,
#'                        xscale = Var1 + Var2, yscale = Var1 + Var2)) +
#'   coord_fixed() +
#'   theme_minimal() +
#'   scale_color_viridis_c(option = "B") +
#'   guides(color = "none") +
#'   facet_wrap(~ Var2)
NULL

#' @rdname ggshapes-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat aes
#' @export
StatEpitrochoid <- ggproto('StatEpitrochoid', Stat,
                            compute_layer = function(self, data, params, layout) {
                              if (is.null(data)) return(data)
                              if (is.null(data$x0)) data$x0 <- 0
                              if (is.null(data$y0)) data$y0 <- 0
                              if (is.null(data$xscale)) data$xscale <- 1
                              if (is.null(data$yscale)) data$yscale <- 1
                              if (is.null(data$h)) data$h <- data$r_min
                              data$group <- seq_len(nrow(data))

                              data <- tidyr::nest(data, r_min, r_max, h, x0, y0, xscale, yscale)
                              data$data <- lapply(data$data, epitrochoid_calc, params = params)
                              tidyr::unnest(data)
                            },
                            required_aes = c('r_min', 'r_max'),
                            default_aes = aes(x0 = 0, y0 = 0, h = NULL, xscale = 1, yscale = 1),
                            extra_params = c('n_points', 'na.rm')
)

epitrochoid_calc <- function(data, params) {
  t <- seq(from = 0, to = 2 * pi * data$r_min, length.out = params$n_points)

  data.frame(
    x = data$x0 + data$xscale * ((data$r_max + data$r_min) * cos(t) -
      data$h * cos((data$r_max + data$r_min) / data$r_min * t)) / (data$r_max + data$r_min + data$h),
    y = data$y0 + data$yscale * ((data$r_max + data$r_min) * sin(t) -
      data$h * sin((data$r_max + data$r_min) / data$r_min * t)) / (data$r_max + data$r_min + data$h)
  )
}

#' @rdname geom_epitrochoid
#' @importFrom ggplot2 layer
#' @export
stat_epitrochoid <- function(mapping = NULL, data = NULL, geom = "epitrochoid",
                              position = "identity", n_points = 360, na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  layer(
    stat = StatEpitrochoid, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_points = n_points, ...)
  )
}

#' @rdname geom_epitrochoid
#' @importFrom ggplot2 layer
#' @export
geom_epitrochoid <- function(mapping = NULL, data = NULL, stat = "epitrochoid",
                              position = "identity", n_points = 360, na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomShape,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n_points = n_points, na.rm = na.rm, ...))
}
