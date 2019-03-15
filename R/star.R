#' Draw a n pointed regular star
#'
#' This geom allows you to draw the star curve.
#'
#' @section Aesthetics:
#' geom_arc understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **r_min**
#' - **r_max**
#' - **n_tips**
#' - x0
#' - y0
#' - xscale
#' - yscale
#' - offset
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
#'  \item{x, y}{The coordinates for the points along the star curve}
#' }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#'
#' @param n_points The number of points to sample along the curve.
#'
#' @author Emil Hvitfeldt
#'
#' @name geom_star
#' @rdname geom_star
#'
#' @examples
#' # Changing number of tips
#' ggplot() +
#'   geom_star(aes(r_min = 0.5, r_max = 1, n_tips = 5))
#'
#' ggplot() +
#'   geom_star(aes(r_min = 0.5, r_max = 1, n_tips = 11))
#'
#' # changing radei
#' ggplot() +
#'   geom_star(aes(r_min = 0.2, r_max = 1, n_tips = 5))
#'
#' ggplot() +
#'   geom_star(aes(r_min = 0.7, r_max = 1, n_tips = 5))
#'
#' # rescaling
#' ggplot() +
#'   geom_star(aes(r_min = 0.5, r_max = 1, n_tips = 5, xscale = 4, yscale = 2))
#'
#' # Playing witn offset parameter
#' ggplot() +
#'   geom_star(aes(r_min = 0.5, r_max = 1, n_tips = 5, offset = 0))
#'
#' ggplot() +
#'   geom_star(aes(r_min = 0.5, r_max = 1, n_tips = 5, offset = 1))
#'
#' ggplot() +
#'   geom_star(aes(r_min = 0.5, r_max = 1, n_tips = 11, offset = 5))
#'
#' # Multiple stars
#' ggplot() +
#'   geom_star(aes(r_min = 0.5, r_max = 1, n_tips = c(3, 5, 7),
#'   x0 = c(1, 4, 7), y0 = c(1, 4, 7)))
#'
#' # Regular polygons comes up as a special example when
#' # r_max = r_min / cos(pi / n_tips)
#' ggplot() +
#'   geom_star(aes(r_min = 0.5, r_max = 0.5 / cos(pi / 5), n_tips = 5))
NULL

#' @rdname ggshapes-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat aes
#' @export
StatStar <- ggproto('StatStar', Stat,
                         compute_layer = function(self, data, params, layout) {
                           if (is.null(data)) return(data)
                           if (is.null(data$x0)) data$x0 <- 0
                           if (is.null(data$y0)) data$y0 <- 0
                           if (is.null(data$xscale)) data$xscale <- 1
                           if (is.null(data$yscale)) data$yscale <- 1
                           if (is.null(data$offset)) data$offset <- 0.5
                           data$group <- seq_len(nrow(data))

                           data <- tidyr::nest(data, r_min, r_max, n_tips, offset, x0, y0, xscale, yscale)
                           data$data <- lapply(data$data, star_calc, params = params)
                           tidyr::unnest(data)
                         },
                         required_aes = c('r_min', 'r_max', 'n_tips'),
                         default_aes = aes(x0 = 0, y0 = 0, offset = 0.5, xscale = 1, yscale = 1),
                         extra_params = c('n_points', 'na.rm')
)

star_calc <- function(data, params) {
  theta <- seq(0, 2 * pi, length.out = data$n_tips + 1) + pi / 2
  theta_s <-seq(0, 2 * pi, length.out = data$n_tips + 1)[-1]  + pi / 2 -
                        pi / data$n_tips * 2 * data$offset

  data.frame(
    x = data$x0 + data$xscale * weave(data$r_max * cos(theta), data$r_min * cos(theta_s)),
    y = data$y0 + data$yscale * weave(data$r_max * sin(theta), data$r_min * sin(theta_s))
  )

}

#' @rdname geom_star
#' @importFrom ggplot2 layer
#' @export
stat_star <- function(mapping = NULL, data = NULL, geom = "star",
                           position = "identity", n_points = 360, na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    stat = StatStar, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_points = n_points, ...)
  )
}

#' @rdname geom_star
#' @importFrom ggplot2 layer
#' @export
geom_star <- function(mapping = NULL, data = NULL, stat = "star",
                           position = "identity", n_points = 360, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomShape,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n_points = n_points, na.rm = na.rm, ...))
}
