#' Draw a n pointed regular star
#'
#' This geom allows you to draw the star curve.
#'
#' @section Aesthetics:
#' geom_arc understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - **r_min**
#' - **r_max**
#' - **n_tips**
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
#'   geom_star(aes(r_min = 0.8, r_max = 1, n_tips = 5))
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
                           if (is.null(data$offset)) data$offset <- 0.5

                           data$group <- seq_len(nrow(data))

                           theta <- lapply(data$group,
                                           function(x) seq(0, 2 * pi, length.out = data$n_tips[x] + 1) + pi / 2)

                           theta_s <- lapply(data$group,
                                  function(x) {
                                    seq(0, 2 * pi, length.out = data$n_tips[x] + 1)[-1]  + pi / 2 -
                                      pi / data$n_tips[x] * 2 * data$offset[x]
                                  })

                           x_max <- lapply(data$group,
                                           function(x) data$r_max[x] * cos(theta[[x]]))
                           y_max <- lapply(data$group,
                                           function(x) data$r_max[x] * sin(theta[[x]]))

                           x_min <- lapply(data$group,
                                           function(x) data$r_min[x] * cos(theta_s[[x]]))
                           y_min <- lapply(data$group,
                                           function(x) data$r_min[x] * sin(theta_s[[x]]))

                           x_val <- lapply(data$group,
                                           function(x) weave(x_max[[x]], x_min[[x]]))
                           y_val <- lapply(data$group,
                                           function(x) weave(y_max[[x]], y_min[[x]]))

                           data <- data[rep(data$group, times = data$n_tips * 2 + 1), ]

                           data$x <- unlist(x_val) + data$x0
                           data$y <- unlist(y_val) + data$y0
                           data
                         },
                         required_aes = c('r_min', 'r_max', 'n_tips'),
                         default_aes = aes(x0 = 0, y0 = 0, offset = 0.5),
                         extra_params = c('n_points', 'na.rm')
)
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
