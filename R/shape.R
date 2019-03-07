#' @rdname ggshapes-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomShape <- ggproto('GeomShape', GeomPolygon,
                      default_aes = list(
                        colour = 'black',
                        fill = NA,
                        size = 0.5,
                        linetype = 1,
                        alpha = NA
                      )
)
