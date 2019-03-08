#' Weaves two vectors together using alternating indecies
#'
#' @param a Vector
#' @param b Vector

#' @return A vector
weave <- function(a, b) {
  x <- vector(class(a), length(c(a, b)))
  x[c(TRUE, FALSE)] <- a
  x[c(FALSE, TRUE)] <- b
  x
}
