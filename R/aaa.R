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

#' Rotates a parametric data.frame
#'
#' @param df a data frame with two columns x and y
#' @param theta a numeric denoting the amount of rotation
#'
#' @return a data frame with two columns x and y
rotate_df <- function(df, theta) {
  if (theta %% (pi * 2) == 0) {
    return(df)
  }
  out <- df
  out$x <- df$x * cos(theta) - df$y * sin(theta)
  out$y <- df$x * sin(theta) + df$y * cos(theta)
  out
}
