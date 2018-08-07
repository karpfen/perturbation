#' Generate data
#'
#' This function can be used to generate randomised test data.
#' The data is uniformly distributed within a given radius.
#'
#' @param n Number of points.
#' @param x X position of center.
#' @param y Y position of center
#' @param radius Radius of the circle in which the data lies.
#'
#' @return A \code{data.frame} of x/y coordinates.
#'
#' @export
generate_data <- function (n = 100, x = 0.5, y = 0.5, radius = 0.1)
{
  if (radius == 0)
  {
    x <- rep (x, n)
    y <- rep (y, n)
  } else
  {
    angles <- runif (n, 0, 2 * pi)
    dists <- runif (n, 0, radius)
    x <- dists * cos (angles)
    y <- dists * sin (angles)
  }
  data.frame(x, y)
}
