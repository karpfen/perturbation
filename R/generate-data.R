#' Generate data
#'
#' Generate randomised test data. The data is uniformly distributed within a
#' given radius.
#'
#' @param n Number of points.
#' @param x X position of center.
#' @param y Y position of center
#' @param r Radius of the circle in which the data lies.
#'
#' @return A \code{data.frame} of x/y coordinates.
#'
#' @export
generate_data <- function (n = 100, x = 0.5, y = 0.5, r = 0.1)
{
  if (r == 0)
  {
    x <- rep (x, n)
    y <- rep (y, n)
  } else
  {
    angles <- runif (n, 0, 2 * pi)
    dists <- runif (n, 0, r)
    x <- dists * cos (angles)
    y <- dists * sin (angles)
  }
  data.frame(x, y)
}
