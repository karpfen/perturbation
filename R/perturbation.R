#' Circle line perturbation mask
#'
#' Adds perturbation constrained on a line with fixed length around a point.
#'
#' @param p Original point.
#' @param r Perturbation radius.
#'
#' @return Perturbated point.
#'
#' @export
circle <- function (p, r)
{
  angle <- runif (1, 0, 2 * pi)
  x_out <- p [1] + (r * cos (angle))
  y_out <- p [2] + (r * sin (angle))

  c (x_out, y_out)
}

#' Donut perturbation mask
#'
#' Adds perturbation constrained on a ring (donut) around a point.
#'
#' @param p Original point.
#' @param r1 Inner ring radius.
#' @param r2 Outer ring radius.
#'
#' @return Perturbated point.
#'
#' @export
donut <- function (p, r1, r2)
{
  if (r1 > r2)
  {
    r1 <- r1 + r2
    r2 <- r1 - r2
    r1 <- r1 - r2
  }
  r <- runif (1, r1, r2)
  circle (p, r)
}

#' Uniform Circle
#'
#' Adds perturbation within a circle with uniform distribution.
#'
#' @param p Original point.
#' @param r Circle radius.
#'
#' @return Perturbated point.
#'
#' @export
circle_uniform <- function (p, r)
{
  donut (p, 0, r)
}

#' Gauss Circle
#'
#' Adds perturbation within a circle with normal (Gauss) distribution.
#'
#' @param p Original point.
#' @param sd Standard deviation of the distribution.
#'
#' @return Perturbated point.
#'
#' @export
circle_gauss <- function (p, sd)
{
  r_circle <- abs (rnorm (1, sd = sd))
  circle (p, r_circle)
}
