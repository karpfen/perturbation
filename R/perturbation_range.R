#' Circle perturbation range
#'
#' Applies a linear circle perturbation function for a range of parameters to a
#' set of points.
#'
#' @param pts \code{matrix} of original points.
#' @param r \code{vector} of radii.
#'
#' @return A \code{list} of \code{matrix} objects with perturbated points.
#'
#' @export
apply_circle_perturbation_range <- function (pts, r)
{
  res <- vector ("list", length (r))
  for (i in seq_along (r))
  {
    r_i <- r [i]
    res [[i]] <- t (apply (pts, 1, circle, r_i))
  }
  res
}

#' Donut perturbation range
#'
#' Applies a donut perturbation function for a range of parameters to a set of
#' points.
#'
#' @param pts \code{matrix} of original points.
#' @param r1 \code{vector} of inner radii.
#' @param r2 \code{vector} of outer radii.
#'
#' @return A \code{list} of \code{matrix} objects with perturbated points.
#'
#' @export
apply_donut_perturbation_range <- function (pts, r1, r2)
{
  if (length (r1) != length (r2))
    stop ("r1 and r2 must be of same length.")

  res <- vector ("list", length (r1))
  for (i in seq_along (r1))
  {
    r1_i <- r1 [i]
    r2_i <- r2 [i]
    res [[i]] <- t (apply (pts, 1, donut, r1_i, r2_i))
  }
  res
}

#' Uniform circle perturbation range
#'
#' Applies a uniformly distributed circle perturbation function for a range of
#' parameters to a set of points.
#'
#' @param pts \code{matrix} of original points.
#' @param r \code{vector} of radii.
#'
#' @return A \code{list} of \code{matrix} objects with perturbated points.
#'
#' @export
apply_circle_uniform_perturbation_range <- function (pts, r)
{
  res <- vector ("list", length (r))
  for (i in seq_along (r))
  {
    r_i <- r [i]
    res [[i]] <- t (apply (pts, 1, circle_uniform, r_i))
  }
  res
}

#' Gauss circle perturbation range
#'
#' Applies a normal (Gauss) distributed circle perturbation function for a
#' range of parameters to a set of points.
#'
#' @param pts \code{matrix} of original points.
#' @param r \code{vector} of radii.
#'
#' @return A \code{list} of \code{matrix} objects with perturbated points.
#'
#' @export
apply_circle_gauss_perturbation_range <- function (pts, r)
{
  res <- vector ("list", length (r))
  for (i in seq_along (r))
  {
    r_i <- r [i]
    res [[i]] <- t (apply (pts, 1, circle_gauss, r_i))
  }
  res
}
