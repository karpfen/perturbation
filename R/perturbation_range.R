#' Perturbation Range
#'
#' Applies a given perturbation function for a range of parameters to a set of
#' points
#'
#' @param pts \code{matrix} of original points.
#' @param perturbation_method Method used for perturbation (One of "circle",
#' "donut", "circle_uniform", "circle_gauss").
#' @param r1 \code{vector} of radii (or standard deviations for "circle_gauss").
#' @param r2 \code{vector} of radii (only needed if using donut pertubation method).
#'
#' @return A \code{list} of perturbated points.
#'
#' @export
apply_perturbation_range <- function (pts, perturbation_method, r1, r2)
{
  validfuncs <- c ("circle", "donut", "circle_uniform", "circle_gauss")
  if (!perturbation_method %in% validfuncs)
    stop ("Invalid perturbaton method selected.")
  if (perturbation_method == "donut" && missing (r2))
    stop ("r2 must be provided if using the donut perturbation method.")
  if (perturbation_method == "donut" && length (r1) != length (r2))
    stop ("r1 and r2 must be of same length.")

  res <- vector ("list", length (r1))
  for (i in seq_along (r1))
  {
    r1_i <- r1 [i]
    if (perturbation_method == "donut")
    {
      r2_i <- r2 [i]
      res [[i]] <- t (apply (pts, 1, donut, r1, r2))
    } else
    {
      res [[i]] <- t (apply (pts, 1, perturbation_method, r1))
    }
  }
  res
}
