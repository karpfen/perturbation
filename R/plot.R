#' Distance to zero
#'
#' Generate boxplots showing the data's distance to (0/0).
#'
#' @param dat \code{data.frame} containing all data.
#'
#' @export
distance_to_zero <- function (dat)
{
  d_zero [1, ] <- sqrt ((d_original [, 1]) ^ 2 + (d_original [, 2]) ^ 2)
  d_zero [2, ] <- sqrt ((d_circle [, 1]) ^ 2 + (d_circle [, 2]) ^ 2)
  d_zero [3, ] <- sqrt ((d_donut [, 1]) ^ 2 + (d_donut [, 2]) ^ 2)
  d_zero [4, ] <- sqrt ((d_uni [, 1]) ^ 2 + (d_uni [, 2]) ^ 2)
  d_zero [5, ] <- sqrt ((d_gauss [, 1]) ^ 2 + (d_gauss [, 2]) ^ 2)
  boxplot (d_zero, main = "Point distance to (0/0)", ylab = "Distance")
}
