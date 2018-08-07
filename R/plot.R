#' Distance to zero
#'
#' Generate boxplots showing the data's distance to (0/0).
#'
#' @param dat \code{data.frame} containing all data.
#' @param xlabels Optional \code{vector} of x labels for the plot.
#'
#' @export
distance_to_zero <- function (dat, xlabels)
{
  nrow <- dim (dat [[1]]) [1]
  ncol <- length (dat)
  d_zero <- as.data.frame (matrix (0, nrow = nrow, ncol = ncol))

  for (i in seq_len (ncol))
  {
    d_original <- dat [[i]]
    d_zero [, i] <- sqrt ((d_original [, 1]) ^ 2 + (d_original [, 2]) ^ 2)
  }
  if (missing(xlabels))
  {
    boxplot (d_zero, main = "Point distance to (0/0)", ylab = "Distance")
  } else
  {
    boxplot (d_zero, main = "Point distance to (0/0)", ylab = "Distance", xlab = xlabels)
  }
}
