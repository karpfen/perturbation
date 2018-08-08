#' Calculate descriptive statistics
#'
#' Calculate the mean, median, standard deviation of a set of perturbated data.
#'
#' @param dat input data.
#' @param range \code{vector} of perturbation values.
#'
#' @return A \code{data.frame} containing all statistics.
#'
#' @export
descriptive_stats <- function (dat, range)
{
  n_dat <- length (dat)
  if (n_dat != length (range))
    stop ("dat and range have to be of same length.")

  results <- as.data.frame (matrix (ncol = 4, nrow = n_dat, 0))
  names (results) <- c ("Perturbation", "Mean", "Median", "Standard deviation")

  results$Perturbation <- range
  results$Mean <- sapply (dat, mean)
  results$Median <- sapply (dat, median)
  results$`Standard deviation` <- sapply (dat, sd)

  results
}
