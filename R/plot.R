#' Distance to zero
#'
#' Generate boxplots showing the data's distance to (0/0).
#'
#' @param dat \code{list} containing all data.
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

  dat_plot <- stack(d_zero)
  ggplot2::ggplot(dat_plot) +
    ggplot2::geom_boxplot(ggplot2::aes(x = dat_plot$ind, y = dat_plot$values))
}

#' Comparison plots
#'
#' Generate plot overlaying the original and distorted data.
#'
#' @param dat_orig \code{data.frame} of original point data.
#' @param dat_dist \code{list} containing all data.
#' @param labels Optional \code{vector} of labels for the plots.
#'
#' @export
comparison_plots <- function (dat_orig, dat_dist, labels)
{
  dat_orig$type <- "Original"
  n_dist <- length (dat_dist)
  if (!missing(labels) && length(labels) != n_dist)
    stop ("labels must be of the same size as dat_dist.")

  minxy <- apply (sapply (dat_dist, function (x) {
    c (min (x [, 1]), min (x [, 2]))
  }), 1, min)

  maxxy <- apply (sapply (dat_dist, function (x) {
    c (max (x [, 1]), max (x [, 2]))
  }), 1, max)

  xl <- c (minxy [1], maxxy [1])
  yl <- c (minxy [2], maxxy [2])

  plots_out <- vector ("list", n_dist)

  for (i in seq_len (n_dist))
  {
    dat <- as.data.frame (dat_dist [[i]])
    names (dat) <- c ("x", "y")
    dat$type <- "Distorted"

    dat_plot <- rbind (dat_orig, dat)

    plots_out [[i]] <- ggplot2::ggplot (dat_plot,
                                        ggplot2::aes (x = dat_plot$x,
                                                      y = dat_plot$y,
                                                      color = dat_plot$type)) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(limits = xl) +
      ggplot2::scale_y_continuous(limits = yl)
  }
  plots_out
}
