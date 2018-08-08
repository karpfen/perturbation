#' Distance to zero
#'
#' Generate boxplots showing the data's distance to (0/0).
#'
#' @param dat \code{list} containing all data.
#' @param xlabels \code{vector} of perturbation values as labels for the plot.
#'
#' @export
distance_to_zero <- function (dat, xlabels)
{
  xlabels <- as.character (round (xlabels, 2))
  nrow <- dim (dat [[1]]) [1]
  ncol <- length (dat)
  d_zero <- as.data.frame (matrix (0, nrow = nrow, ncol = ncol))

  for (i in seq_len (ncol))
  {
    d_original <- dat [[i]]
    d_zero [, i] <- sqrt ((d_original [, 1]) ^ 2 + (d_original [, 2]) ^ 2)
  }

  names (d_zero) <- xlabels
  dat_plot <- stack(d_zero)
  ggplot2::ggplot(dat_plot) +
    ggplot2::geom_boxplot(ggplot2::aes(x = ind, y = values)) +
    ggplot2::ggtitle ("Point distance from (0/0)") +
    ggplot2::xlab ("Perturbation") +
    ggplot2::ylab ("Distance") +
    ggplot2::theme_linedraw ()
}

#' Comparison plots
#'
#' Generate plot overlaying the original and distorted data.
#'
#' @param dat_orig \code{data.frame} of original point data.
#' @param dat_dist \code{list} containing all data.
#' @param labels \code{vector} of perturbation values as labels for the plots.
#'
#' @export
comparison_plots <- function (dat_orig, dat_dist, labels)
{
  labels <- as.character (round (labels, 2))
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
    title <- paste0 ("Original - distorted points (Perturbation: ", labels [i], ")")

    plots_out [[i]] <- ggplot2::ggplot (dat_plot,
                                        ggplot2::aes (x = x, y = y,
                                                      color = type)) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(limits = xl) +
      ggplot2::scale_y_continuous(limits = yl) +
      ggplot2::ggtitle (title) +
      ggplot2::xlab ("X") +
      ggplot2::ylab ("Y") +
      ggplot2::theme_linedraw ()
  }
  plots_out
}

#' Plot descriptive statistics
#'
#' Plot the statistics calculated by descriptive_stats.
#'
#' @param stats input data.
#'
#' @return A \code{ggplot2} object of the plot.
#'
#' @export
plot_descriptive_stats <- function (stats)
{
  stats_melt <- reshape2::melt (stats, id.vars = "Perturbation")
  names (stats_melt) [which (names(stats_melt) == "variable")] <- "Statistic"

  ggplot2::ggplot(stats_melt, ggplot2::aes(x = Perturbation, y = value, color = Statistic)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle ("Effects of perturbation on the descriptive statistics") +
    ggplot2::xlab ("Perturbation") +
    ggplot2::ylab ("Value") +
    ggplot2::theme_linedraw ()
}
