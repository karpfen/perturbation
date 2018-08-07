#' Plot animation wrapper
#'
#' Generate an animated gif from a list of ggplot objects.
#'
#' @param plots \code{list} of ggplot objects.
#' @param file_out output file location.
#' @param fps frames per second.
#' @param res resolution in pixels.
#'
#' @export
save_animated_plot <- function (plots, file_out, fps = 10, res = 96)
{
  img <- magick::image_graph (res = res)
  lapply (plots, print)
  dev.off ()
  animation <- magick::image_animate(img, fps = fps)
  image_write(animation, file_out)
}
