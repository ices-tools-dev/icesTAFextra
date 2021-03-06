#' Zoom ggplot
#'
#' Change text size in a ggplot.
#'
#' @param x an object of class \code{"ggplot"}.
#' @param size text size multiplier.
#' @param main size of main title (default is \code{1.2 * size}).
## #' @param lab size of axis labels (default is \code{size}).
#' @param axis size of tick labels (default is \code{size}).
## #' @param strip size of strip labels (default is \code{size}).
## #' @param sub size of subtitle (default is \code{0.9 * size}).
## #' @param legend size of legend labels (default is \code{0.9 * size}).
#' @param adjust whether to center plot title.
#' @param \dots further arguments, currently ignored.
#'
#' @return The same ggplot object, but with altered text size.
#'
#' @note
#' The default values result in ggplots that have similar text size as base
#' plots, when using \code{taf.png}.
#'
#' This function ends with a \code{\link[=print.ggplot]{print}} call, to make it
#' easy to export the ggplot to a file, without the need of an explicit
#' \code{print}.
#'
#' @seealso
#' \code{\link[ggplot2]{ggplot}} initializes a ggplot.
#'
#' \code{\link[ggplot2]{ggsave}} can save a ggplot to a PNG file, as an
#' alternative pathway from \code{taf.png}.
#'
#' \code{\link[icesTAF]{zoom}} is a generic function in the \pkg{icesTAF}
#' package, to change text size in plots.
#'
#' \code{\link[icesTAF]{taf.png}} opens a PNG graphics device.
#'
#' @examples
#' library(icesTAF)
#' library(ggplot2)
#'
#' qplot(1,1)
#' zoom(qplot(1,1))
#' zoom(qplot(1,1), size=10, axis=11)
#'
#' \dontrun{
#' taf.png("myplot")
#' plot(1)
#' dev.off()
#'
#' taf.png("myggplot")
#' zoom(qplot(1,1))
#' dev.off()
#'
#' taf.png("myggplot_zoom")
#' zoom(qplot(1,1))
#' dev.off()
#'
#' # Export 1600x1200 file using ggsave
#' qplot(1,1)
#' ggsave("myggplot_save200.png", width=8, height=6, dpi=200)
#'
#' dpi <- 260
#' qplot(1,1)
#' ggsave("myggplot_save260.png", width=1600/dpi, height=1200/dpi, dpi=dpi)
#'}
#'
#' @importFrom ggplot2 element_text theme
#' @importFrom icesTAF zoom
#'
#' @export
#' @export zoom.ggplot

zoom.ggplot <-
  function(x, size=11, main=1.2*size, axis=1.3*size, adjust=TRUE, ...)
{
  if(adjust) x <- x + theme(plot.title=element_text(hjust=0.5))
  x <- x + theme(title=element_text(size=main))
  x <- x + theme(text=element_text(size=axis))
  print(x)
}
