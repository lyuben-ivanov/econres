
#' Function for plotting unadjusted (raw) time series
#'
#' @param x
#' @param y
#' @param xlab
#' @param main
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tsr <- function (
    x,
    y,
    xlab = NULL,
    main = NULL,
    ...
) {
  graphics::matplot(
    x = x,
    y = y,
    type = 'l',
    bty = 'l',
    col = 1,
    xlab = xlab,
    ylab = '',
    xaxs = 'i',
    axes = F,
    main = main
  )

  graphics::axis(
    side = 1,
    line = 1
  )

  graphics::axis(
    side = 2,
    at = graphics::axTicks(side = 2),
    labels = graphics::axTicks(side = 2),
    las = 2
  )

}
