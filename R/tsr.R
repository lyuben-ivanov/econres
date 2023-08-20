
#' Function for plotting unadjusted (raw) time series
#'
#' @param x
#' @param xlab
#' @param main
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' econres::mGDP |>
#' dplyr::select(c(Year, `United Kingdom`, `United States`, China, India, Germany)) |>
#' dplyr::filter(Year >= 1000) |>
#' dplyr::arrange(Year) |>
#' tsr(xlab = `Year`)
#'
#'
tsr <- function (
    x,
    xlab = NULL,
    main = NULL,
    ...
) {

  graphics::matplot(
    x = dplyr::select(x, 1),
    y = dplyr::select(x, -1),
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
    side = 1
  )

  graphics::axis(
    side = 2,
    at = graphics::axTicks(side = 2),
    labels = graphics::axTicks(side = 2),
    las = 2,
    tick = F
  )

}





