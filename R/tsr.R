
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
#' mGDP |>
#' dplyr::select(c(Year, `United Kingdom`, `United States`, China, India, Germany)) |>
#' dplyr::filter(Year >= 1000) |>
#' dplyr::arrange(Year) |>
#' tsr(xlab = 'Year')
#'
#'
tsr <- function (
    x,
    currency = '$',
    unit = '',
    ...
) {

  par(
    mar = c(5, 5, 4, 6) + 0.1
  )

  graphics::matplot(
    x = dplyr::select(x, 1),
    y = dplyr::select(x, -1),
    type = 'l',
    bty = 'l',
    col = 1,
    ylab = '',
    xaxs = 'i',
    axes = F,
    ...
  )

  graphics::axis(
    side = 1
  )

  ylabs <- paste0(
    currency,
    graphics::axTicks(side = 2) |> format(big.mark = ',', trim = TRUE),
    ' ',
    unit
  )


  graphics::axis(
    side = 2,
    at = graphics::axTicks(side = 2),
    labels = ylabs,
    las = 2,
    tick = F
  )

}





