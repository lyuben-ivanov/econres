
#' Function for plotting unadjusted (raw) time series
#'
#' `tsr()` takes a dataframe with raw time series data and uses the data to
#' create a time series plot with multiple lines for the time period which the
#' data covers.
#'
#' @param x A dataframe (in wide format) where the first column is a vector of
#' years and the rest of the columns are vectors of values presenting time
#' series for one or more economic variables.
#' @param currency The currency in which the time series are quoted (defaulting
#' to $).
#' @param unit The unit in which the time series are measured (mlns, kgs, etc.)
#' @param ... Arguments to be passed to methods of [plot()] and [lines()], such
#' as [graphical parameters] (see [par]).
#'
#' @return A plot object.
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

  graphics::par(
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
    side = 1,
    cex = 0.9
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
    tick = F,
    cex = 0.9
  )

  graphics::text(
    x = x |> dplyr::select(1) |> utils::tail(n = 1),
    y = x |> utils::tail(n = 1) |> as.numeric() |> utils::tail(n = -1L),
    labels = x |> names() |> utils::tail(n = -1L),
    xpd = TRUE,
    pos = 4,
    cex = 0.9
  )



}





