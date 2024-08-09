
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
#' tsr(xlab = 'Year', main = "The hockeystick of growth", cex.main = 1.2)
#'
#'
tsr <- function (
    x,
    currency = '$',
    unit = '',
    ...
) {

  def.par = graphics::par(no.readonly = TRUE) # save current par() settings

  graphics::par(
    mar = c(4, 5, 3, 6.5) + 0.1,
    tcl = -0.3,
    # cex = 0.8,
    font.main = 1,
    mgp = c(2, 1, 0)
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
    ylim = c(
      0,
      dplyr::select(x, -1) |> max(na.rm = TRUE) |> magrittr::multiply_by(1.11)
    ),
    ...
  )

  graphics::axis(
    side = 1,
    cex = 1
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
    cex = 1
  )

  graphics::text(
    x = x |> dplyr::select(1) |> utils::tail(n = 1),
    y = x |> utils::tail(n = 1) |> as.numeric() |> utils::tail(n = -1L),
    labels = x |> names() |> utils::tail(n = -1L),
    xpd = TRUE,
    pos = 4,
    cex = 1
  )

  graphics::par(def.par)

}





