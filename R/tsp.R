#' Plot relative changes (percentage changes)
#'
#' 'tsp()' takes as an input a vector containing time series and produces as an
#' output a plot showing graphically the relative (compared to initial value)
#' change of the time series over time.
#'
#' It was moved from the `ter` package as a part of its *conscious uncoupling*
#' into several smaller packages. Needs heavy editing because the body of `tsp()`
#' is a *copy-paste* of `R` code which I wrote about a research project and
#' expects data frame in a very specific format (each input is a two-column
#' data frame that contains dates in the first column and values in the second
#' column). Furthermore, it is only limited to plotting three lines because it
#' uses [plot()] under the hood (instead of the more suitable [matplot()]
#' function).
#' @param line_1_data
#' @param line_2_data
#' @param line_3_data
#' @param start_date
#' @param end_date
#' @param y_ticks
#' @param labels_margin
#' @param y_axis
#' @param y_lim
#' @param mar_values
#'
#' @return
#' @export
#'
#' @examples
tsp <- function (                        # add percentage change plot function
  line_1_data = NULL,                    # require data for line 1
  line_2_data = NULL,                    # require data for line 2
  line_3_data = NULL,                    # require data for line 3
  start_date,                            # require start of period
  end_date,                              # require end of period,
  y_ticks = 100,                         # require tick distance for y-axis
  labels_margin = 0,                     # require space for line labels
  y_axis = TRUE,                         # ask for y_axis
  y_lim = c(-100, 100),                  # set default value of y_axis limits
  mar_values = c(2, 3, 2, 1)             # set default value of margins
) {

  line_1_data <-
    line_1_data |>                         # load line 1 data
    subset(date >= start_date) |>          # subset rows by date
    subset(date <= end_date) |>            # subset rows by date
    transform(value = value/value[1]) |>   # normalize values
    transform(value = value*100 - 100)     # make index

  line_2_data <-
    line_2_data |>                         # load line 2 data
    subset(date >= start_date) |>          # subset rows by date
    subset(date <= end_date) |>            # subset rows by date
    transform(value = value/value[1]) |>   # normalize values
    transform(value = value*100 - 100)     # make index

  line_3_data <-
    line_3_data |>                         # load line 3 data
    subset(date >= start_date) |>          # subset rows by date
    subset(date <= end_date) |>            # subset rows by date
    transform(value = value/value[1]) |>   # normalize values
    transform(value = value*100 - 100)     # make index

  par(                                # set plot options
    mar = mar_values,                 # set margins (bottom, left, top, right)
    cex = 0.9,                        # set text magnification
    family="serif"                    # specify font
  )

  plot(                               # start new plot
    line_1_data,                      # provide data for first line
    axes = F,                         # remove axes
    xlab = "", 		  					        # blank space for x-axis label
    ylab = "", 		              		  # blank space for y-axis label
    type = "l",                       # specify type of plot
    lwd = 1,                          # specify line width
    xlim =                            # specify x-axis limit
      c(as.Date(start_date), as.Date(end_date) + labels_margin),
    ylim = y_lim

  )

  lines(                              # add base line
    x = c(start_date, end_date) |>    # x-axis coordinates
      as.Date(),                      # in date format
    y = c(0, 0),                      # y-axis coordinates
    lty = "dotted"                    # line type
  )

  lines(                              # add second line
    line_2_data,                      # provide data for second line
    lty = "dashed"                    # specify type of second line
  )

  lines(                              # add third line
    line_3_data,                      # provide data for third line
    lty = "twodash"                   # specify type of third line
  )

  axis(						                            # add x-axis to plot
    side = 1, 				                        # axis should be drawn below
    at = c(start_date, end_date) |>           # specify labels locations
      as.Date(),	                            # in date format
    labels = c(start_date, end_date) |>       # specify labels text
      as.Date() |>                            # in date format
      format("%Y") |>                         # specify date format
      paste("г.")
  )

  if (y_axis == TRUE) {

    axis(						                            # add y-axis to plot
      side = 2, 				                        # axis should be drawn on left side
      at = seq(                                 # specify labels locations
        from = y_lim[1],                        # first location
        to = y_lim[2],                          # last location
        by = y_ticks),	                        # distance between labels
      labels = seq(                             # specify labels' text
        from = y_lim[1],                        # first label
        to = y_lim[2],                          # last label
        by = y_ticks                            # distance between first and last
      ) |>
        paste0("\\%"),
      las = 2,                        	      	# specify labels orientation
      tick = F				                          # remove ticks
    )

  }

  line_1_ending_value <-                   # compute y-axis coordinate for label 1
    line_1_data |>                         # load line 1 data
    magrittr::use_series(value) |>         # use value
    dplyr::last()                          # use last value

  line_2_ending_value <-                   # compute y-axis coordinate for label 2
    line_2_data |>                         # load line 2 data
    magrittr::use_series(value) |>         # use value
    dplyr::last()                          # use last value

  line_3_ending_value <-                   # compute y-axis coordinate for label 3
    line_3_data |>                         # load line 2 data
    magrittr::use_series(value) |>         # use value
    dplyr::last()                          # use last value

  labels <-                               # labels for the three lines
    c(
      "БГ", "ЕЗ", "САЩ"
    )


  graphics::text(                                               # add line labels
    x = as.Date(end_date) |>                          # recycle x-axis coordinates
      add(                                            # add space
        (as.Date(end_date) - as.Date(start_date)) |>
          multiply_by(0.01)                             # make space proportional
      ),                                           # to x-axis distance
    y = c(
      line_1_ending_value,
      line_2_ending_value,
      line_3_ending_value
    ),                                              # specify y-axis coordinates
    labels = labels,                                  # specify labels
    cex = 0.9,                                        # expand characters by 0.9
    adj = c(0,0)                                      # label justification (left)
  )
}


