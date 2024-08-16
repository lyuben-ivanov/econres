#' TeX Exporting Function
#'
#' This function exports a plot object to a tex file using the awesome
#' tikzDevice engine.
#'
#' Use `texport()` if you really need to insert your file in a LaTeX document
#' as a TeX code (possibly to do some changes that are easier to do in LaTeX
#' than in R).
#'
#' @param x plot object to be exported to the current working directory
#' @param file_name name of resulting figure
#' @param operating_system specify the operating system to ensure smooth
#' handling of \' when the file path for the new figure is created
#'
#' @return a TeX file containing the plot that is being exported
#' @export
#'
#' @examples
texport <-
  function(x, file_name = "figure_1", operating_system = "macOS") {
    if (operating_system == "macOS" | operating_system == "Linux") {
      file_path <- paste0(getwd(),"/", file_name, ".tex")
    } else{
      file_path <- paste0(getwd(),"\\", file_name, ".tex")
    }

    tikzDevice::tikz(                                # starting tikzDevice
      file = file_path,                              # output path and file name
      width = 4.5, height = 2.5                      # the size of the figure
    )

    x

    dev.off()
  }
