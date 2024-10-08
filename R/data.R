#' GDP per capita data for 178 countries
#'
#' Maddison Project data of GDP per capita for 178 different countries spanning
#' the time period from year 1 to year 2022 AD.
#'
#' The data is expressed in international-$ at 2011 prices. International
#' dollars are a hypothetical currency that is used to make meaningful
#' comparisons of monetary indicators of living standards.Figures expressed in
#' international dollars are adjusted for inflation within countries over time,
#' and for differences in the cost of living between countries. The goal of such
#' adjustments is to provide a unit whose purchasing power is held fixed over
#' time and across countries, such that one international dollar can buy the
#' same quantity and quality of goods and services no matter where or when it is
#' spent.
#'
#' @format
#' A dataframe with 776 rows and 179 columns:
#' \describe{
#'   \item{Year}{Year for which GDP per capita data is available}
#'   \item{Afghanistan, Albania, etc.}{Country names}
#' }
#' @source Our World in Data website (<https://ourworldindata.org/grapher/gdp-per-capita-maddison>)
#'
#' After the data was downloaded, it was recast in a wide form dataframe and the
#' country codes were removed.
#' @references Bolt and van Zanden - Maddison Project Database 2023 – with minor processing by Our World in Data
"mGDP"
