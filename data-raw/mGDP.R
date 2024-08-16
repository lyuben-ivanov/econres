## code to prepare `mGDP` dataset goes here

mGDP <- readr::read_csv(
  file = paste0(getwd(),"/data-raw/gdp-per-capita-maddison.csv"),
  col_select = c(Entity:`GDP per capita`, -Code)) |>
  tidyr::pivot_wider(names_from = Entity, values_from = `GDP per capita`) |>
  dplyr::arrange(Year)

usethis::use_data(mGDP, overwrite = TRUE, internal = FALSE)

# source: https://ourworldindata.org/grapher/gdp-per-capita-maddison
