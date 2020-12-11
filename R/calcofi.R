#' Map of CALCOFI sites
#'
#' This function generates an interactive figure that shows the area in which
#' CALCOFI spring season net samples were located.
#'
#' @param geo A geojson object that defines the polygons to be mapped.
#' @param filter_str A string used to filter in (or out) particular polygons.
#' @param colors A string vector that defines the colors of the mapped polygons.
#' @return The output is an interactive map of CALCOFI sites overlaid on a coastal map of Southern California.
#' @export
#' @examples \dontrun{
#' calcofi_map()
#' }
#'
calcofi_map <- function(
  geo        = "https://raw.githubusercontent.com/marinebon/calcofi-analysis/master/data/plys_cinms.geojson",
  filter_str = 'ply_code != "SoCal"',
  colors     = c("red", "yellow")
){
  plys <- sf::read_sf(geo)

  if (!any(is.na(filter_str), is.null(filter_str), nchar(filter_str)==0)){
    # https://edwinth.github.io/blog/dplyr-recipes/
    expr <- rlang::parse_expr(filter_str)
    plys <- dplyr::filter(plys, !! expr)
    plys <- plys %>%
      dplyr::mutate(
        area_km2 = sf::st_area(geometry) %>% units::set_units(km^2),
        color    = !!colors) %>%
      dplyr::arrange(desc(area_km2))
  }

  leaflet::leaflet(
    data = plys,
    options = leaflet::leafletOptions(
      attributionControl = F)) %>%
    leaflet::addProviderTiles(providers$Esri.OceanBasemap) %>%
    leaflet::addPolygons(
      label = ~ply_code,
      color = ~color, fillColor = ~color,
      fillOpacity = 0.4, weight = 2) %>%
    leaflet::addLegend(
      colors = ~color,
      labels = ~ply_code)
}

#' Produces plots of CALCOFI data
#'
#' This function produces plots of CALCOFI-originated time series data.
#'
#' @param csv The CALCOFI time series data, in csv format, to be plotted.
#' @param x_fld The column of the time series data to be used for the x-axis of the plot.
#' @param y_fld The column of the time series data to be used for the y-axis of the plot.
#' @param y_trans The transformation to be performed on the data used for the y-axis.
#' @param x_lab The label for the x-axis on the plot.
#' @param y_lab The label for the y-axis on the plot.
#' @param title The label for the title of the plot.
#' @param yrs_recent The number of most recent years to be shaded in the plot.
#' @param interactive A Boolean variable indicating whether the plot is to be interactive or not.
#' @param in_loop A Boolean variable indicating whether an error condition exists.
#' @return The output is a plot of time series data.
#' @export
#' @examples \dontrun{
#' calcofi_plot(csv = "https://raw.githubusercontent.com/marinebon/calcofi-analysis/master/data/Anchovy_CINMS.csv", title = "Anchovy - CINMS Region")
#' }
calcofi_plot <- function(
  csv,
  x_fld       = "year",
  y_fld       = "avg_larvae_count_per_volume_sampled",
  y_trans     = "log(y + 1)",
  x_lab       = "Year",
  y_lab       = "ln(mean abundance + 1)",
  title       = NULL,
  yrs_recent  = 5,
  interactive = T,
  in_loop     = F){

  d <- csv %>%
    stringr::str_replace_all(" ", "%20") %>%
    readr::read_csv()

  if (nrow(d) == 0) return(NULL)

  flds <- list(x = rlang::sym(x_fld), y = rlang::sym(y_fld))
  d <- dplyr::select(d, !!!flds)

  if (!is.null(y_trans))
    d <- dplyr::mutate(d, y = !! rlang::parse_expr(y_trans))

  z <- dplyr::filter(d, x < max(x) - lubridate::years(yrs_recent))
  y_avg <- mean(z$y)
  y_sd  <- sd(z$y)

  if (interactive){
    q<-d
    colnames(q)<-c("year","value")
    q <- xts::xts(x = q[,-1], order.by = q$year)
    p <- dygraphs::dygraph(q, main=title, xlab = x_lab, ylab=y_lab)  %>%
      dygraphs::dyOptions(colors="black") %>%
      dygraphs::dyRangeSelector(fillColor = " #FFFFFF", strokeColor = "#FFFFFF") %>%
      dygraphs::dyLimit(y_avg, color = "blue", strokePattern = "solid") %>%
      dygraphs::dyShading(from = y_avg-y_sd, to = y_avg+y_sd, axis = "y")

    if (in_loop){
      # [`ggplotly` from inside `for` loop in `.Rmd` file does not work · Issue #570 · ropensci/plotly](https://github.com/ropensci/plotly/issues/570)
      print(htmltools::tagList(p))
      message(
        "need to add dependencies in R chunk per: \n",
        " - https://github.com/marinebon/calcofi-analysis/blob/6c678b052ded628cf149d5e37a1560e9f5efa6e5/docs/index.Rmd#L595-L615\n",
        " - [`ggplotly` from inside `for` loop in `.Rmd` file does not work · Issue #570 · ropensci/plotly](https://github.com/ropensci/plotly/issues/570)")
    } else {
      p
    }
  } else {
    print(g)
  }
}
