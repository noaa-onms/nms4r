#' Produce a map of where rocky intertidal data was collected
#'
#' This function produces an interactive map showing where rocky intertidal data
#' was collected by the MARINe consortium.
#'
#' @param nms The National Marine Sanctuary code.
#' @return This function returns a mapview object displaying data collection sites.
#' @export
#' @examples \dontrun{
#' map_nms_sites("cinms")
#' }
#'
map_nms_sites <- function(nms){

  NMS <- stringr::str_to_upper(nms)

  # get sites in nms
  dir_gdrive <- "/Volumes/GoogleDrive/Shared drives/NMS/data"
  dir_pfx     <- file.path(dir_gdrive, "github_info-intertidal_data")
  dir_shp1     <- file.path(dir_pfx, "shp")

  sites_nms_shp <- glue::glue("{dir_shp1}/{NMS}_sites.shp")
  nms_ply <- get_nms_polygons(nms)

  if (!file.exists(sites_nms_shp)){
    # BEN: note that the following line of code will fail because raw_csv and sites_csv
    # aren't defined anywhere
    if (!file.exists(sites_csv)) make_sites_csv(raw_csv, sites_csv)

    sites_pts <- readr::read_csv(sites_csv) %>%
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)

    sites_nms_pts <- sites_pts %>%
      sf::st_intersection(
        nms_ply %>%
          sf::st_buffer(0.01)) # 0.01 dd ≈ 1.11 km
    sf::write_sf(sites_nms_pts, sites_nms_shp)
  }
  sites_nms_pts <- sf::read_sf(sites_nms_shp)

  mapview::mapview(
    nms_ply, legend = TRUE, layer.name = "Sanctuary", zcol = "SANCTUARY") +
    mapview::mapview(
      sites_nms_pts, legend = TRUE, layer.name = "Site",
      zcol = "site", col.regions = colorRampPalette(brewer.pal(11, "Set3")))
}

#' Generate a plot of intertidal monitoring data
#'
#' This function generates a plot of time series data collected by the Rocky Intertidal
#' Monitoring program.
#'
#' @param d_csv A csv file containing the time series data.
#' @param NMS The National Marine Sanctuary code.
#' @param spp The species of interest.
#' @param sp_name The species name used in the title of the plot.
#' @param nms_rgns A table containing the locations within National Marine Sanctuary.
#' @param spp_targets The species that was being looked for when the species of interest was found.
#' @param fld_val The column of data to provide the y-axis to be plotted.
#' @param label_y The label for the y-axis for the plot.
#' @param label_x The label for the x-axis for the plot.
#' @param nms_skip_regions National Marine Sanctuaries to be skipped (using sanctuary codes).
#' @return This function returns a dygraph object of the plotted time series data.
#' @export
#' @import dplyr dygraphs glue lubridate magrittr mapview RColorBrewer readr tidyr
#'
#' @examples \dontrun{
#' nms <- "cinms"
#' d_csv <- "/Volumes/GoogleDrive/Shared drives/NMS/data/github_info-intertidal_data/sanctuary_species_percentcover.csv"
#' nms_rgns_csv    <- file.path(dir_pfx, "MARINe_graphs.xlsx - sites in regions.csv")
#' nms_rgns1 <- readr::read_csv(nms_rgns_csv) %>%
#'   tidyr::fill(nms) %>%
#'   dplyr::group_by(nms) %>%
#'   tidyr::fill(region) %>%
#'   dplyr::mutate(rgn = region)
#' plot_intertidal_nms(d_csv, "CINMS", "MYTCAL", "California Mussels", nms_rgns1)
#' }
#'
plot_intertidal_nms <- function(
  d_csv, NMS, spp, sp_name, nms_rgns, spp_targets = NULL,
  fld_val = "pct_cover", label_y = "Annual Mean Percent Cover (%)",
  label_x = "Year", nms_skip_regions = c("OCNMS","MBNMS")){

  d <- readr::read_csv(d_csv) %>%
    dplyr::filter(nms == NMS, sp %in% spp) %>%
    dplyr::rename(v = !!fld_val)

  if (!is.null(spp_targets)){
    d <- d %>%
      dplyr::filter(sp_target %in% spp_targets)
  }

  d <- d %>%
    dplyr::group_by(site, date) %>%
    dplyr::summarize(
      v = mean(v)) %>%
    dplyr::ungroup()

  if (!NMS %in% nms_skip_regions){
    sites_no_rgn <- d %>% dplyr::filter(site != NMS) %>% dplyr::anti_join(nms_rgns, by="site") %>% dplyr::pull(site) %>% unique()
    stopifnot(length(sites_no_rgn) == 0)
    rgns <- nms_rgns %>% dplyr::filter(nms == NMS) %>% dplyr::pull(rgn) %>% unique()
  } else {
    rgns = character(0)
  }

  if (length(rgns) > 0){
    # avg by region
    d_sites <- d %>%
      dplyr::filter(site != NMS) %>%
      dplyr::left_join(nms_rgns, by="site") %>%
      dplyr::group_by(rgn, date) %>%
      dplyr::summarize(
        v = mean(v)) %>%
      dplyr::ungroup()

    d_allsites <- d %>%
      dplyr::filter(site == NMS) %>%
      dplyr::mutate(
        rgn = site) %>%
      dplyr::select(rgn, date, v)

    d <- dplyr::bind_rows(d_sites, d_allsites)
  } else {
    d <- d %>%
      dplyr::mutate(
        rgn = site) %>%
      dplyr::select(rgn, date, v)
  }

  # avg by year and spread
  d <- d %>%
    dplyr::mutate(
      yr = lubridate::year(date)) %>%
    dplyr::group_by(rgn, yr) %>%
    dplyr::summarize(
      v = mean(v)) %>%
    tidyr::spread(rgn, v) # View(d)

  # create a time series to be plotted and then set it up the way dygraph needs it
  # for plotting
  q <- d %>%
    dplyr::mutate(
      date = as.Date(paste0(d$yr, "-07-01"), "%Y-%m-%d"),.before = "yr") %>%
    dplyr::mutate(yr = NULL)
  q <- xts::xts(x = q[,-1], order.by = q$date)

  # line colors
  if (ncol(q) - 1 > 12){
    pal <- mapview::colorRampPalette(RColorBrewer::brewer.pal(12, "Set1"))
    ln_colors <- pal(ncol(q))
  } else {
    ln_colors <- RColorBrewer::brewer.pal(ncol(q) , "Set3")
  }
  ln_colors[which(names(q) == NMS)] <- "black"

  # plot dygraph
  dygraphs::dygraph(
    q,
    main = glue::glue("{sp_name} in {NMS}"),
    xlab = label_x,
    ylab = label_y) %>%
    dygraphs::dyOptions(
      connectSeparatedPoints = FALSE,
      colors = ln_colors) %>%
    dygraphs::dySeries(NMS, strokeWidth = 3) %>%
    dygraphs::dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
    dygraphs::dyRangeSelector(fillColor = " #FFFFFF", strokeColor = "#FFFFFF")
}

#' Plot ERRDAP time series data
#'
#' The purpose of this function is to generate time series plots of ERRDAP data.
#' Currently, this function can plot sea surface temperature and chlorophyll data.
#'
#' @param csv The csv file containing the ERRDAP data to be plotted.
#' @param metric The metric to be plotted.
#' @param ... additional parameters to pass to \link[dygraphs]{dygraph}
#' @return This function outputs a dygraph object of the time series plot.
#' @export
#' @examples \dontrun{
#' csv_SST <-here::here("data/oceano/statistics_sst_cinms.csv")
#' plot_metric_timeseries(csv_SST, "sst")
#' }
#'
plot_metric_timeseries <- function(csv, metric, ...){

  # Read in the csv file
  data_history <- read.csv(csv, header = TRUE)
  dates<- data_history[,1]
  average_value <- data_history[,2]
  lower_value <- data_history[,5]
  upper_value <- data_history[,6]

  # create a data frame which lines up the data in the way that dygraph needs it
  history <- data.frame(date = as.Date(dates, "%Y-%m-%d"), avg_value = average_value, lower = lower_value, upper = upper_value)
  history <- xts::xts(x = history[,-1], order.by = history$date)

  # create the figure
  if (metric == "sst"){ # plotting sea surface temperature
    dygraphs::dygraph(
      history,
      main = "Sea Surface Temperature",
      xlab = "Date", ylab = "Temperature (°C)",
      ...)%>%
      dygraphs::dySeries(c("lower", "avg_value", "upper"), label = "Temperature (°C)", color = "Red")%>%
      dygraphs::dyRangeSelector(fillColor = " #FFFFFF", strokeColor = "#FFFFFF")
  } else if (metric == "chl") { # plotting chlorophyll
    dygraphs::dygraph(
      history,
      main = "Chlorophyll Concentration",
      xlab = "Date", ylab = "Chlorophyll Concentration, OC3 Algorithm (mg/m<sup>3</sup>)",
      ...)%>%
      dygraphs::dySeries(c("lower", "avg_value", "upper"), label = "Chlorophyll concentration", color = "Green")%>%
      dygraphs::dyRangeSelector(fillColor = " #FFFFFF", strokeColor = "#FFFFFF")
  } else { # if any other metric is called, stop everything
    stop("Error in metric: the function plot_metric_timeseries only currently knows how to handle the metrics sst and chl")
  }
}

#' Plot SST anomaly data
#'
#' The purpose of this function is to plot the sea surface temperature anomaly
#' data.
#'
#' @param csv_SST The csv file containing the SST anomaly data to be plotted.
#' @return This function outputs a dygraph object of the time series plot.
#' @export
#' @examples \dontrun{
#' csv_SST <-here::here("data/oceano/sst_anomaly_cinms.csv")
#' plot_metric_timeseries(csv_SST)
#' }
#'
plot_SST_anomaly <- function(csv_SST){

  # We want to plot the SST anomaly data with values below zero colored differently
  # than those above zero. The strategy here is to split the SST anomaly data (which
  # is 1 column of data) into 2 columns of data: values below zero and values above
  # zero. Each of those lines will be plotted separately as a different filled-in
  # color. We also need to interpolate some points. Every time the anomaly time
  # series data crosses the zero anomaly line (0 on the y axis), that "zero day"
  # needs to be inserted into the 2 columns of data. This is necessary to have a
  # figure that doesn't have gaps in the color fill.

  # load in the SST anomaly data
  SST_data<-read.csv(csv_SST, header = T, colClasses = c("Date", "numeric"))

  # create an output data frame and initialize the row of the data frame to write
  # on to 1
  output_data<-data.frame(date= SST_data$date,below=NA,above=NA )
  write_line <-1

  # Let's go through every row of the anomaly data
  for (i in 1:nrow(SST_data)){

    # first let's write the row of data to the output data frame, putting below
    # zero and above zero values in different columns
    if (SST_data$sst_anomaly[i] <= 0){
      output_data[write_line,]<- c(as.character(SST_data$date[i]),
                                   as.numeric(SST_data$sst_anomaly[i]), NA)
    }
    else {
      output_data[write_line,]<- c(as.character(SST_data$date[i]), NA,
                                   as.numeric(SST_data$sst_anomaly[i]))
    }
    write_line <- write_line +1

    # Next, let's see if the anomaly value AFTER the current one changes sign.
    # If it does, we need to calculate the date at which the anomaly was zero and
    # insert that "zero day" into the output file after the current anomaly value.
    # We want to skip this whole procedure if we are at the last record of the file.
    if ((i < nrow(SST_data)) && (sign(SST_data$sst_anomaly[i]) !=
                                 sign(SST_data$sst_anomaly[i+1])) ){
      first_SST <- SST_data$sst_anomaly[i]
      second_SST <- SST_data$sst_anomaly[i+1]
      first_date <- SST_data$date[i]
      second_date <- SST_data$date[i+1]
      day_num <- as.numeric(second_date - first_date)
      daily_increment <- (second_SST - first_SST)/day_num
      days_more <- round(abs(first_SST/daily_increment),0)
      output_data[write_line, ] <- c(as.character(first_date + days_more), 0, 0)
      write_line <- write_line +1
    }
  }

  # create the data frame to be read by dygraphs and then produce the dygraph figure
  history <- data.frame(date = as.Date(output_data$date, "%Y-%m-%d"),
                        below = as.numeric(output_data$below),
                        above = as.numeric(output_data$above))
  history <- xts::xts(x = history[,-1], order.by = history$date)

  dygraphs::dygraph(history, main = "Sea Surface Temperature Anomaly", xlab = "Date", ylab = "Sea Surface Temperature Anomaly (°C)") %>%
    dygraphs::dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
    dygraphs::dySeries("below", label = "°C Below Average", color = "Blue") %>%
    dygraphs::dySeries("above", label = "°C Above Average", color = "Red") %>%
    dygraphs::dyLegend(show = "auto", showZeroValues = FALSE) %>%
    dygraphs::dyLimit(0, strokePattern = "solid", color = "black") %>%
    dygraphs::dyRangeSelector(fillColor = " #FFFFFF", strokeColor = "#FFFFFF")
}



