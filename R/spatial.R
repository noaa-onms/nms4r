#' Get NMS polygon
#'
#' given NMS code (see
#' \url{https://sanctuaries.noaa.gov/library/imast_gis.html}), download and
#' extract zip, cache shapefile or read existing shapefile
#'
#' @param nms code for national marine sanctuary
#' @param dir_shp directory to store cached shapefile
#'
#' @return sf object
#' @export
#'
#' @examples

#' Generate statistics for raster data within NMS polygon
#'
#' @param sanctuary_code code for national marine sanctuary
#' @param erddap_id the name of the satellite data set to be pulled from erddap servers
#' @param erddap_fld the parameter to be pulled from the data set
#' @param year year of requested data in integer form
#' @param month month of requested data in integer form
#' @param stats the statistics to be generated for the data set
#'
#' @return statistics
#' @export
#' @import glue sf magrittr dplyr
#'
#' @examples
# This function gets the polygons for a National Marine Sanctuary
get_nms_polygons <- function(nms){
  # nms_shp <- here::here(glue::glue("data/shp/cinms_py.shp"))
  location<-here::here()
  start_point <- nchar(location) - nchar(nms) +1
  if (substr(location, start_point, nchar(location)) == nms){
    sanctuary_in_path = TRUE
  } else {
    sanctuary_in_path = FALSE
  }

  if (sanctuary_in_path == TRUE) {
   nms_shp <- here::here(glue::glue("data/shp/{nms}_py.shp"))
  } else {
    nms_shp <- paste0(location, "/", nms, "/data/shp/", nms, "_py.shp")
  }

  # download if needed
  if (!file.exists(nms_shp)){

    nms_url <- glue::glue("https://sanctuaries.noaa.gov/library/imast/{nms}_py2.zip")

    if (sanctuary_in_path == TRUE) {
      nms_zip <- here::here(glue::glue("data/{nms}.zip"))
      shp_dir <- here::here("data/shp")
    } else {
      nms_zip <- paste0(location, "/", nms, "/data/", nms, ".zip")
      shp_dir <-paste0(location, "/", nms, "/data/shp")
    }

    download.file(nms_url, nms_zip)
    unzip(nms_zip, exdir = shp_dir)
    file_delete(nms_zip)
  }
  # read and convert to standard geographic projection
  sf::read_sf(nms_shp) %>%
    sf::st_transform(4326)
}

#' Extract ERDDAP statistics from polygon by year-month
#'
#' Extract satellite data in an ERDDAP dataset from national marine sanctuary polygon for a given year and month.
#'
#' @param sanctuary_code sanctuary code based on prefix to \href{https://sanctuaries.noaa.gov/library/imast_gis.html}{Sanctuary GIS files}, e.g. \code{"cinms"} for Channel Islands Marine Sanctuary
#' @param erddap_id dataset ID of ERDDAP dataset (see \href{https://coastwatch.pfeg.noaa.gov/erddap/index.html}{coastwatch.pfeg.noaa.gov/erddap}), e.g. \code{"jplMURSST41mday"} for Multi-scale Ultra-high Resolution SST Analysis
#' @param erddap_fld variable of ERDDAP dataset to extract, e.g. \code{"sst"}
#' @param year 4-digit year
#' @param month integer month (1-12)
#' @param stats statistics to be generated for the data set \code{c("mean", "sd")}, to calculate mean and standard deviation)
#'
#' @return a list of values by statistic
#' @export
#' @import glue rerddap
#'
#' @examples
ply2erddap <- function (sanctuary_code, erddap_id, erddap_fld, year, month, stats) {

  # check inputs
  stopifnot(all(is.numeric(year), is.numeric(month)))

  # Get the polygons for the sanctuary.
  sanctuary_ply <-   sf::as_Spatial(sf::st_union(nms4r::get_nms_polygons(sanctuary_code)))

  # set the x and y limits of the raster to be pulled based upon the sanctuary polygons
  bb <- sf::st_bbox(sanctuary_ply)

  # TODO: deal with wrapping around dateline
  # https://github.com/rstudio/leaflet/issues/225#issuecomment-347721709

  # The dates to be considered (note that dates are handled differently when pulling different datasets)
  m_beg   <- lubridate::ymd(glue::glue("{year}-{month}-01"))
  m_end   <- m_beg + lubridate::days(lubridate::days_in_month(m_beg)) - lubridate::days(1)

  # pull data from errdap server, with the process handled differently based upon the dataset - the value of erddap_id
  # (as the datasets are not structured identically)

  # set desired date range
  m_dates <- c(m_beg, m_end)

  nc <- try(
    rerddap::griddap(
      rerddap::info(erddap_id),
      url = "https://coastwatch.pfeg.noaa.gov/erddap/",
      time = m_dates,
      #time = c("2013-01-01", "2013-06-01"),
      #time = "2013-04-01",
      latitude = c(bb$ymin, bb$ymax), longitude = c(bb$xmax, bb$xmin),
      fields = erddap_fld, fmt = 'nc'))
  if (class(nc) == "try-error"){
    stats_na <- setNames(rep(NA, length(stats)), stats) %>% as.list()
    return(stats_na)
  }

  if (erddap_id == "nesdisVHNSQchlaMonthly") { # pulling monthly chlorophyll data
    # TODO: delete this chunk since this dataset now seems gone or renamed?, per https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaMonthly.graph
    ylim <- range(nc$data$lat, na.rm = TRUE)
    xlim <- range(nc$data$lon, na.rm = TRUE)
    ext <- raster::extent(xlim[1], xlim[2], ylim[1], ylim[2])

    #create raster
    d <- dplyr::arrange(nc$data, desc(nc$data$lat), nc$data$lon)
    r <- raster::raster(nrows = length(unique(nc$data$lat)), ncols = length(unique(nc$data$lon)),
                        ext = ext, vals = d[,erddap_fld])
  } else { # if errdap_id calls any other dataset, stop everything as who knows how this other dataset is structured
    # stop("Error in erddap_id: this function only currently knows how to handle the datasets jplMURSST41mday and nesdisVHNSQchlaMonthly")
    r <- raster::raster(nc$summary$filename)
  }

  # The following get_stat function extracts a statistical value (eg. mean or standard deviation) from the raster
  # cells remaining after being overlaid with the sanctuary polygons

  get_stat <- function(stat){
    fxn <- get(stat)
    raster::extract(
      r, sanctuary_ply, layer = 1,
      method = "simple", na.rm=TRUE, fun = fxn)
  }

  # Let's run the function get_stat for every statistic asked for by the parameter value stats - this is the overall function output
  sapply(stats, get_stat)
}

#' Generate statistics for last month for NMS polygon
#'
#' @param wrapper_sanctuary_code code for national marine sanctuary
#' @param wrapper_erddap_id the name of the satellite data set to be pulled from erddap servers
#' @param wrapper_erddap_fld the parameter to be pulled from the data set
#' @param wrapper_stats the statistics to be generated for the data set
#'
#' @return nothing
#' @export
#'
#' @examples
generate_latest_SST<- function(wrapper_sanctuary_code, wrapper_erddap_id, wrapper_erddap_fld, wrapper_stats){
  # this function generates the SST statistics for the latest month of available data. It (hopefully) is
  # run near the beginning of the month and generates the SST statistics for the month before that.

  # a function to generate first day of current month
  som <- function(x) {
    as.Date(format(x, "%Y-%m-01"))
  }

  # calculate the month and year of last month
  last_month <- som(som(Sys.Date()) - 1)
  year <- as.numeric(substr(last_month, 1, 4))
  month <- as.numeric(substr(last_month, 6, 7))

  # The date range to be considered
  m_beg   <- lubridate::ymd(glue::glue("{year}-{month}-01"))
  m_end   <- m_beg + lubridate::days(lubridate::days_in_month(m_beg)) - lubridate::days(1)
  m_dates <- c(m_beg, m_end)

  # write to this file
  SST_file <- paste0(here::here("data/oceano/"),"avg-sst_", wrapper_sanctuary_code, ".csv")

  # generate requested statistics using the ply2erddap function and then append the requested data to
  # the csv file that has the data
  write_out = ply2erddap(sanctuary_code = wrapper_sanctuary_code, erddap_id = wrapper_erddap_id, wrapper_erddap_fld, year = year, month = month, stats = wrapper_stats)
  write(paste0(year, "-" , month, "-16," , round(write_out[1], 5), "," , round(write_out[2], 5)), file = SST_file, append = TRUE)
  return(invisible())
}

#' Generate statistics for any missing months for NMS Sanctuary
#'
#' @param wrapper_sanctuary code for national marine sanctuary
#' @param wrapper_erddap_id the name of the satellite data set to be pulled from erddap servers
#' @param wrapper_metric the parameter to be pulled from the data set
#' @param csv_file the csv file containing the data for the given metric for the sanctuary
#'
#' @return nothing
#' @export
#'
#' @examples
calculate_statistics <-function(sanctuary, erddap_id, metric, csv_file) {
  # The purpose of this function is to update csv files that hold a history of
  # satellite-derived metrics for a sanctuary. These csv files are then used as the basis
  # for graphs that plot the metric values over time. Currently, there are two such metrics
  # being kept track of in the csv files: sea surface temperature and chlorophyll. This function is intended
  # to be run each month in github actions, adding the latest month's data to the intended csv file - and additionally
  # filling in any data holes that have crept in, in previous months. The reason for these data holes is
  # that I have found the NOAA servers on which this satellite data is kept to be rather temperamental and often
  # down. So, it is very possible that for a given moment at which github actions is attempting to run this function,
  # the server will be down - meaning that for that month's run of this function, there will be no data. The hope is
  # that in future months, the server will be up and will fill in the data holes.

  # There are four parameters for this function: 1) sanctuary - the NMS sanctuary, with "cinms" currently doing
  # anything, 2) erddap_id: the dataset, with two values defined so far "jplMURSST41mday" & "nesdisVHNSQchlaMonthly",
  # 3) metric: the metric being pulled from the dataset with "sst" and "chlor_a" currently defined, and 4) csv_file:
  # the csv file that holds the data to be updated.

  # the first step is to set some variables depending on the dataset being called
  if (erddap_id == "jplMURSST41mday"){
    start_day <- 16
    beginning_year <- 2002
    beginning_month <- 6
  #} else if (erddap_id == "nesdisVHNSQchlaMonthly"){ # unknown datasetID
  } else if (erddap_id == "erdMWchlamday"){
    start_day <- 1
    beginning_year <- 2012
    beginning_month <- 1
  } else {
    stop("Error in erddap_id: this function only currently knows how to handle the datasets jplMURSST41mday and nesdisVHNSQchlaMonthly")
  }

  # next, let's calculate the date range over which we want to find values. The date range is defined
  # as the beginning of the satellite coverage for that dataset to current
  som <- function(x) {
    as.Date(format(x, "%Y/%m/01"))
  }
  last_month <- som(som(Sys.Date()) - 1)
  end_year <- as.numeric(substr(last_month, 1, 4))
  end_month <- as.numeric(substr(last_month, 6, 7))
  start_date <- paste(beginning_year,beginning_month,start_day, sep = "/")
  end_date <- paste(end_year,end_month,start_day, sep = "/")

  # let's define the date sequence as every month in the date range
  date_sequence <- seq(as.Date(start_date), as.Date(end_date), "months")

  # load in the csv file
  location<-here::here()
  start_point <- nchar(location) - nchar(sanctuary) +1
  if (substr(location, start_point, nchar(location)) == sanctuary){
      datafile <- here::here(paste0("data/oceano/",csv_file))
  } else {
      datafile <- here::here(paste0(sanctuary,"/data/oceano/",csv_file))
  }
  read_in <- read.csv(datafile, stringsAsFactors = FALSE)

  # Let's generate the data frame that will ultimately be written back out to overwrite the csv file.
  # The data frame by default sets NA for all metric values for every month, to start. Later in this function, we'll change
  # those values
  write_out <- data.frame(date_sequence, "NA", "NA")
  col2<- paste0("average_",metric)
  col3<- paste0("standard_deviation_",metric)
  names(write_out) <- c("date", col2, col3)

  # let's go through every month in the date range
  for (i in 1:length(date_sequence)){

    # create a flag to keep track of whether the data for a particular month needs to be calculated
    need_to_calculate = FALSE

    # check to see if the month in question exists in the existing data
    match_date <- which(read_in$date == date_sequence[i])
    if (length(match_date)==0){ # if the month doesn't exist, we need to calculate the data for this month
      need_to_calculate = TRUE
    } else {
      # additionally, if the date exists, but the data for that date is NA, we need to calculate the data for this month
      if (is.na(read_in[match_date,2])==TRUE) {need_to_calculate = TRUE}
    }

    # if non NA data exists for a given month, copy that for the month in the data frame that is going to
    # eventually write over the existing csv file
    if (need_to_calculate==FALSE){
      write_out[i, 2:3] = read_in[match_date, 2:3]
    } else {
      # if not, then we need to calculate the statistics from the satellite data, using the ply2erddap function
      year <- as.numeric(substr(write_out$date[i],1,4))
      month <- as.numeric(substr(write_out$date[i],6,7))
      try(
        # note the use of the try function, due to the flaky nature of the server holding the satellite
        # data. If the server is down, this given month will retain NA until a future point that the server is up
        write_out[i, 2:3]<-round(nms4r::ply2erddap(sanctuary, erddap_id, metric, year, month, c("mean", "sd")),5)
      )
    }
  }
  # overwrite the existing csv file with the output dataframe
  write.table(write_out, file = datafile, sep =",", row.names=FALSE, quote = FALSE)
  return(invisible())
}

#' get_box
#'
#' @param lon
#' @param lat
#' @param cells_wide
#'
#' @return
#' @export
#'
#' @examples
get_box <- function(lon, lat, cells_wide){
  w <- cells_wide * 0.01 / 2
  box <- list(
    lon = c(round(lon, 2) - w, round(lon, 2) + w),
    lat = c(round(lat, 2) - w, round(lat, 2) + w))
}

#' get_dates
#'
#' @param info
#'
#' @return
#' @export
#'
#' @examples
get_dates <- function(info){
  info$alldata$time %>%
    filter(attribute_name=="actual_range") %>%
    pull(value) %>%
    str_split(", ", simplify = T) %>%
    as.numeric() %>%
    as.POSIXct(origin = "1970-01-01", tz = "GMT")
}

#' get_raster
#'
#' @param info
#' @param lon
#' @param lat
#' @param date
#' @param field
#'
#' @return
#' @export
#'
#' @examples
get_raster <- function(info, lon, lat, date="last", field="sst"){
  g <- griddap(
    info, longitude = lon, latitude = lat,
    time = c(date, date), fields = field)
  grid_to_raster(g, "sst") %>%
    leaflet::projectRasterForLeaflet(method="ngb")

}

#' get_raster_2
#'
#' @param info
#' @param lon
#' @param lat
#' @param date
#' @param field
#'
#' @return
#' @export
#'
#' @examples
get_raster_2 <- function(info, lon, lat, date="last", field="chlor_a"){
  g_2 <- griddap(
    info, longitude = lon, latitude = lat,
    time = c(date, date), fields = field)
  grid_to_raster(g_2, "chlor_a") %>%
    leaflet::projectRasterForLeaflet(method="ngb")
}

#' get_timeseries
#'
#' @param info
#' @param lon
#' @param lat
#' @param csv
#' @param field
#'
#' @return
#' @export
#'
#' @examples
get_timeseries <- function(info, lon, lat, csv, field="sst"){

  dates  <- get_dates(info)

  if (file.exists(csv)){
    d_prev <- read_csv(csv) %>%
      arrange(date)
    start_date <- read_csv(csv) %>%
      tail(1) %>%
      pull(date) %>%
      as.POSIXct()
  } else {
    start_date <- dates[1]
  }

  v <- griddap(
    info,
    longitude = c(lon, lon), latitude = c(lat, lat),
    time = c(start_date, dates[2]), fields = field)

  d_now <- v$data %>%
    as_tibble() %>%
    mutate(
      date = lubridate::as_date(time, "%Y-%m-%dT00:00:00Z")) %>%
    select(date, field) %>%
    arrange(date)

  if (file.exists(csv)){
    d <- bind_rows(d_prev, d_now) %>%
      filter(!duplicated(date))
  } else {
    d <- d_now
  }

  d %>%
    write_csv(csv)
  d
}

#' grid_to_raster
#'
#' @param grid
#' @param var
#'
#' @return
#' @export
#'
#' @examples
grid_to_raster <- function (grid, var) {
  # original: plotdap:::get_raster
  # grid <- sst_grid
  #library(magrittr)

  times <- grid$summary$dim$time$vals
  lats <- grid$summary$dim$latitude$vals
  lons <- grid$summary$dim$longitude$vals
  ylim <- range(lats, na.rm = TRUE)
  xlim <- range(lons, na.rm = TRUE)
  ext <- raster::extent(xlim[1], xlim[2], ylim[1], ylim[2])
  r <- if (length(times) > 1) {
    d <- dplyr::arrange(grid$data, time, desc(lat), lon)
    b <- raster::brick(nl = length(times), nrows = length(lats),
                       ncols = length(lons))
    raster::values(b) <- lazyeval::f_eval(var, d)
    raster::setExtent(b, ext)
  }
  else {
    d <- dplyr::arrange(grid$data, desc(lat), lon)
    r <- raster::raster(nrows = length(lats), ncols = length(lons),
                        #ext = ext, vals = lazyeval::f_eval(var, d)) # plotdap:::get_raster
                        ext = ext, vals = d[,var])
  }
  #browser()
  #names(r) <- make.names(unique(grid$data$time) %||% "")
  r
}

#' map_raster
#'
#' @param r
#' @param site_lon
#' @param site_lat
#' @param site_label
#' @param title
#'
#' @return
#' @export
#'
#' @examples
map_raster <- function(r, site_lon, site_lat, site_label, title){
  pal <- colorNumeric(colors$temperature, values(r), na.color = "transparent")

  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group="Color") %>%
    addProviderTiles(providers$Stamen.TonerLite, group="B&W") %>%
    #addProviderTiles(providers$Stamen.TonerLabels) %>%
    addRasterImage(r, colors = pal, opacity = 0.8, project=F, group="CHL") %>%
    addMarkers(lng = site_lon, lat = site_lat, label = site_label) %>%
    addLegend(pal = pal, values = values(r), title = title, position="bottomright") %>%
    addLayersControl(
      baseGroups = c("Color", "B&W"),
      overlayGroups = c("SST"),
      options = layersControlOptions(collapsed = T))
}

#' map_raster_2
#'
#' @param r
#' @param site_lon
#' @param site_lat
#' @param site_label
#' @param title
#'
#' @return
#' @export
#'
#' @examples
map_raster_2 <- function(r, site_lon, site_lat, site_label, title){
  pal <- colorNumeric(colors$temperature, values(r), na.color = "transparent")

  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group="Color") %>%
    addProviderTiles(providers$Stamen.TonerLite, group="B&W") %>%
    #addProviderTiles(providers$Stamen.TonerLabels) %>%
    addRasterImage(r, colors = pal, opacity = 0.8, project=F, group="CHL") %>%
    addMarkers(lng = site_lon, lat = site_lat, label = site_label) %>%
    addLegend(pal = pal, values = values(r), title = title, position="bottomright") %>%
    addLayersControl(
      baseGroups = c("Color", "B&W"),
      overlayGroups = c("CHL"),
      options = layersControlOptions(collapsed = T))
}

#' plot_metric_timeseries
#'
#' @param csv
#' @param metric
#'
#' @return
#' @export
#'
#' @examples
plot_metric_timeseries <- function(csv, metric){
  # The purpose of this function is to generate figures showing the sea surface temperature time series
  # for a Sanctuary (displaying both avg and standard deviation of temp). The function has two parameters: 1)
  # csv: which is the path name for the csv data file to be plotted and 2) metric: which is the type of data
  # to be plotted; currently only "sst" (for sea surface temperature) and "chl" (for chlorophyll) are recognized

  # Read in the csv file
  data_history <- read.csv(csv, header = TRUE)
  dates<- data_history[,1]
  average_value <- data_history[,2]
  standard_deviation <- data_history[,3]

  # create a data frame which lines up the data in the way that dygraph needs it
  history <- data.frame(date = as.Date(dates, "%Y-%m-%d"), avg_value = average_value, lower = average_value - standard_deviation, upper = average_value + standard_deviation)
  history <- xts(x = history[,-1], order.by = history$date)

  # create the figure
  if (metric == "sst"){ # plotting sea surface temperature
    dygraph(history, main = "Sea Surface Temperature", xlab = "Date", ylab = "Temperature (°C)")%>%
      dySeries(c("lower", "avg_value", "upper"), label = "Temperature (°C)", color = "Red")%>%
      dyRangeSelector()
  } else if (metric == "chl") { # plotting chlorophyll
    dygraph(history, main = "Chlorophyll Concentration", xlab = "Date", ylab = "Chlorophyll Concentration, OC3 Algorithm (mg/m<sup>3</sup>)")%>%
      dySeries(c("lower", "avg_value", "upper"), label = "Chlorophyll concentration", color = "Green")%>%
      dyRangeSelector()
  } else { # if any other metric is called, stop everything
    stop("Error in metric: the function plot_metric_timeseries only currently knows how to handle the metrics sst and chl")
  }
}

#' plot_timeseries
#'
#' @param d
#' @param title
#' @param color
#' @param dyRangeSelector
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_timeseries <- function(d, title="SST", color="red", dyRangeSelector=T, ...){
  p <- xts(select(d, -date), order.by=d$date) %>%
    dygraph(main=title, ...) %>%
    dyOptions(
      colors = color,
      fillGraph = TRUE, fillAlpha = 0.4)
  if (dyRangeSelector){
    p <- p %>%
      dyRangeSelector()
  }
  p
}
