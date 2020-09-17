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
   nms_shp <- here::here(glue::glue("data/shp/{nms}_py.shp"))

  # download if needed
  if (!file.exists(nms_shp)){

    nms_url <- glue::glue("https://sanctuaries.noaa.gov/library/imast/{nms}_py2.zip")
    nms_zip <- here::here(glue::glue("data/{nms}.zip"))
    shp_dir <- here::here("data/shp")

    download.file(nms_url, nms_zip)
    unzip(nms_zip, exdir = shp_dir)
    file_delete(nms_zip)
  }
  # read and convert to standard geographic projection
  sf::read_sf(nms_shp) %>%
    sf::st_transform(4326)
}


#' Title
#'
#' @param sanctuary_code
#' @param erddap_id
#' @param erddap_fld
#' @param year
#' @param month
#' @param stats
#'
#' @return
#' @export
#' @import glue rerddap
#'
#' @examples
ply2erddap <- function (sanctuary_code, erddap_id, erddap_fld, year, month, stats) {

 # The following  function generates statistics for the SST for a national marine sanctuary for a given
# month. For the moment, when we say "statistics", we mean the average SST and standard deviation.
# Parameters going into the function: 1) sanctuary_code: a string that describes a specific national marine sanctuary,
# ("cinms" for Channel Islands Marine Sanctuary), 2) erddap_id: the name of the satellite data set to be pulled
# from erddap servers ("jplMURSST41mday" for Multi-scale Ultra-high Resolution SST Analysis), 3) erddap_fld: the parameter
# to be pulled from the data set ("sst" for sea surface temperature), 4) year: in integer form, 5) month: in integer form,
# 6) stats: the statistics to be generated for the data set (c("mean", "sd"), to calculate mean and standard deviation).

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

  if (erddap_id == "jplMURSST41mday"){ # pulling monthly sea surface temperature data
    # set desired date range
    m_dates <- c(m_beg, m_end)
    # pull the raster data
    nc <- rerddap::griddap(
      rerddap::info(erddap_id),
      time = m_dates,
      latitude = c(bb$ymin, bb$ymax), longitude = c(bb$xmax, bb$xmin),
      fields = erddap_fld, fmt = 'nc')
    # Extract the raster from the data object.
    r <- raster::raster(nc$summary$filename)
  } else if (erddap_id == "nesdisVHNSQchlaMonthly") { # pulling monthly chlorophyll data
    # set desired date range
    m_dates <- c(m_beg, m_beg)
    nc <- rerddap::griddap(
      rerddap::info(erddap_id),
      time = m_dates,
      latitude = c(bb$ymin, bb$ymax), longitude = c(bb$xmax, bb$xmin),
      fields = erddap_fld, fmt = 'nc')
    lat <- nc$data$lat
    lon <- nc$data$lon
    ylim <- range(nc$data$lat, na.rm = TRUE)
    xlim <- range(nc$data$lon, na.rm = TRUE)
    ext <- raster::extent(xlim[1], xlim[2], ylim[1], ylim[2])
    d <- dplyr::arrange(grid$data, desc(nc$data$lat), nc$data$lon)
    r <- raster::raster(nrows = length(unique(nc$data$lat)), ncols = length(unique(nc$data$lon)),
                        ext = ext, vals = d[,erddap_fld])
  } else { # if errdap_id calls any other dataset, stop everything as who knows how this other dataset is structured
    stop("Error in erddap_id: this function only currenly knows how to handle the datasets jplMURSST41mday and nesdisVHNSQchlaMonthly")
  }

  # The following get_stat function extracts a statistical value (eg. mean or standard deviation) from the raster
  # cells remaining after being overlaid with the sanctuary polygons

  get_stat <- function(stat){
    fxn <- get(stat)
    raster::extract(
      r, sanctuary_ply, layer = 1,
      method = "simple", fun = fxn)
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
