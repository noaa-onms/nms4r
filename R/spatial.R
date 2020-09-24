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
    #set latitude and longitude limits of raster
    ylim <- range(nc$data$lat, na.rm = TRUE)
    xlim <- range(nc$data$lon, na.rm = TRUE)
    ext <- raster::extent(xlim[1], xlim[2], ylim[1], ylim[2])

    #create raster
    d <- dplyr::arrange(nc$data, desc(nc$data$lat), nc$data$lon)
    r <- raster::raster(nrows = length(unique(nc$data$lat)), ncols = length(unique(nc$data$lon)),
                        ext = ext, vals = d[,erddap_fld])

  } else { # if errdap_id calls any other dataset, stop everything as who knows how this other dataset is structured
    stop("Error in erddap_id: this function only currently knows how to handle the datasets jplMURSST41mday and nesdisVHNSQchlaMonthly")
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
  } else if (erddap_id == "nesdisVHNSQchlaMonthly"){
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
  datafile <- here::here(paste0("data/oceano/",csv_file))
  read_in <- read.csv(datafile)

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
