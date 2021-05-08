#' Calculate SST anomaly
#'
#' This function calculates the SST anomaly for every month of SST data and then
#' writes that out to a csv file that will later be used to produce a SST anomaly figure. The
#' approach used here is based on [Reed et al. 2016, Nature Communications](https://www.nature.com/articles/ncomms13757).
#' Using the first 15 full years of data from the SST data set (2003-2017), an average SST value is
#' generated for every month of the year. Then the average SST value for the appropriate month is
#' subtracted from every value in the SST dataset.
#'
#' @param sanct The NMS sanctuary, with only the value "cinms" currently doing anything.
#' @return The output is a csv file containing the time series anomaly data.
#' @export
#' @import here
#' @examples \dontrun{
#' calculate_SST_anomaly("cinms")
#' }
#'
calculate_SST_anomaly <-function(sanct) {

  # The following mini-function generates the full path for a file in the data directory
  get_filepath <- function(csv_file, sanctuary){
    location<-here::here()
    start_point <- nchar(location) - nchar(sanctuary) +1
    if (substr(location, start_point, nchar(location)) == sanctuary){
      datafile <- here::here(paste0("data/oceano/",csv_file))
    } else {
      datafile <- here::here(paste0(sanctuary,"/data/oceano/",csv_file))
    }
    return(datafile)
  }

  # Let's read in the SST data file and then pull the data from 2003-2017
  SST_filepath <- get_filepath("statistics_sst_cinms.csv", sanct)
  SST_data<-read.csv(SST_filepath, header = T)
  right_dates <- SST_data[SST_data$date >= "2003-01-01" & SST_data$date <= "2017-12-31", ]

  # Now let's define a data frame, where for every month of the year, an average SST value is calculated
  SST_avg <- data.frame(Month = month.name, SST_Average_2003_2017 = 0)
  for (i in 1:12){
    month_slice <- right_dates[months(as.Date(right_dates$date)) == month.name[i],]
    SST_avg$SST_Average_2003_2017[i] <- round(mean(month_slice$average_sst),5)
  }

  # Now let's define a data frame, where for every SST in the dataset, we subtract the average SST for the
  # relevant month from that SST. This is the anomaly value.
  SST_anom <- data.frame(date = SST_data$date, sst_anomaly = 0)
  for (q in 1:length(SST_anom$date)){
    correct_month<-which(SST_avg$Month == months(as.Date((SST_anom[q,]$date))))
    SST_anomaly <- SST_data$average_sst[q] - SST_avg$SST_Average_2003_2017[correct_month]
    SST_anom$sst_anomaly[q] <- round(SST_anomaly,5)
  }

  # Let's write the anomaly data frame to a file
  write_filepath <- get_filepath("sst_anomaly_cinms.csv", sanct)
  write.csv(SST_anom, file = write_filepath, quote= F, row.names = F)
}

#' Generate statistics for any missing months for NMS Sanctuary
#'
#' The purpose of this function is to update csv files that hold a history of
#' satellite-derived metrics for a sanctuary. These csv files are then used as the basis
#' for graphs that plot the metric values over time. Currently, there are two such metrics
#' being kept track of in the csv files: sea surface temperature and chlorophyll. This function is intended
#' to be run each month in github actions, adding the latest month's data to the intended csv file - and additionally
#' filling in any data holes that have crept in, in previous months. The reason for these data holes is
#' that I have found the NOAA servers on which this satellite data is kept to be rather temperamental and often
#' down. So, it is very possible that for a given moment at which github actions is attempting to run this function,
#' the server will be down - meaning that for that month's run of this function, there will be no data. The hope is
#' that in future months, the server will be up and will fill in the data holes.
#'
#' @param csv_file the csv file containing the data for the given metric for the sanctuary
#' @param sanctuary the NMS sanctuary, with only "cinms" currently doing anything
#' @param erddap_id the dataset, with two values defined so far "jplMURSST41mday" & "nesdisVHNSQchlaMonthly"
#' @param metric the metric being pulled from the dataset with "sst" and "chlor_a" currently defined
#' @return The output is a csv file that contains a time series of satellite-data-derived statistics.
#' @export
#' @examples \dontrun{
#' calculate_statistics("cinms", "jplMURSST41mday", "sst", "avg-sst_cinms.csv")
#' calculate_statistics("cinms", "nesdisVHNSQchlaMonthly", "chlor_a", "avg-chl_cinms.csv")
#' }
#'
calculate_statistics <-function(sanctuary, erddap_id, metric, csv_file) {

  # the first step is to check if the function knows how to handle the dataset being called. If it doesn't, stop everything.
  if (!(erddap_id == "jplMURSST41mday" | erddap_id == "nesdisVHNSQchlaMonthly" | erddap_id == "erdMWchlamday")) {
    stop("Error in erddap_id: this function only currently knows how to handle the datasets jplMURSST41mday, erdMWchlamday, and nesdisVHNSQchlaMonthly")
  }

  # Next, let's pull in the starting date of the dataset
  dataset_info   <- rerddap::info(erddap_id)
  dataset_global <- dataset_info$alldata$NC_GLOBAL
  tt <- dataset_global[
    dataset_global$attribute_name %in%
      c('time_coverage_end','time_coverage_start'), "value", ]
  t_beg = strptime(tt[2], "%Y-%m-%dT%H:%M:%SZ", tz = "GMT") %>% as.Date()
  t_end = strptime(tt[1], "%Y-%m-%dT%H:%M:%SZ", tz = "GMT") %>% as.Date()

  # let's define the date sequence as every month in the date range
  date_sequence <- seq.Date(t_beg, t_end, by = 'month') # , len = 12)

  # DEBUG
  print("Date Sequence:")
  print(head(date_sequence))

  # for the following data set, the date is off by 1 day, so let's fix that
  if (erddap_id == "nesdisVHNSQchlaMonthly"){
    date_sequence <- date_sequence -1
  }

  # TODO: get list of dates in dataset

  # load in the csv file. The problem here is that there are a couple of possibilities of how our current path relates to the datafiles we want to access.
  # There are two possibilities accounted for here: 1 (the top half of the if statement): the path includes the sanctuary at the end, 2 (the else half of the if statement):
  # the path doesn't include the sanctuary at the end (in which case we need to add it)

  location<-here::here()
  start_point <- nchar(location) - nchar(sanctuary) +1
  if (substr(location, start_point, nchar(location)) == sanctuary){
    datafile <- here::here(paste0("data/oceano/",csv_file))
  } else {
    datafile <- here::here(paste0(sanctuary,"/data/oceano/",csv_file))
  }

  # DEBUG
  print("datafile:")
  print(datafile)

  # load in the csv file containing the SST or chlorophyll data for a given sanctuary
  read_in <- read.csv(datafile, stringsAsFactors = FALSE)

  # DEBUG
  print("Read In:")
  print(head(read_in))

  # Let's generate the data frame that will ultimately be written back out to overwrite the csv file.
  # The data frame by default sets NA for all metric values for every month, to start. Later in this function, we'll change
  # those values
  write_out <- data.frame(date_sequence, "NA", "NA", "NA", "NA", "NA", stringsAsFactors = FALSE)
  col2<- paste0("average_",metric)
  col3<- paste0("standard_deviation_",metric)
  col4<-paste0("median_",metric)
  col5<-paste0("quantile5_",metric)
  col6<-paste0("quantile95_",metric)
  names(write_out) <- c("date", col2, col3, col4, col5, col6)

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
      write_out[i, 2:6] = read_in[match_date, 2:6]
    } else {
      # if not, then we need to calculate the statistics from the satellite data, using the ply2erddap function
      year <- as.numeric(substr(write_out$date[i],1,4))
      month <- as.numeric(substr(write_out$date[i],6,7))
      try(
        # note the use of the try function, due to the flaky nature of the server holding the satellite
        # data. If the server is down, this given month will retain NA until a future point that the server is up
        write_out[i, 2:6]<-round(nms4r::ply2erddap(sanctuary, erddap_id, metric, year, month, c("mean", "sd", "median", "q5", "q95")),5)
      )
    }
  }

  # DEBUG
  print("Write Out:")
  print(head(write_out))

  # overwrite the existing csv file with the output dataframe
  write.table(write_out, file = datafile, sep =",", row.names=FALSE, quote = FALSE)
  return(invisible())
}

#' Get date range for an ERDDAP data set
#'
#' This function provides the first and last dates for which data is available for
#' an ERDDAP data set.
#' @param info A rerddap::info() object.
#' @return This function outputs a string vector, with the first element being the start date and the last element being the end date.
#' @export
#' @examples \dontrun{
#' get_dates(rerddap::info('jplMURSST41mday'))
#' }
#'
get_dates <- function(info){
  info$alldata$time %>%
    dplyr::filter(attribute_name=="actual_range") %>%
    pull(value) %>%
    str_split(", ", simplify = T) %>%
    as.numeric() %>%
    as.POSIXct(origin = "1970-01-01", tz = "GMT")
}

#' Get National Marine Sanctuary polygons
#'
#' Given NMS code (see
#' \url{https://sanctuaries.noaa.gov/library/imast_gis.html}), download and
#' extract zip, cache shapefile or read existing shapefile.
#'
#' @param nms The code for a national marine sanctuary.
#' @return The function returns a sf object containing the polygons of a sanctuary.
#' @export
#' @examples \dontrun{
#' get_nms_polygons("cinms")
#' }
#'
get_nms_polygons <- function(nms){
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

#' make_sites_csv BEN
#'
#' description BEN
#'
#' @param raw_csv BEN
#' @param sites_csv BEN
#' @return BEN
#'
make_sites_csv <- function(raw_csv, sites_csv){
  raw <- read_csv_fmt(raw_csv, raw_fmt)

  sites_pts <- raw %>%
    dplyr::rename(
      site = marine_site_name) %>%
    dplyr::group_by(site) %>%
    dplyr::summarize(
      lat = dplyr::first(latitude),
      lon = dplyr::first(longitude)) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)

  sites_pts %>%
    sf::st_set_geometry(NULL) %>%
    readr::write_csv(sites_csv)
}

#' Extract ERDDAP statistics from polygon by year-month
#'
#' Extract satellite data in an ERDDAP dataset from national marine sanctuary polygon
#' for a given year and month.
#'
#' @param sanctuary_code The sanctuary code based on prefix to \href{https://sanctuaries.noaa.gov/library/imast_gis.html}{Sanctuary GIS files}, e.g. \code{"cinms"} for Channel Islands Marine Sanctuary
#' @param erddap_id The dataset ID of ERDDAP dataset (see \href{https://coastwatch.pfeg.noaa.gov/erddap/index.html}{coastwatch.pfeg.noaa.gov/erddap}), e.g. \code{"jplMURSST41mday"} for Multi-scale Ultra-high Resolution SST Analysis
#' @param erddap_fld The variable of ERDDAP dataset to extract, e.g. \code{"sst"}
#' @param year 4-digit year.
#' @param month integer month (1-12).
#' @param stats The statistics to be calculated.
#' @return A list of values by statistic.
#' @export
#' @examples
#' \dontrun{
#' ply2erddap("cinms", "jplMURSST41mday", "sst", year = 2010, month = 6, c("mean", "sd"))
#' }
ply2erddap <- function(sanctuary_code, erddap_id, erddap_fld, year, month, stats) {

  # check inputs
  stopifnot(all(is.numeric(year), is.numeric(month)))

  # Get the polygons for the sanctuary.
  sanctuary_ply <-   sf::as_Spatial(sf::st_union(get_nms_polygons(sanctuary_code)))

  # set the x and y limits of the raster to be pulled based upon the sanctuary polygons
  bb <- sf::st_bbox(sanctuary_ply)

  # TODO: deal with wrapping around dateline
  # https://github.com/rstudio/leaflet/issues/225#issuecomment-347721709

  # The dates to be considered (note that dates are handled differently when pulling different datasets)

  if (erddap_id == "nesdisVHNSQchlaMonthly") {
    m_date <- lubridate::ymd(glue::glue("{year}-{month}-01"))
  } else if (erddap_id == "jplMURSST41mday" || erddap_id == "erdMWchlamday") {
    m_date <- lubridate::ymd(glue::glue("{year}-{month}-16"))
  } else {
    stop("Error in erddap_id: the function ply2erddap only currently knows how to
         handle the datasets nesdisVHNSQchlaMonthly, jplMURSST41mday, and erdMWchlamday")
  }

  # set desired date range
  m_dates <- c(m_date, m_date)

  # pull data from errdap server, with the process handled differently based upon the dataset - the value of erddap_id
  # (as the datasets are not structured identically)

  # Let's define the latitude and longitude box for the raster we want to create. The dataset erdMWchlamday defines longitude in positive
  # degrees east, while the other two datasets considered so far (jplMURSST41mday & nesdisVHNSQchlaMonthly) define longitude in negative degrees west.
  # Since the polygons for the Sanctuary have their longitude defined in negative degrees west (as pulled in the variable bb),
  # longitude translation is required for the erdMWchlamday dataset
  latitude_range <- c(bb$ymin, bb$ymax)

  if (erddap_id == "erdMWchlamday"){
    longitude_range <- c(360 + bb$xmax, 360 + bb$xmin)
  } else {
    longitude_range <- c(bb$xmax, bb$xmin)
  }

  nc <- try(
    rerddap::griddap(
      rerddap::info(erddap_id),
      url = "https://coastwatch.pfeg.noaa.gov/erddap/",
      time = m_dates,
      latitude = latitude_range, longitude = longitude_range,
      fields = erddap_fld, fmt = 'nc'))
  if ("try-error" %in% class(nc)){
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
  } else {
    r <- raster::raster(nc$summary$filename)
  }

  # The following get_stat function extracts a statistical value (eg. mean or standard deviation) from the raster
  # cells remaining after being overlaid with the sanctuary polygons

  get_stat <- function(stat, v){
    # stat <- "mean"
    # stat <- "q95"

    q_pct <- stringr::str_match(stat, "^q([0-9]+)$")[2] %>%
      as.numeric()

    if (!is.na(q_pct)){
      quantile(v, q_pct/100)
    } else {
      fxn <- get(stat)
      fxn(v)
    }
  }

  # Let's run the function get_stat for every statistic asked for by the parameter value stats - this is the overall function output
  # stats = c("mean", "sd", "q5", "q95")
  r_v <- raster::extract(
    r, sanctuary_ply, layer = 1,
    method = "simple", na.rm=TRUE)[[1]]

  # The raster::extract function above is supposed to remove NA values, but (at least
  # in the case of the nesdisVHNSQchlaMonthly dataset) it doesn't. I have no idea why.
  # So, let's get rid of NA values for sure
  r_v <- na.omit(r_v)

  out <- purrr::map_dbl(stats, get_stat, v = r_v)
  names(out) <- stats
  out
}

#' read_csv_fmt BEN
#'
#' description BEN
#'
#' @param csv BEN
#' @param erddap_format BEN
#' @return BEN
#'
read_csv_fmt <- function(csv, erddap_format = "csv"){
  # erddap_format = "csv" # or "csvp"

  stopifnot(erddap_format %in% c("csv", "csvp"))

  if (erddap_format == "csv"){
    # ERDDAP: csv format, remove units from 2nd row
    hdr <- readr::read_csv(csv, n_max=1)
    d <- readr::read_csv(csv, skip = 2, col_names = names(hdr))
  }

  if (erddap_format == "csvp"){
    # ERDDAP: csvp format; remove ' (units)' suffix
    d <- readr::read_csv(csv)
    names(d) <- names(d) %>% stringr::str_replace(" \\(.*\\)", "")
  }
  d
}
