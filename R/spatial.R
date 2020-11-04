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
#' @examples calcofi_map()
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
#' @examples calcofi_plot(csv = "https://raw.githubusercontent.com/marinebon/calcofi-analysis/master/data/Anchovy_CINMS.csv", title = "Anchovy - CINMS Region")
#'
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
  y_r   <- scales::expand_range(range(d$y), mul=0.05)

  g <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
    ggplot2::annotate(
      "rect",
      xmin = max(d$x) - lubridate::years(yrs_recent), xmax = max(d$x) + months(6),
      ymin = y_r[1], ymax = y_r[2],
      fill  = "lightblue", alpha=0.5) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(
      yintercept = c(y_avg + y_sd, y_avg,  y_avg - y_sd),
      linetype   = c("solid", "dashed", "solid"),
      color       = "darkblue") +
    ggplot2::coord_cartesian(
      xlim = c(
        min(d$x) - months(6),
        max(d$x) + months(6)), expand = F) +
    ggplot2::theme_light() +
    ggplot2::labs(
      x     = x_lab,
      y     = y_lab,
      title = title)

  if (interactive){
    p <- plotly::ggplotly(g)
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
#' @examples calculate_SST_anomaly("cinms")
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
#' @examples
#' calculate_statistics("cinms", "jplMURSST41mday", "sst", "avg-sst_cinms.csv")
#' calculate_statistics("cinms", "nesdisVHNSQchlaMonthly", "chlor_a", "avg-chl_cinms.csv")
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

  # load in the csv file containing the SST or chlorophyll data for a given sanctuary
  read_in <- read.csv(datafile, stringsAsFactors = FALSE)

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
  # overwrite the existing csv file with the output dataframe
  write.table(write_out, file = datafile, sep =",", row.names=FALSE, quote = FALSE)
  return(invisible())
}

#' Generate the html for rmd files with interactive figures
#'
#' Rmd files with interactive figures present a special problem in terms of rendering
#' them into html. The problem is that, if one uses the markdown library to
#' create the html, the figures will turn out fine but the glossary tooltip functionality
#' will be missing. This tooltip functionality is created by the function rmd2html,
#' described in this package. If one uses rmd2html to render a rmd file containing
#' interactive figures, the tooltips will turn out fine but the figures won't show
#' up. The problem with rmd2html is that the appropriate javascript libraries are
#' not loaded into the resulting <head> section of the final html. This function
#' solves the problem (thereby producing html with both working figures and tooltips)
#' by rendering the rmd using both the markdown and rmd2html approaches and then rewriting
#' the <head> section of the rmd2html version with the markdown version.
#'
#' @param nms The NMS sanctuary, with only "cinms" currently doing anything.
#' @return The function outputs a html file for every rmd file containing interactive figures.
#' @export
#' @examples generate_html_4_interactive_rmd("cinms")
#'
generate_html_4_interactive_rmd <- function (nms){

  # the following mini-function where_is_head has two simple purposes. When fed in a html file, which has already been brought in
  # to R via readLines, the function will tell you the line number of the html file that contains "</html>" and
  # the total number of lines in the file
  where_is_head <-function(input_lines){
    i<-1
    while (!(input_lines[i]=="</head>")){
      i <-i + 1
    }
    output_list <- list("total_lines" = length(input_lines), "head_line" = i)
    return(output_list)
  }

  # Let's figure out where we are. In my local environment, I am in the directory for
  # the sanctuary. In a docker container though, I won't be. So the following section of
  # code attempts to put us in the right directory if we aren't there already.
  location <- here::here()
  start_point <- nchar(location) - nchar(nms) +1
  if (!(substr(location, start_point, nchar(location)) == nms)){
    location <- paste(location, nms, sep = "/")
  }
  modal_dir<- paste0(location,"/modals/")

  # Now, let's generate a list of rmd files that need to be worked on.
  if (nms == "cinms"){
    interactive_rmd <- c("algal-groups.Rmd",
    "barnacles.Rmd",
    "deep-seafloor_key-climate-ocean.Rmd",
    "forage-assemblage.Rmd",
    "forage-fish.Rmd",
    "forage-inverts.Rmd",
    "kelp-forest_key-climate-ocean.Rmd",
    "key-climate-ocean.Rmd",
    "mussels.Rmd",
    "ochre-stars.Rmd",
    "pelagic_key-climate-ocean.Rmd",
    "rocky-map.Rmd",
    "rocky-shore_key-climate-ocean.Rmd",
    "sandy-beach_key-climate-ocean.Rmd",
    "sandy-seafloor_key-climate-ocean.Rmd",
    "tar.Rmd")
  }

  oceano_Rmds<-paste0(modal_dir, interactive_rmd)

  # let's go through every rmd file to be worked on
  for (i in 1:length(oceano_Rmds)){
    # for a given rmd file, let's generate the html for it in two ways. Way 1 is via
    # rmd2html which gives us the glossary tooltip working right (but where the interactive
    # figures don't work). Way 2 is via render which gives us the interactive figures working
    # right (but where the glossary tooltip doesn't work)
    target_rmd<- oceano_Rmds[i] #  "/Users/jai/Documents/cinms/modals/key-climate-ocean.Rmd"
    rmd2html(target_rmd)
    rmarkdown::render(target_rmd, output_file = paste(modal_dir, "temp_file.html", sep ="/"))

    # We want both the interactive figures and the glossary tooltip working in the html. The way to do
    # that is to grab everything in the <head> section of the html produced by render and then
    # to replace the <head> section of the html produced by rmd2html with that. The first step
    # here is to read in the two html files
    target_html <- gsub("Rmd", "html", target_rmd)
    target_lines  <- readLines(target_html)
    replacement_path <- paste0(modal_dir,"temp_file.html")
    replacement_lines <- readLines(replacement_path)

    # Next, let's figure out where the <head> section ends in each html file
    target_location <- where_is_head(target_lines)
    replacement_location <-where_is_head(replacement_lines)

    # Now, let's replace the <head> section and save the new version of the html
    output_file = c(replacement_lines[1:replacement_location$head_line],target_lines[(target_location$head_line+1):target_location$total_lines])
    write(output_file, file = target_html)

    # let's delete the temp html file that we created
    file.remove(paste(modal_dir, "temp_file.html", sep ="/"))
  }
}

#' Generate the html for rmd files with non-interactive figures
#'
#' The purpose of this function is to insert the glossary tooltips into the html
#' for a rmd file (not containing interactive figures, which are dealt with by another
#' function as they present special complications). The function works by converting
#' a rmd file to a markdown file, inserting the relevant tooltip tags and scripts
#' into that markdown file, and then creating a html file from that markdown file.
#'
#' @param nms The NMS sanctuary with only "cinms" currently doing anything.
#' @return The function outputs a html file for every rmd file not containing interactive figures.
#' @examples generate_html_4_noninteractive_rmd("cinms")
#'
generate_html_4_noninteractive_rmd <- function (nms){

  # Let's figure out where we are. In my local environment, I am in the directory for
  # the sanctuary. In a docker container though, I won't be. So the following section of
  # code attempts to put us in the right directory if we aren't there already.
  location <- here::here()
  start_point <- nchar(location) - nchar(nms) +1
  if (!(substr(location, start_point, nchar(location)) == nms)){
    location <- paste(location, nms, sep = "/")
  }
  modal_dir<- paste0(location,"/modals/")

  # let's get a list of all rmd files in the directory
  modal_list<-list.files(path = modal_dir, pattern = ".Rmd", ignore.case = TRUE)

  # Now, let's generate a list of rmd files that need to be skipped.
  if (nms == "cinms"){
    skip_rmd <- c("_key-climate-ocean.Rmd",
      "algal-groups.Rmd",
      "barnacles.Rmd",
      "deep-seafloor_key-climate-ocean.Rmd",
      "forage-assemblage.Rmd",
      "forage-fish.Rmd",
      "forage-inverts.Rmd",
      "kelp-forest_key-climate-ocean.Rmd",
      "key-climate-ocean.Rmd",
      "key-climate-ocean_seascape.Rmd",
      "mussels.Rmd",
      "ochre-stars.Rmd",
      "pelagic_key-climate-ocean.Rmd",
      "rocky-map.Rmd",
      "rocky-shore_key-climate-ocean.Rmd",
      "sandy-beach_key-climate-ocean.Rmd",
      "sandy-seafloor_key-climate-ocean.Rmd",
      "tar.Rmd")
    }
  # Let's create the final list of non=interactive rmd files to be rendered
  oceano_Rmds<-setdiff(modal_list, skip_rmd)
  oceano_Rmds<-paste0(modal_dir,oceano_Rmds)

  # let's render every rmd file to be worked on
  for (i in 1:length(oceano_Rmds)){
    rmd2html(oceano_Rmds[i])
  }
}

#' Get date range for an ERDDAP data set
#'
#' This function provides the first and last dates for which data is available for
#' an ERDDAP data set.
#' @param info A rerddap::info() object.
#' @return This function outputs a string vector, with the first element being the start date and the last element being the end date.
#' @export
#' @examples get_dates(rerddap::info('jplMURSST41mday'))
#'
get_dates <- function(info){
  info$alldata$time %>%
    dplyr::filter(attribute_name=="actual_range") %>%
    pull(value) %>%
    str_split(", ", simplify = T) %>%
    as.numeric() %>%
    as.POSIXct(origin = "1970-01-01", tz = "GMT")
}

#' Generate hyperlinked gray bar above figure
#'
#' The purpose of this function is to generate the hyperlinks for the monitoring program and data
#' associated with a figure and then to insert them into a gray bar above the figure in the modal window.
#'
#' @param figure_id The name of a row in the following google sheet cinms_content::info_figure_links
#' @return The output is a set of html tags to be inserted into a html file.
#' @export
#' @examples get_figure_info("Figure App.E.11.8.")

get_figure_info <- function (figure_id){

  info_csv = "https://docs.google.com/spreadsheets/d/1yEuI7BT9fJEcGAFNPM0mCq16nFsbn0b-bNirYPU5W8c/gviz/tq?tqx=out:csv&sheet=info_figure_links"

  d <- readr::read_csv(info_csv)  %>%
    dplyr::filter(md_caption == figure_id)

  if (nrow(d) == 0){
    warning(paste("Need link in cinms_content:info_figure_links Google Sheet for", figure_id))
    return("")
  }

  html  <- NULL
  no_ws <- c("before","after","outside","after-begin","before-end")

  icons <- tibble::tribble(
    ~description_bkup   ,    ~css,            ~icon,         ~fld_url, ~fld_description,
    "Monitoring Program",  "left", "clipboard-list", "url_monitoring", "title_monitoring",
    "Data"              , "right", "database"      ,       "url_data", "title_data")

  for (i in 1:nrow(icons)){  # i=1

    h           <- icons[i,]
    url         <- d[h$fld_url]
    description <- d[h$fld_description]

    if(!is.na(url) & substr(url,0,4) == "http"){
      if (is.na(description)){
        description <- h$description_bkup
      } else {
        description <- substr(stringr::str_trim(description), 0, 45)
      }

      html <- shiny::tagList(
        html,
        htmltools::div(
          .noWS = no_ws,
          style = glue("text-align:{h$css}; display:table-cell;"),
          a(
            .noWS = no_ws,
            href = url, target = '_blank',
            shiny::icon(h$icon), description)))
    }
  }

  if (is.null(html))
    return("")

  shiny::tagList(
    htmltools::div(
      .noWS = no_ws,
      style = "background:LightGrey; width:100%; display:table; font-size:120%; padding: 10px 10px 10px 10px; margin-bottom: 10px;",
      htmltools::div(
        .noWS = no_ws,
        style = "display:table-row",
        html)))
}

#' Generate introductory info for the html of a modal window
#'
#' This function generates the html tags for the top portion of a rmd modal window,
#' containing the introductory information about that window. This function only
#' will work within a rmd file to be knitted.
#'
#' @param rmd  The name of an input file passed to knit().
#' @param info_modal_links_csv A hyperlink to the google sheet, in csv format, that contains the modal links info.
#' @return The function returns a string that is a set of html tags to be inserted into a html file.
#' @export
#' @examples  get_modal_info()
#'
get_modal_info <- function(
  rmd = knitr::current_input(),
  info_modal_links_csv = "https://docs.google.com/spreadsheets/d/1yEuI7BT9fJEcGAFNPM0mCq16nFsbn0b-bNirYPU5W8c/gviz/tq?tqx=out:csv&sheet=info_modal_links"){

  modal_id <- basename(fs::path_ext_remove(rmd))
  row <- readr::read_csv(info_modal_links_csv) %>%
    dplyr::filter(modal == modal_id)

  if (nrow(row) == 0) stop("Need link in cinms_content:info_modal_links Google Sheet!")

  icons_html = NULL
  if (!is.na(row$url_info)){
    icons_html =
      htmltools::a(shiny::icon("info-circle"), href=row$url_info, target='_blank')
  }
  if (!is.na(row$url_photo)){
    icons_html = htmltools::tagList(
      icons_html,
      htmltools::a(shiny::icon("camera"), href=row$url_photo, target='_blank'))
  }

  htmltools::div(
    htmltools::div(htmltools::tagList(icons_html), style = "margin-top: 10px;margin-bottom: 10px; margin-right: 10px; flex: 1;"), htmltools::div(
      ifelse(!is.na(row$tagline), row$tagline, ""), style = "margin: 10px; font-style: italic; flex: 20; "), style="display: flex"

  )
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
#' @examples get_nms_polygons("cinms")
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

#' Insert html tags for glossary tooltips into md files
#'
#' The purpose of this function is to insert the html tags required for glossary
#' tooltip functionality into a given markdown file. The function glossarize_md
#' is used by the function rmd2html described in this package.
#'
#' @param md The markdown file where the tags are to be inserted.
#' @param md_out The markdown output file.
#' @return The output of this function is html tags inserted into a markdown file.
glossarize_md <- function(md, md_out = md){

  # read the markdown file
  tx  <- readLines(md)

  # only go forward with the glossarizing if the file contains more than "data to be added soon"
  if (length(tx) > 12) {

    # load in the glossary that will be used to create the tooltips.  Reverse alphabetize the glossary, which will come in handy later
    glossary_csv = "https://docs.google.com/spreadsheets/d/1yEuI7BT9fJEcGAFNPM0mCq16nFsbn0b-bNirYPU5W8c/gviz/tq?tqx=out:csv&sheet=glossary"
    glossary <- readr::read_csv(glossary_csv)
    glossary <- glossary[order(glossary$term, decreasing = TRUE),]

    # initialize the string variable that will hold the javascript tooltip
    script_tooltip = ""

    # go through each row of the glossary
    for (q in 1:nrow(glossary)) {

      # set a variable to zero that is used to keep track of whether a particular glossary word is in the modal window
      flag = 0

      # load in a specific glossary term
      search_term = glossary$term[q]

      # the css to be wrapped around any glossary word
      span_definition = paste0('<span aria-describedby="tooltip', q, '" tabindex="0" style="border-bottom: 1px dashed #000000; font-size:100%" id="tooltip', q, '">')

      # let's look to see if the glossary term is a subset of a longer glossary term (that is: "aragonite" and "aragonite saturation")
      # if it is a subset, we want to identify the longer term (so that we don't put the tooltip for the
      # shorter term with the longer term). Here is why the prior alphabetizing of the glossary matters
      glossary_match = glossary$term[startsWith (glossary$term, search_term)]

      if (length(glossary_match)>1){
        longer_term = glossary_match[1]
      }

      # let's go through every line of the markdown file looking for glossary words. We are skipping the first several
      # lines in order to avoid putting any tooltips in the modal window description
      for (i in 12:length(tx)) {

        # We want to avoid putting in tooltips in several situations that would cause the window to break.
        # 1. No tooltips on tabs (that is what the searching for "#" takes care of)
        # 2. No tooltips in the gray bar above the image (that is what the searching for the "</i>" and "</div> tags
        # take care of)
        # 3. No tooltips on lines where there is a link for a data download
        # 4. No tooltips on lines that create interactive graphs (no line starting with "<script")
        if (substr(tx[i],1,1) != "#" && str_sub(tx[i],-4) != "</i>" && str_sub(tx[i],-5) != "</div>" && substr(tx[i], 1, 24) != "Download timeseries data" && substr(tx[i], 1, 7) != "<script"){

          # We also want to avoid inserting tooltips into the path of the image file, which is what the following
          # image_start is looking for. If a line does contain an image path, we want to separate that from the rest of
          # the line, do a glossary word replace on the image-less line, and then - later in this code - paste the image back on to the line
          image_start = regexpr(pattern = "/img/cinms_cr", tx[i])[1] - 4

          if (image_start > 1) {
            line_content = substr(tx[i], 1, image_start)
            image_link = str_sub(tx[i], -(nchar(tx[i])-image_start))
          }
          else {
            line_content = tx[i]
          }

          # here is where we keep track of whether a glossary word shows up in the modal window - this will be used later
          if (grepl(pattern = search_term, x = line_content, ignore.case = TRUE) ==TRUE){
            flag = 1
          }

          # If the text contains a glossary term that is a shorter subset of another glossary term, we first
          # split the text by the longer glossary term and separately save the longer glossary terms (to preserve
          # the pattern of capitalization). We then run the split text through the tooltip function to add the required
          # span tags around the glossary terms and then paste the split text back together
          if (length(glossary_match)>1){

            split_text_longer <- stringr::str_split(line_content, regex(longer_term, ignore_case = TRUE))[[1]]
            save_glossary_terms_longer <- c(stringr::str_extract_all(line_content, regex(longer_term, ignore_case = TRUE))[[1]],"")

            for (s in 1:length(split_text_longer)){
              split_text_longer[s] <- insert_tooltip(split_text_longer[s], search_term, span_definition)
            }
            line_content<- paste0(split_text_longer, save_glossary_terms_longer, collapse="")
          }

          else {
            # In the case that the glossary term is not a shorter subset, life is much easier. We just run the line of content
            # through the insert tooltip function
            line_content <- insert_tooltip(line_content, search_term, span_definition)
          }

          # if we separated the image path, let's paste it back on
          if (image_start > 1) {
            tx[i] = paste0(line_content, image_link)
          }
          else {
            tx[i] = line_content
          }
        }
      }

      #if a glossary word was found in a modal window, let's add the javascript for that tooltip in
      if (flag == 1){
        script_tooltip = paste0(script_tooltip, '<script>tippy ("#tooltip', q, '",{content: "', glossary$definition[q], '"});</script>\r\n')
      }
    }

    # let's replace the markdown file with the modified version of the markdown file that contains all of the tooltip stuff
    # (if any)
    writeLines(tx, con=md_out)

    # if any glossary words are found, let's add in the javascript needed to make this all go
    if (script_tooltip != ""){
      load_script=' <script src="https://unpkg.com/@popperjs/core@2"></script><script src="https://unpkg.com/tippy.js@6"></script>\r\n'
      write(   load_script, file=md_out, append=TRUE)
      write(script_tooltip, file=md_out, append=TRUE)
    }
  }
}

#' Insert tooltips into text
#'
#' The purpose of the following function is, for a provided section of text, to
#' insert the required tooltip css around a provided glossary term. The function
#' preserves the pattern of capitalization of the glossary term that already exists.
#' This function is used by the function glossarize_md also described in this
#' package.
#'
#' @param text The section of text where tooltips are to be added.
#' @param glossary_term The glossary term to be looked for.
#' @param span_css The css tags to add before the glossary term.
#' @return The function outputs a string containing the text section with html tags inserted.
#'
insert_tooltip<- function(text, glossary_term, span_css){

  # We start by splitting the text by the glossary term and then separately saving the glossary terms. This is done
  # so that we can preserve the pattern of capitalization of the glossary term
  split_text <- stringr::str_split(text, regex(glossary_term, ignore_case = TRUE))[[1]]
  save_glossary_terms <- c(stringr::str_extract_all(text, regex(glossary_term, ignore_case = TRUE))[[1]],"")

  # Let's go through every section of the split text and add the required css tags
  for (q in 1:length(split_text)){
    if (q>1){
      split_text[q] = paste0("</span>", split_text[q])
    }

    if (q<length(split_text)){
      split_text[q] = paste0(split_text[q], span_css)
    }
  }

  # put the split text and the glossary terms back together again and then return that as the output
  return (paste0(split_text, save_glossary_terms, collapse=""))
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

#' Produce a map of where rocky intertidal data was collected
#'
#' This function produces an interactive map showing where rocky intertidal data
#' was collected by the MARINe consortium.
#'
#' @param nms The National Marine Sanctuary code.
#' @return This function returns a mapview object displaying data collection sites.
#' @export
#' @examples map_nms_sites("cinms")
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

#' Generate the caption for a figure
#'
#' This function generates either a short or expanded caption for a given figure.
#'
#' @param title The name of a figure.
#' @param md The md file containing the list of captions.
#' @param get_details A Boolean variable indicating whether a short or expanded caption is required.
#' @return A string containing the caption, with html tags inserted.
#' @export
#' @import tibble dplyr stringr glue tidyr
#' @examples md_caption("Figure Ux.Ocean.SST.ERD.map.", get_details = T)
#'
md_caption <- function(title, md = here::here("modals/_captions.md"), get_details = F){

  stopifnot(file.exists(md))

  tbl <- tibble::tibble(
    # read lines of markdown in _captions.md
    ln = readLines(md) %>% stringr::str_trim()) %>%
    # detect header with title, set rest to NA
    dplyr::mutate(
      is_hdr = stringr::str_detect(
        ln,
        glue::glue('^## {str_replace_all(title, fixed("."), "\\\\.")}'))
      %>% dplyr::na_if(FALSE)) %>%
    # fill down so capturing all starting with title header
    tidyr::fill(is_hdr) %>%
    # filter for title header down, removing previous lines
    dplyr::filter(is_hdr) %>%
    # remove title header
    dplyr::slice(-1) %>%
    # detect subsequent headers
    dplyr::mutate(
      is_hdr = stringr::str_detect(ln, "^## ") %>% dplyr::na_if(F)) %>%
    # fill down
    tidyr::fill(is_hdr) %>%
    dplyr::mutate(
      is_hdr = tidyr::replace_na(is_hdr, FALSE)) %>%
    # filter for not header down, removing subsequent lines outside caption
    dplyr::filter(!is_hdr) %>%
    # replace links in markdown with html to open in new tab
    dplyr::mutate(
      ln = stringr::str_replace_all(ln, "\\[(.*?)\\]\\((.*?)\\)", "<a href='\\2' target='_blank'>\\1</a>")) %>%
    # details
    mutate(
      is_details = stringr::str_detect(ln, "^### Details") %>% dplyr::na_if(F)) %>%
    # fill down
    tidyr::fill(is_details)

  simple_md <- tbl %>%
    dplyr::filter(is.na(is_details)) %>%
    dplyr::filter(ln != "") %>%
    dplyr::pull(ln) %>%
    paste0(collapse = "\n") %>%
    stringr::str_trim()

  # Remove spaces around figure title.
  title <- stringr::str_trim(title)

  # If the last character of the figure title is a period, delete it. This will
  # improve how the title looks when embedded into the text.
  if (substring(title, nchar(title))=="."){
    title<- substring(title,0,nchar(title)-1)
  }

  # Append figure title (like App.F.13.2) to the end of expanded figure caption and add link to condition report

  expanded_caption = paste('<details>\n  <summary>Click for Details</summary>\n\\1 For more information, consult', title,
                           'in the [CINMS 2016 Condition Report](https://nmssanctuaries.blob.core.windows.net/sanctuaries-prod/media/docs/2016-condition-report-channel-islands-nms.pdf){target="_blank"}.</details>')

  details_md <- tbl %>%
    dplyr::filter(is_details) %>%
    dplyr::filter(ln != "") %>%
    dplyr::pull(ln) %>%
    paste0(collapse = "\n") %>%
    stringr::str_replace("### Details\n(.*)", expanded_caption) %>%
    stringr::str_trim()

  if (get_details == T){
    return(details_md)
  } else {
    return(simple_md)
  }
}

#' Generate a plot of intertidal monitoring data
#'
#' This function generates a plot of time series data collected by the Rocky Intertidal
#' Monitoring program.
#'
#' @param d_csv A csv file containing the time series data.
#' @param NMS The National Marine Sanctuary code.
#' @param spp BEN
#' @param sp_name The species name to be plotted.
#' @param nms_rgns BEN
#' @param spp_targets BEN
#' @param fld_val The column of data to provide the y-axis to be plotted.
#' @param label_y The label for the y-axis for the plot.
#' @param label_x The label for the x-axis for the plot.
#' @param nms_skip_regions National Marine Sanctuaries to be skipped (using sanctuary codes).
#' @return This function returns a dygraph object of the plotted time series data.
#' @export
#' @import dplyr dygraphs glue lubridate magrittr mapview RColorBrewer readr tidyr
#'
#' @examples
#' nms <- "cinms"
#' d_csv <- "/Volumes/GoogleDrive/Shared drives/NMS/data/github_info-intertidal_data/sanctuary_species_percentcover.csv"
#' nms_rgns_csv    <- file.path(dir_pfx, "MARINe_graphs.xlsx - sites in regions.csv")
#' nms_rgns1 <- readr::read_csv(nms_rgns_csv) %>%
#'   tidyr::fill(nms) %>%
#'   dplyr::group_by(nms) %>%
#'   tidyr::fill(region) %>%
#'   dplyr::mutate(rgn = region)
#' plot_intertidal_nms(d_csv, "CINMS", "MYTCAL", "California Mussels", nms_rgns1)
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

  # line colors
  if (ncol(d) - 1 > 12){
    pal <- mapview::colorRampPalette(RColorBrewer::brewer.pal(12, "Set1"))
    ln_colors <- pal(ncol(d) - 1)
  } else {
    ln_colors <- RColorBrewer::brewer.pal(ncol(d) - 1, "Set3")
  }
  #filled.contour(volcano, col=ln_colors)
  ln_colors[which(names(d) == NMS) - 1] <- "black"

  # plot dygraph
  dygraphs::dygraph(
    #x,
    d,
    main = glue::glue("{sp_name} in {NMS}"),
    xlab = label_x,
    ylab = label_y) %>%
    dygraphs::dyOptions(
      connectSeparatedPoints = TRUE,
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
#' @examples
#' csv_SST <-here::here("data/oceano/statistics_sst_cinms.csv")
#' plot_metric_timeseries(csv_SST, "sst")
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
#' ply2erddap("cinms", "jplMURSST41mday", "sst", year = 2010, month = 6, c("mean", "sd"))
ply2erddap <- function (sanctuary_code, erddap_id, erddap_fld, year, month, stats) {

  # check inputs
  stopifnot(all(is.numeric(year), is.numeric(month)))

  # Get the polygons for the sanctuary.
  sanctuary_ply <-   sf::as_Spatial(sf::st_union(get_nms_polygons(sanctuary_code)))

  # set the x and y limits of the raster to be pulled based upon the sanctuary polygons
  bb <- sf::st_bbox(sanctuary_ply)

  # TODO: deal with wrapping around dateline
  # https://github.com/rstudio/leaflet/issues/225#issuecomment-347721709

  # The dates to be considered (note that dates are handled differently when pulling different datasets)
  # Additionally, the end date for the dataset nesdisVHNSQchlaMonthly needs to be set close to the beginning
  # date or multiple time slices of data will be called. I don't know why nesdisVHNSQchlaMonthly behaves this way.
    m_beg   <- lubridate::ymd(glue::glue("{year}-{month}-01"))
  if (erddap_id == "nesdisVHNSQchlaMonthly"){
    m_end   <- m_beg + 1
   } else{
    m_end   <- m_beg + lubridate::days(lubridate::days_in_month(m_beg)) - lubridate::days(1)
  }

  # set desired date range
  m_dates <- c(m_beg, m_end)

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
      #time = c("2013-01-01", "2013-06-01"),
      #time = "2013-04-01",
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
  } else { # if errdap_id calls any other dataset, stop everything as who knows how this other dataset is structured
    # stop("Error in erddap_id: this function only currently knows how to handle the datasets jplMURSST41mday and nesdisVHNSQchlaMonthly")
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
#' @import magrittr readr stringr

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

#' Produce full html for static figures, minus tooltips.
#'
#' This is a function that generates the html to display a static figure and the
#' captions for that figure. Glossary tooltips are not created here, as that
#' occurs at a later stage of the html production process.
#'
#' @param figure_id The id of the figure.
#' @param figure_img The path of the figure image.
#' @return The output is a string containing the html tags to display the figure and figure caption.
#' @export
#' @examples render_figure("Figure App.C.4.4.", "../img/cinms_cr/App.C.4.4.Leeworthy_landings.jpg")
#'
render_figure <- function(figure_id, figure_img){
  glue::glue(
    "
  {nms4r::get_figure_info(figure_id)}

  ![{nms4r::md_caption(figure_id)}]({figure_img})

  {nms4r::md_caption(figure_id, get_details=T)}
  ")
}

#' Render html for all R Markdown files in modal directory
#'
#' This function generates the full html, including tooltips, for all R markdown
#' files in the modal directory.
#'
#' @param NMS the NMS sanctuary
#'
#' @return This function outputs html files for most rmd files in the modal directory.
#' @export
#' @examples render_modal_windows("cinms")
#'
render_modal_windows <- function (NMS) {
  generate_html_4_noninteractive_rmd(NMS)
  generate_html_4_interactive_rmd(NMS)
}

#' Render html for rmd file, including glossary tooltips.
#'
#' This function creates all of the html for a R markdown file, inserting in the
#' glossary tooltips.
#'
#' @param rmd The R markdown file to be rendered into html.
#' @return The output is a html file that is the rendered rmd file.
#' @export
#' @examples rmd2html(here::here("modals/ca-sheephead.Rmd"))
rmd2html <- function(rmd){

  md1  <- fs::path_ext_set(rmd, "md")
  md2  <- paste0(fs::path_ext_remove(rmd), ".glossarized.md")
  htm1 <- paste0(fs::path_ext_remove(rmd), ".glossarized.html")
  htm2 <- fs::path_ext_set(rmd, "html")

  # create the intermediary markdown file (with disposable html)
  rmarkdown::render(
    rmd, output_file = htm1,
    output_format    = "html_document",
    output_options   = list(self_contained = F, keep_md = T))

  # glossarize
  glossarize_md(md2, md2)

  # create the final html file
  rmarkdown::render(
    md2, output_file = htm2,
    output_format    = "html_document",
    output_options   = list(self_contained = F), clean = F)

  # final cleanup
  file.remove(htm1)
  file.remove(md2)
  file.remove(paste0(substring(md2,1,str_length(md2)-3),".utf8.md"))
}
