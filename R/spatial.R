
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
#'
#' @return nothing
#' @export
#' @import here rerddap
#'
#' @examples
#' calculate_statistics("cinms", "jplMURSST41mday", "sst", "avg-sst_cinms.csv")
#' calculate_statistics("cinms", "nesdisVHNSQchlaMonthly", "chlor_a", "avg-chl_cinms.csv")
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

#' This function generates the html for rmd files with interactive figures.
#'
#' @param nms the NMS sanctuary
#'
#' @return nothing
#' @export
#' @import here rmarkdown
#'
generate_html_4_interactive_rmd <- function (nms){
  # The purpose of generate_html is to create the html for rmd files with interactive figures.

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
  modal_list<-list.files(path = modal_dir)

  # Now, let's generate a list of rmd files that need to be worked on.

  # Step 1. find Rmd files that have _key-climate-ocean.Rmd in them
  keep_modals<-grep("key-climate-ocean.Rmd",modal_list, ignore.case = TRUE)

  # Step 2.  find the Rmd files that is ONLY _key-climate-ocean.Rmd (which we want to ignore)
  throw_out_modal<-grep("^_key-climate-ocean.Rmd$",modal_list, ignore.case = TRUE)

  # Step 3. create list of Rmds that we want to render and append full path to those file names
  oceano_Rmds<-modal_list[keep_modals[!(keep_modals==throw_out_modal)]]
  oceano_Rmds<-paste0(modal_dir,oceano_Rmds)

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

#' get_box
#'
#' @param lon
#' @param lat
#' @param cells_wide
#'
#' @return
#' @export
#'
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
get_dates <- function(info){
  info$alldata$time %>%
    filter(attribute_name=="actual_range") %>%
    pull(value) %>%
    str_split(", ", simplify = T) %>%
    as.numeric() %>%
    as.POSIXct(origin = "1970-01-01", tz = "GMT")
}

#' get_modal_info
#'
#' @param rmd
#' @param info_modal_links_csv
#'
#' @return
#' @export
#' @import knitr fs readr
#'
get_modal_info <- function(
  rmd = knitr::current_input(),
  info_modal_links_csv = "https://docs.google.com/spreadsheets/d/1yEuI7BT9fJEcGAFNPM0mCq16nFsbn0b-bNirYPU5W8c/gviz/tq?tqx=out:csv&sheet=info_modal_links"){

  # rmd = "infauna.Rmd"
  # rmd = "key-human-activities.Rmd"
  modal_id <- basename(fs::path_ext_remove(rmd))

  #message(glue("modal_id: {modal_id}"))

  # modal_id = "ochre-stars"
  row <- readr::read_csv(info_modal_links_csv) %>%
    filter(modal == modal_id)

  if (nrow(row) == 0) stop("Need link in cinms_content:info_modal_links Google Sheet!")

  icons_html = NULL
  if (!is.na(row$url_info)){
    icons_html =
      a(icon("info-circle"), href=row$url_info, target='_blank')
  }
  if (!is.na(row$url_photo)){
    icons_html = tagList(
      icons_html,
      a(icon("camera"), href=row$url_photo, target='_blank'))
  }

  div(
    div(tagList(icons_html), style = "margin-top: 10px;margin-bottom: 10px; margin-right: 10px; flex: 1;"), div(
      ifelse(!is.na(row$tagline), row$tagline, ""), style = "margin: 10px; font-style: italic; flex: 20; "), style="display: flex"

  )
}

#' Get NMS polygons
#'
#' given NMS code (see
#' \url{https://sanctuaries.noaa.gov/library/imast_gis.html}), download and
#' extract zip, cache shapefile or read existing shapefile
#'
#' @param nms code for national marine sanctuary
#'
#' @return sf object
#' @export
#'
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
get_raster <- function(info, lon, lat, date="last", field="sst"){
  g <- griddap(
    info, longitude = lon, latitude = lat,
    time = c(date, date), fields = field)
  grid_to_raster(g, "sst") %>%
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

#' Insert html tags for glossary tooltips into md files
#'
#' The purpose of this function is to insert the html tags required for glossary tooltip functionality into
#' a given md file
#'
#' @param md the md file where the tags are to be inserted
#' @param md_out the md output file
#'
#' @return nothing
#' @export
#' @import readr stringr
#'
#' @examples
glossarize_md <- function(md, md_out = md){
  # The purpose of this function is to insert the html tags required for  glossary tooltip functionality into
  # a given md file

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
        if (substr(tx[i],1,1) != "#" && str_sub(tx[i],-4) != "</i>" && str_sub(tx[i],-5) != "</div>" && substr(tx[i], 1, 24) != "Download timeseries data"){

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

#' Insert tooltips into text
#'
#' Draft version of code to render modal windows with tooltips. The overall idea is to generate a markdown file from a
#' given modal rmd file. Within that markdown file, we then insert the javascript package tippy as well as inserting the
#' specific tippy tooltip. We then generate a html file for the modal window from the modified markdown file and then
#' delete the markdown file

#' The purpose of the following function is, for a provided section of text, to insert the required tooltip css around a
#' provided glossary term. The function preserves the pattern of capitalization of the glossary term that already exists.
#' The function requires three parameters: 1) text: the section of text where we are looking to add tooltips, 2)
#' glossary_term: the glossary term that we are looking for, 3) span_css: the css tags to add before the glossary term

#' @param text the section of text where we are looking to add tooltips
#' @param glossary_term the glossary term that we are looking for
#' @param span_css the css tags to add before the glossary term
#'
#' @return paste0(split_text, save_glossary_terms, collapse="")
#' @export
#' @import stringr
#'
#' @examples
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

#' md_caption
#'
#' @param title
#' @param md
#' @param get_details
#'
#' @return
#' @export
#' @import tibble dplyr stringr glue tidyr
#'
#' @examples
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

  # If the last character of the figure title is a period, delete it. This will improve how the title looks when embedded into the text.

  if (substring(title, nchar(title))=="."){
    title<- substring(title,0,nchar(title)-1)
  }

  # Append figure title (like App.F.13.2) to the end of expanded figure caption and add link to condition report

  expanded_caption = paste('<details>\n  <summary>Click for Details</summary>\n\\1 For more information, consult', title,
                           'in the [CINMS 2016 Condition Report](https://nmssanctuaries.blob.core.windows.net/sanctuaries-prod/media/docs/2016-condition-report-channel-islands-nms.pdf){target="_blank"}.</details>')

  details_md <- dplyr::tbl %>%
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

#' plot_metric_timeseries
#'
#' @param csv
#' @param metric
#' @param ... additional parameters to pass to \link[dygraphs]{dygraph}
#'
#' @return
#' @export
#'
#' @examples
plot_metric_timeseries <- function(csv, metric, ...){
  # The purpose of this function is to generate figures showing the sea surface temperature time series
  # for a Sanctuary (displaying both avg and standard deviation of temp). The function has two parameters: 1)
  # csv: which is the path name for the csv data file to be plotted and 2) metric: which is the type of data
  # to be plotted; currently only "sst" (for sea surface temperature) and "chl" (for chlorophyll) are recognized

  # Read in the csv file
  data_history <- read.csv(csv, header = TRUE)
  dates<- data_history[,1]
  average_value <- data_history[,2]
  lower_value <- data_history[,5]
  upper_value <- data_history[,6]

  # create a data frame which lines up the data in the way that dygraph needs it
  # history <- data.frame(date = as.Date(dates, "%Y-%m-%d"), avg_value = average_value, lower = average_value - standard_deviation, upper = average_value + standard_deviation)
  history <- data.frame(date = as.Date(dates, "%Y-%m-%d"), avg_value = average_value, lower = lower_value, upper = upper_value)
  history <- xts(x = history[,-1], order.by = history$date)

  # create the figure
  if (metric == "sst"){ # plotting sea surface temperature
    dygraph(
      history,
      main = "Sea Surface Temperature",
      xlab = "Date", ylab = "Temperature (°C)",
      ...)%>%
      dySeries(c("lower", "avg_value", "upper"), label = "Temperature (°C)", color = "Red")%>%
      dyRangeSelector()
  } else if (metric == "chl") { # plotting chlorophyll
    dygraph(
      history,
      main = "Chlorophyll Concentration",
      xlab = "Date", ylab = "Chlorophyll Concentration, OC3 Algorithm (mg/m<sup>3</sup>)",
      ...)%>%
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

#' Extract ERDDAP statistics from polygon by year-month
#'
#' Extract satellite data in an ERDDAP dataset from national marine sanctuary polygon for a given year and month.
#'
#' @param sanctuary_code sanctuary code based on prefix to \href{https://sanctuaries.noaa.gov/library/imast_gis.html}{Sanctuary GIS files}, e.g. \code{"cinms"} for Channel Islands Marine Sanctuary
#' @param erddap_id dataset ID of ERDDAP dataset (see \href{https://coastwatch.pfeg.noaa.gov/erddap/index.html}{coastwatch.pfeg.noaa.gov/erddap}), e.g. \code{"jplMURSST41mday"} for Multi-scale Ultra-high Resolution SST Analysis
#' @param erddap_fld variable of ERDDAP dataset to extract, e.g. \code{"sst"}
#' @param year 4-digit year
#' @param month integer month (1-12)
#' @stats statistics to be calculated
#'
#' @return a list of values by statistic
#'
#' @export
#' @import glue sf magrittr dplyr here rerddap
#'
#' @examples
#'
ply2erddap <- function (sanctuary_code, erddap_id, erddap_fld, year, month, stats) {
  # sanctuary_code = "cinms"; erddap_id = "jplMURSST41mday"; erddap_fld = "sst"; year = 2010; month = 6; stats = c("mean", "sd")

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

#' Render html for rmd files, including glossary tooltips
#'
#' @param rmd the rmd file to be rendered into html
#'
#' @return nothing
#' @export
#' @import fs markdown
#'
#' @examples
rmd2html <- function(rmd){

  md1  <- fs::path_ext_set(rmd, "md")
  md2  <- paste0(fs::path_ext_remove(rmd), ".glossarized.md")
  htm1 <- paste0(fs::path_ext_remove(rmd), ".glossarized.html")
  htm2 <- fs::path_ext_set(rmd, "html")

  # create the intermediary markdown file (with disposable html)
  markdown::render(
    rmd, output_file = htm1,
    output_format    = "html_document",
    output_options   = list(self_contained = F, keep_md = T))

  # glossarize
  glossarize_md(md2, md2)

  # create the final html file
  markdown::render(
    md2, output_file = htm2,
    output_format    = "html_document",
    output_options   = list(self_contained = F), clean = F)

  # final cleanup
  file.remove(htm1)
  file.remove(md2)
  file.remove(paste0(substring(md2,1,str_length(md2)-3),".utf8.md"))
}
