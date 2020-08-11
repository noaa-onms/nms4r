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
get_nms_ply <- function(nms, dir_pfx){
  library(glue)
  library(here)
  library(sf)

  nms_shp <- glue("{dir_shp}/{nms}_py.shp")

  if (!file.exists(nms_shp)){
    # download if needed

    # https://sanctuaries.noaa.gov/library/imast_gis.html
    nms_url <- glue("https://sanctuaries.noaa.gov/library/imast/{nms}_py2.zip")
    nms_zip <- tempfile(fileext = ".zip")

    download.file(nms_url, nms_zip)
    unzip(nms_zip, exdir = dir_shp)
    unlink(nms_zip)
  }
  # read and convert to standard geographic projection
  read_sf(nms_shp) %>%
    st_transform(4326)
}
