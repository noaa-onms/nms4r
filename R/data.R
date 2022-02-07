#' Rocky intertidal site Sanctuary groupings
#'
#' A simple table describing groupings per National Marine Sanctuary.
#'
#' @format A tibble with the following columns:
#' \describe{
#'   \item{nms}{National Marine Sanctuary acronym, e.g. CINMS}
#'   \item{cluster}{group to name cluster of sites, e.g. Santa Cruz Island}
#'   \item{site}{site in rocky_sanctuary_data}
#' }
#' @source \url{https://marine.ucsc.edu/sites/sites-region/index.html}
"rocky_clusters"

#' Rocky intertidal sites
#'
#' A simple features table of MARINe intertidal sites associated with a National Marine Sanctuary.
#'
#' @format A simple features tibble with:
#' \describe{
#'   \item{site}{site}
#'   \item{lat}{latitude}
#'   \item{lon}{longitude}
#'   \item{nms}{National Marine Sanctuary acronym, e.g. CINMS}
#'   \item{cluster}{group to name cluster of sites, e.g. Santa Cruz Island}
#' }
#' @source \url{https://marine.ucsc.edu/sites/sites-region/index.html}
"rocky_sites"

#' Rocky intertidal percent cover data
#'
#' A big table of MARINe intertidal percent cover data summarized annually
#' across sanctuaries, clusters (including "ALL"), sites (including "ALL"),
#' target_assemblage (including "ALL") and species code.
#'
#' @format A simple features tibble with:
#' \describe{
#'   \item{nms}{National Marine Sanctuary acronym, e.g. CINMS}
#'   \item{cluster}{grouping of sites, e.g. Santa Cruz Island}
#'   \item{site}{MARINe site}
#'   \item{date}{annual date YYYY-06-15}
#'   \item{target_assemblage}{MARINe target assemblage}
#'   \item{sp_code}{MARINe species code}
#'   \item{pct_cover}{percent cover}
#' }
#' @source \url{https://marine.ucsc.edu/sites/sites-region/index.html}
"rocky_cover"

#' Sanctuaries with spatial information
#'
#' A spatial features table with sanctuary name and acronym. The default geometry is the unioned set of features found inside the spatial cell.
#'
#' @format A tibble with the following columns:
#' \describe{
#'   \item{sanctuary}{name of National Marine Sanctuary, e.g. Channel Islands}
#'   \item{nms}{national marine sanctuary acronym, e.g. CINMS}
#'   \item{spatial}{a list column of a spatial feature set of individual polygons comprising the sanctuary}
#' }
#' @source \url{https://sanctuaries.noaa.gov/library/imast_gis.html}
"sanctuaries"
