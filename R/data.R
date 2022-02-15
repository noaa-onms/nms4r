#' Rocky intertidal site clusters
#'
#' A simple table clustering sites per National Marine Sanctuary,
#' such as by island.
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
#' A simple features table of MARINe intertidal sites associated
#' with a National Marine Sanctuary.
#'
#' @format A simple features tibble with:
#' \describe{
#'   \item{nms}{National Marine Sanctuary acronym, e.g. CINMS}
#'   \item{cluster}{group to name cluster of sites, e.g. Santa Cruz Island}
#'   \item{site}{site}
#'   \item{lat}{latitude}
#'   \item{lon}{longitude}
#' }
#' @source \url{https://marine.ucsc.edu/sites/sites-region/index.html}
"rocky_sites"

#' Rocky intertidal percent cover data
#'
#' A table of MARINe intertidal species percent cover summarized annually
#' across sanctuaries, clusters (including "ALL"), sites (including "ALL"),
#' `sp_target` (including "ALL") and species code `sp_code`.
#'
#' @format A simple features tibble with:
#' \describe{
#'   \item{nms}{National Marine Sanctuary acronym, e.g. CINMS}
#'   \item{cluster}{grouping of sites, e.g. Santa Cruz Island}
#'   \item{site}{MARINe site}
#'   \item{date}{annual date YYYY-06-15}
#'   \item{sp_target}{MARINe target assemblage}
#'   \item{sp_code}{MARINe species code}
#'   \item{pct_cover}{percent cover}
#' }
#' @source \url{https://marine.ucsc.edu/sites/sites-region/index.html}
"rocky_cover"

#' Rocky intertidal species counts
#'
#' A table of MARINe intertidal average species counts summarized annually
#' across sanctuaries, clusters (including "ALL"), sites (including "ALL"),
#' sp_method (including "ALL") and species code `sp_code`. All data has sp_target (or
#' target_assemblage) of "sea_star".
#'
#' @format A simple features tibble with:
#' \describe{
#'   \item{nms}{National Marine Sanctuary acronym, e.g. CINMS}
#'   \item{cluster}{grouping of sites, e.g. Santa Cruz Island}
#'   \item{site}{MARINe site}
#'   \item{date}{annual date YYYY-06-15}
#'   \item{sp_method}{MARINe method used for sampling the plot:
#'     IP = Irregular Plot; BT25 = Band Transect 2m x 5m;
#'     GSES = General search entire site; TS30 = Timed Search 30 Minute.}
#'   \item{sp_code}{MARINe species code}
#'   \item{count}{average count}
#' }
#' @source \url{https://marine.ucsc.edu/sites/sites-region/index.html}
"rocky_counts"

#' Rocky species
#'
#' A table of MARINe species with `sp_code` to relate to [rocky_cover]
#' and [rocky_counts].
#'
#' @source \url{https://marine.ucsc.edu/sites/sites-region/index.html}
"rocky_species"

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
