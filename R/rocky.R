#' Map rocky intertidal sites where data was collected
#'
#' This function produces an interactive map showing where rocky intertidal data
#' was collected by the MARINe consortium.
#'
#' @param nms The National Marine Sanctuary code.
#' @return This function returns a mapview object displaying data collection sites.
#' @export
#' @examples \dontrun{
#' rocky_map("cinms")
#' }
#'
rocky_map <- function(nms){
  # nms = "ocnms"
  NMS = toupper(nms)

  nms_ply  <- sanctuaries %>%
    dplyr::filter(nms == NMS) %>%
    dplyr::select(sanctuary)

  site_pts <- rocky_sites %>%
    dplyr::filter(nms == NMS)

  mapview::mapview(
    nms_ply, legend = TRUE,
    layer.name = "Sanctuary", zcol = "sanctuary") +
    mapview::mapview(
      site_pts, legend = TRUE,
      layer.name = "Site", zcol = "site",
      col.regions = colorRampPalette(RColorBrewer::brewer.pal(11, "Set3")))
}

#' Plot time series of rocky intertidal monitoring data
#'
#' This function generates a plot of time series data collected by the Rocky Intertidal
#' Monitoring program.
#'
#' @param data Either [rocky_data] or [rocky_counts].
#' @param nms The National Marine Sanctuary code, e.g. "CINMS", from [sanctuaries].
#' @param sp_code The species code of interest as referenced in [rocky_data] or [rocky_counts] and described in [rocky_species].
#' @param sp_name The species name used in the title of the plot.
#' @param sp_filter The filter value to apply for [rocky_cover] the `sp_target`
#'                  and for [rocky_counts] the `sp_method`. Defaults to "ALL".
#' @return This function returns a dygraph object of the plotted time series data.
#' @export
#' @import dplyr dygraphs glue lubridate magrittr mapview RColorBrewer readr tidyr
#'
#' @examples \dontrun{
#' rocky_tsplot(rocky_cover, "OCNMS", "CHTBAL", "Acorn Barnacles")
#' rocky_tsplot(rocky_cover, "CINMS", "CHTBAL", "Acorn Barnacles")
#' rocky_tsplot(rocky_counts, "OCNMS", "PISOCH", "Ochre Seastar")
#' }
rocky_tsplot <- function(
  data, nms, sp_code, sp_name, sp_filter = "ALL"){

  NMS       <- toupper(nms)
  sanctuary <- sanctuaries %>%
    filter(nms == !!NMS) %>%
    pull(sanctuary)

  if ("pct_cover" %in% names(data)){
    fld_val    <- "pct_cover"
    fld_filter <- "sp_target"
    label_y    <- "% Cover (annual avg)"
  } else {
    fld_val    <- "count"
    fld_filter <- "sp_method"
    label_y    <- "Count (annual avg)"
  }

  d <- data %>%
    dplyr::filter(
      nms         == !!NMS,
      sp_code   %in% !!sp_code,
      !!as.symbol(fld_filter) %in% !!sp_filter) %>%
    dplyr::rename(v = !!fld_val)

  if (NMS %in% rocky_clusters$nms){
    fld_grp <- "cluster"
    d <- d %>%
      filter(site == "ALL")
  } else {
    d <- d %>%
      filter(
        is.na(cluster))
    fld_grp <- "site"
  }

  # avg by year and spread
  x <- d %>%
    dplyr::rename(grp= !!fld_grp) %>%
    dplyr::select(year, grp, v) %>%
    tidyr::spread(grp, v) # View(x)

  # line widths and colors
  n_lns <- ncol(x) - 1
  ln_widths <- rep(2, n_lns)
  if (n_lns > 11){
    ln_colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_lns)
  } else {
    ln_colors <- RColorBrewer::brewer.pal(n_lns , "Set2")
  }
  i_all <- which(names(x) == "ALL") - 1
  ln_colors[i_all] <- "black"
  ln_widths[i_all] <- 4

  # plot dygraph
  dygraphs::dygraph(
    x,
    main = glue::glue("{sp_name} in {sanctuary}"),
    xlab = "Year",
    ylab = label_y) %>%
    dygraphs::dyOptions(
      connectSeparatedPoints = FALSE,
      colors      = ln_colors,
      fillAlpha   = 0.8,
      strokeWidth = 2) %>%
    dygraphs::dySeries(
      "ALL",
      #color       = "black",
      strokeWidth = 4) %>%
    dygraphs::dyHighlight(
      highlightSeriesOpts = list(strokeWidth = 6)) %>%
    dygraphs::dyRangeSelector(
      fillColor = " #FFFFFF",
      strokeColor = "#FFFFFF")
}
