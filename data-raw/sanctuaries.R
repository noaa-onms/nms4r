librarian::shelf(
  dplyr, fs, glue, here, purrr,sf, stringr, rvest)

# variables
dir_raw <- here("data-raw/sanctuary_polygons")

# web scrape table with sanctuary names and shapefile zip links
url <- "https://sanctuaries.noaa.gov/library/imast_gis.html"
rows = read_html(url) %>%
  html_nodes(xpath = "/html/body/div/div/article/section/div/table") %>%
  html_nodes(xpath = "tbody/tr")

# read in shapefiles and make overview table
dir_tmp <- tempdir()
d <- tibble(i = 1:length(rows))
for (i in 13:length(rows)){ # i = 1
  cells     <- html_nodes(rows[[i]], xpath = "td")
  sanctuary <- cells[[1]] %>% html_text() %>%
    str_replace(" Boundary Polygon", "") %>% str_replace(" Polygon", "")
  z <- cells[[5]] %>% html_nodes("a") %>% html_attr("href")
  zip_url <- ifelse(
    str_detect(z, "^/"),
    glue("https://sanctuaries.noaa.gov{z}"),
    glue("{dirname(url)}/{z}"))

  zip <- file.path(dir_tmp, basename(zip_url))
  download.file(zip_url, zip)
  f_shp <- unzip(zip, list = T) %>%
    filter(str_detect(Name, "shp$")) %>%
    filter(str_detect(Name, "^__", negate=T)) %>%
    filter(str_detect(Name, "Albers", negate=T)) %>%  # PMNM_py_Albers.shp
    pull(Name)
  unzip(zip, exdir = dir_tmp) # list.files(dir_tmp, recursive=T)
  ply_shp <- file.path(dir_tmp, f_shp)
  ply <- sf::read_sf(ply_shp) %>%
    sf::st_transform(4326)
  names(ply) <- tolower(names(ply))
  nms <- str_replace(f_shp, "_py.*\\.shp", "") %>% toupper()
  ply_json <- glue("{dir_raw}/{nms}.geojson")
  write_sf(ply, ply_json)

  d$sanctuary[i] <- sanctuary
  d$nms[i]       <- nms
  d$geojson[i]   <- ply_json

  unlink(dirname(ply_shp))
  unlink(zip)
}

# read individual geometries and original tables
sanctuaries <- d %>%
  mutate(
    spatial = map(geojson, read_sf),
    geojson = str_replace(geojson, here(), "")) %>%
  select(sanctuary, nms, spatial)

# create single geometry and make table a spatial feature
sanctuaries <- onmsR::sanctuaries %>%
  mutate(
    g = map(spatial, function(x){
      st_union(x) %>%
        st_cast("MULTIPOLYGON") }))
sanctuaries$geom <- st_sfc( sapply( sanctuaries$g, `[`) )
sanctuaries <- sanctuaries %>% select(-g) %>% st_as_sf(crs=4326)

sanctuaries %>%
  select(-spatial) %>%
  sf::st_drop_geometry() %>%
  write_csv(here("data-raw/sanctuaries.csv"))
usethis::use_data(sanctuaries, overwrite = T)
