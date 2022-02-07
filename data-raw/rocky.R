# TODO:
# - update and operationalize download from [MARINe_meta - Google Sheets](https://docs.google.com/spreadsheets/d/1XSCfBdIN0r6pWtSF5-TgscdtxrOmYLaJUrk02cwhlIo/edit#gid=643317542)
# - finish rocky_counts a la rocky_cover, but something to do with `method_code` field?
#   See: old [cinms/rocky.R · noaa-onms/cinms](https://github.com/noaa-onms/cinms/blob/28df6cdb65f4c7439a06f5f3496c0baac1747fe0/scripts/rocky.R)
librarian::shelf(
  dplyr, here, readr, sf)
devtools::load_all()

# on Ben's Mac: 442.1 MB Dec 6, 2021; update these paths with new MARINe data and re-run
pctcover_csv    <- "/Users/bbest/My Drive (ben@ecoquants.com)/projects/nms-web/data/MARINe_Pacific-rocky-intertidal/egnyte_2021-12-06/phototranraw_download_all_20211206.csv"
pctcover_source <- "csv"
counts_csv      <- "/Users/bbest/My Drive (ben@ecoquants.com)/projects/nms-web/data/MARINe_Pacific-rocky-intertidal/egnyte_2021-12-06/seastarkat_size_count_zeroes_totals_download_all_20211206.csv"

# data-raw files
rocky_clusters_csv <- here("data-raw/rocky_clusters.csv")
rocky_sites_geo    <- here("data-raw/rocky_sites.geojson")

# rocky_clusters ----
rocky_clusters <- rocky_clusters_csv %>%
  readr::read_csv() %>%
  tidyr::fill(nms) %>%
  dplyr::group_by(nms) %>%
  tidyr::fill(cluster)
usethis::use_data(rocky_clusters, overwrite = TRUE)

# rocky_sites ----

# read pctcover_csv to table
if (pctcover_source == "erddap"){
  pctcover <- read_csv_fmt(pctcover_csv, pctcover_fmt)
} else {
  pctcover <- read_csv(pctcover_csv)
}

# extract all unique sites from pctcover as spatial points
site_pts <- pctcover %>%
  dplyr::rename(
    site = marine_site_name) %>%
  filter(
    !is.na(longitude),
    !is.na(latitude)) %>%
  dplyr::group_by(site) %>%
  dplyr::summarize(
    lat = dplyr::first(latitude),
    lon = dplyr::first(longitude)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)
message(glue("Found {nrow(site_pts)} sites"))
# Found 120 sites

# assign site sanctuary based on buffered intersection
sanctuaries_buf <- st_buffer(sanctuaries, 0.01) # 0.01 dd ≈ 1.1 km
x <- st_intersects(
  site_pts,
  sanctuaries_buf)
z <- tibble(x) %>%
  mutate(
    n = map_int(x, length),
    i = map_int(x, function(x){
      if (length(x) == 1)
        return(x[[1]])
      if (length(x) > 1)
        stop("whoah! more than one sanctuary for a site?")
      return(NA) }),
    nms = map_chr(i, function(i){
      sanctuaries_buf$nms[i] }))
site_pts$nms_buf <- z$nms

table(z$nms)
# CINMS GFNMS MBNMS OCNMS
#    21    10    20     6

table(rocky_clusters$nms)
# CINMS MBNMS
#    26    33

# assign sanctuary also on membership of site in rocky_clusters
site_pts <- site_pts %>%
  left_join(
    rocky_clusters,
    by="site") %>%
  mutate(
    nms = ifelse(
      is.na(nms),
      nms_buf,
      nms)) %>%
  select(-nms_buf) %>%
  filter(!is.na(nms))
message(glue("Found {nrow(site_pts)} sites within 0.01º of a sanctuary or in rocky_clusters"))

write_sf(site_pts, rocky_sites_geo, delete_dsn=T)
rocky_sites <- read_sf(rocky_sites_geo)
usethis::use_data(rocky_sites, overwrite = TRUE)

# rocky_cover ----
d <- pctcover %>%
  select(
    site = marine_site_name,
    date = survey_date,
    target_assemblage,
    sp_code = lumping_code,
    percent_cover,
    last_updated) %>%
  left_join(
    rocky_sites,
    by = c("site"))
nrow(d) # 2,145,274
d <- d %>%
  filter(!is.na(nms))
nrow(d) # 883,654

table(d$sp_code)
# ANTELE ARTCOR BARNAC CHITON CHOCAN CHTBAL CLACOL CRUCOR EGRMEN
#  22193  22246   1011  21106  21112  21254  21141  22238  22144
# EISARB ENDMUR ENPEPH FUCGAR HALSTE HEDSES HESCAL LIMPET LOTGIG
#  13588  21278  21099  10244  22098    298  21111  21234  21110
# MASSPP MAZAFF MAZSPP MYTCAL MYTTRO NEOLAR NONCRU OTHALG OTHBAR
#  21136  21177  10710  22278   3253   9683  22273  10798  18048
# OTHBRO OTHGRE OTHINV OTHRED OTHSUB PELLIM PHRSAB PHYSPP PISOCH
#  22143  22100  22144  22186  12352  10848  22170  22116  21135
# POLPOL PORSPP   ROCK   SAND SARMUT SCYSPP SEMCAR SEPBRA SILCOM
#  21153  21148  22438  22149  21642  21098   9487  15944  21143
#    TAR TETRUB ULVENT ZOSMAR
#  22030  21155  21149     63
table(d$target_assemblage)
#      balanus chthamalus_balanus         endocladia
#        41723             178298             137551
#        fucus      hesperophycus        mastocarpus
#        20133              44101              44930
#    mazzaella            mytilus       neorhodomela
#         5776             200105               2700
# pelvetiopsis       phyllospadix         pollicipes
#        24181              20431               4683
#     recovery          red_algae               rock
#         1872               2225               5957
#     silvetia                tar         tetraclita
#       131546               4663              12779
table(d$target_assemblage, d$sp_code)

# average percent_cover
d_dates <- bind_rows(
  # with target: average across sanctuary-group-site-date-target-species
  d %>%
  group_by(nms, cluster, site, date, target_assemblage, sp_code) %>%
  summarize(
    pct_cover = mean(percent_cover),
    .groups = "drop"),
  # withOUT target: average across sanctuary-group-site-date-species
  d %>%
    group_by(nms, cluster, site, date, target_assemblage, sp_code) %>%
    summarize(
      pct_cover = mean(percent_cover),
      .groups = "drop") %>%
    mutate(
      target_assemblage = "ALL"))

# average across sites to sanctuary-group-site-year
d_sites <- d_dates %>%
  mutate(
    date = date(glue("{year(date)}-06-15"))) %>%
  group_by(nms, cluster, site, date, target_assemblage, sp_code) %>%
  summarize(
    pct_cover = mean(pct_cover),
    .groups = "drop") # View(d_nms)

# average across sites to sanctuary-group-year
d_clusters <- d_dates %>%
  mutate(
    date = date(glue("{year(date)}-06-15"))) %>%
  group_by(nms, cluster, date, target_assemblage, sp_code) %>%
  summarize(
    pct_cover = mean(pct_cover),
    .groups = "drop") %>%
  mutate(
    site = "ALL") # View(d_clusters)

# average across sites to sanctuary-year
d_sanctuaries <- d_dates %>%
  mutate(
    date = date(glue("{year(date)}-06-15"))) %>%
  group_by(nms, date, target_assemblage, sp_code) %>%
  summarize(
    pct_cover = mean(pct_cover),
    .groups = "drop") %>%
  mutate(
    cluster = "ALL",
    site  = "ALL") # View(d_sanctuaries)

# combine all dates, and annual averages of sanctuary and cluster
rocky_cover <- bind_rows(
  d_sites,
  d_clusters,
  d_sanctuaries) # View(d)

# rocky_cover <- rocky_pctcover %>%
#   mutate(
#     target_assemblage = recode(target_assemblage, ANY="ALL"))
usethis::use_data(rocky_cover, overwrite = TRUE)

rocky_cover

# rocky_counts ----
counts <- read_csv(counts_csv)

length(unique(counts$marine_site_name)) # 128
length(setdiff(unique(counts$marine_site_name), rocky_sites$site)) # 76
# TODO: add to rocky_sites with boolean columns: has_pctcover, has_counts

names(counts)
#   group_code,
# marine_site_name,
#   site_code,
#  site_lat, site_long,
#   ltm_lat, ltm_long, marine_sort_order, marine_common_year,
#   season_name, marine_season_code, marine_common_season,
# min_survey_date, max_survey_date,
# target_assemblage, species_code,
# size_bin, total, num_plots_sampled, method_code,
#   state_province, georegion, bioregion, mpa_designation,
#   mpa_region, mpa_lt_region, mpa_name, island,
#  last_updated

d <- counts %>%
  select(
    site     = marine_site_name,
    date_min = min_survey_date,
    date_max = min_survey_date,
    target_assemblage,
    sp_code = species_code,
    percent_cover,
    last_updated) %>%
# TODO: convert below from rocky_cover to rocky_counts...
  left_join(
    rocky_sites,
    by = c("site"))
nrow(d) # 2,145,274
d <- d %>%
  filter(!is.na(nms))
nrow(d) # 883,654

table(d$sp_code)
# ANTELE ARTCOR BARNAC CHITON CHOCAN CHTBAL CLACOL CRUCOR EGRMEN
#  22193  22246   1011  21106  21112  21254  21141  22238  22144
# EISARB ENDMUR ENPEPH FUCGAR HALSTE HEDSES HESCAL LIMPET LOTGIG
#  13588  21278  21099  10244  22098    298  21111  21234  21110
# MASSPP MAZAFF MAZSPP MYTCAL MYTTRO NEOLAR NONCRU OTHALG OTHBAR
#  21136  21177  10710  22278   3253   9683  22273  10798  18048
# OTHBRO OTHGRE OTHINV OTHRED OTHSUB PELLIM PHRSAB PHYSPP PISOCH
#  22143  22100  22144  22186  12352  10848  22170  22116  21135
# POLPOL PORSPP   ROCK   SAND SARMUT SCYSPP SEMCAR SEPBRA SILCOM
#  21153  21148  22438  22149  21642  21098   9487  15944  21143
#    TAR TETRUB ULVENT ZOSMAR
#  22030  21155  21149     63
table(d$target_assemblage)
#      balanus chthamalus_balanus         endocladia
#        41723             178298             137551
#        fucus      hesperophycus        mastocarpus
#        20133              44101              44930
#    mazzaella            mytilus       neorhodomela
#         5776             200105               2700
# pelvetiopsis       phyllospadix         pollicipes
#        24181              20431               4683
#     recovery          red_algae               rock
#         1872               2225               5957
#     silvetia                tar         tetraclita
#       131546               4663              12779
table(d$target_assemblage, d$sp_code)

# average percent_cover
d_dates <- bind_rows(
  # with target: average across sanctuary-group-site-date-target-species
  d %>%
    group_by(nms, cluster, site, date, target_assemblage, sp_code) %>%
    summarize(
      pct_cover = mean(percent_cover),
      .groups = "drop"),
  # withOUT target: average across sanctuary-group-site-date-species
  d %>%
    group_by(nms, cluster, site, date, target_assemblage, sp_code) %>%
    summarize(
      pct_cover = mean(percent_cover),
      .groups = "drop") %>%
    mutate(
      target_assemblage = "ALL"))

# average across sites to sanctuary-group-site-year
d_sites <- d_dates %>%
  mutate(
    date = date(glue("{year(date)}-06-15"))) %>%
  group_by(nms, cluster, site, date, target_assemblage, sp_code) %>%
  summarize(
    pct_cover = mean(pct_cover),
    .groups = "drop") # View(d_nms)

# average across sites to sanctuary-group-year
d_clusters <- d_dates %>%
  mutate(
    date = date(glue("{year(date)}-06-15"))) %>%
  group_by(nms, cluster, date, target_assemblage, sp_code) %>%
  summarize(
    pct_cover = mean(pct_cover),
    .groups = "drop") %>%
  mutate(
    site = "ALL") # View(d_clusters)

# average across sites to sanctuary-year
d_sanctuaries <- d_dates %>%
  mutate(
    date = date(glue("{year(date)}-06-15"))) %>%
  group_by(nms, date, target_assemblage, sp_code) %>%
  summarize(
    pct_cover = mean(pct_cover),
    .groups = "drop") %>%
  mutate(
    group = "ALL",
    site  = "ALL") # View(d_sanctuaries)

# combine all dates, and annual averages of sanctuary and cluster
rocky_cover <- bind_rows(
  d_sites,
  d_clusters,
  d_sanctuaries) # View(d)

usethis::use_data(rocky_cover, overwrite = TRUE)

