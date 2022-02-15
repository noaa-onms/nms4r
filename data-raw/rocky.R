# TODO:
# - update and operationalize download from [MARINe_meta - Google Sheets](https://docs.google.com/spreadsheets/d/1XSCfBdIN0r6pWtSF5-TgscdtxrOmYLaJUrk02cwhlIo/edit#gid=643317542)
# - finish rocky_counts a la rocky_cover, but something to do with `method_code` field?
#   See: old [cinms/rocky.R · noaa-onms/cinms](https://github.com/noaa-onms/cinms/blob/28df6cdb65f4c7439a06f5f3496c0baac1747fe0/scripts/rocky.R)
librarian::shelf(
  dplyr, here, purrr, readr, sf, tidyr)
devtools::load_all()

# on Ben's Mac: 442.1 MB Dec 6, 2021; update these paths with new MARINe data and re-run
cover_csv    <- "/Users/bbest/My Drive (ben@ecoquants.com)/projects/nms-web/data/MARINe_Pacific-rocky-intertidal/egnyte_2021-12-06/phototranraw_download_all_20211206.csv"
cover_source <- "csv"
counts_csv   <- "/Users/bbest/My Drive (ben@ecoquants.com)/projects/nms-web/data/MARINe_Pacific-rocky-intertidal/egnyte_2021-12-06/seastarkat_size_count_zeroes_totals_download_all_20211206.csv"
species_csv  <- "/Users/bbest/My Drive (ben@ecoquants.com)/projects/nms-web/data/MARINe_Pacific-rocky-intertidal/egnyte_2021-12-06/marine_lumping_codes_definitions.csv"

# data-raw files
rocky_clusters_csv <- here("data-raw/rocky_clusters.csv")
rocky_sites_geo    <- here("data-raw/rocky_sites.geojson")

# rocky_clusters ----
# TODO: operationalize from clusters sheet in [MARINe_meta - Google Sheets](https://docs.google.com/spreadsheets/d/1XSCfBdIN0r6pWtSF5-TgscdtxrOmYLaJUrk02cwhlIo/edit#gid=643317542)
rocky_clusters <- rocky_clusters_csv %>%
  read_csv() %>%
  fill(nms) %>%
  group_by(nms) %>%
  fill(cluster)
usethis::use_data(rocky_clusters, overwrite = TRUE)

# rocky_sites ----

# read cover
if (cover_source == "erddap"){
  d_cover <- read_csv_fmt(cover_csv, cover_fmt)
} else {
  d_cover <- read_csv(cover_csv)
}

# extract all unique sites from cover as spatial points
cover_sites <- d_cover %>%
  rename(
    site = marine_site_name) %>%
  filter(
    !is.na(longitude),
    !is.na(latitude)) %>%
  group_by(site) %>%
  summarize(
    lat = first(latitude),
    lon = first(longitude)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)
message(glue("Found {nrow(cover_sites)} cover_sites"))
# Found 120 cover_sites

# read counts
d_counts <- read_csv(counts_csv)

# extract all unique sites from counts as spatial points
counts_sites <- d_counts %>%
  rename(
    site = marine_site_name) %>%
  filter(
    !is.na(site_lat),
    !is.na(site_long)) %>%
  group_by(site) %>%
  summarize(
    lat = first(site_lat),
    lon = first(site_long)) %>%
  mutate(

  )
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)
message(glue("Found {nrow(counts_sites)} counts_sites"))
# Found 128 counts_sites

# sites = cover_sites + counts_sites
sites <- bind_rows(cover_sites, counts_sites)
message(glue("Filtering {sum(duplicated(sites$site))} duplicate sites in cover_sites + counts_sites"))
# Found 113 duplicate sites in cover_sites + counts_sites
sites <- sites %>%
  filter(!duplicated(site)) %>%
  left_join(
    cover_sites %>%
      st_drop_geometry() %>%
      select(site) %>%
      mutate(
        has_cover = T),
    by = "site") %>%
  left_join(
    counts_sites %>%
      st_drop_geometry() %>%
      select(site) %>%
      mutate(
        has_counts = T),
    by = "site") %>%
  replace_na(list(rocky_cover = F, rocky_counts = F))
table(sites %>% select(rocky_cover, rocky_counts) %>% st_drop_geometry())
#            rocky_counts
# rocky_cover  FALSE TRUE
#       FALSE      0   15
#        TRUE      7  113
# So 113 sites are shared, plus sites unique to
#   rocky_cover (n=7) and rocky_counts (n=15)

# assign site sanctuary based on buffered intersection
sanctuaries_buf <- st_buffer(sanctuaries, 0.01) # 0.01 dd ≈ 1.1 km
x <- st_intersects(
  sites,
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
sites$nms_buf <- z$nms

table(z$nms)
# CINMS GFNMS MBNMS OCNMS
#    22    18    21     6
#
# OLD: rocky_cover only
# CINMS GFNMS MBNMS OCNMS
#    21    10    20     6

table(rocky_clusters$nms)
# CINMS MBNMS
#    26    33
# OLD: rocky_cover only
# CINMS MBNMS
#    26    33

# assign sanctuary also on membership of site in rocky_clusters
sites <- sites %>%
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
message(glue("Found {nrow(sites)} sites within 0.01º of a sanctuary or in rocky_clusters"))

sites <- sites %>%
  relocate(nms, cluster, site)
write_sf(sites, rocky_sites_geo, delete_dsn=T)
rocky_sites <- read_sf(rocky_sites_geo)
usethis::use_data(rocky_sites, overwrite = TRUE)

# rocky_cover ----
d <- d_cover %>%
  select(
    site      = marine_site_name,
    date      = survey_date,
    sp_target = target_assemblage,
    sp_code   = lumping_code,
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
table(d$sp_target)
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
table(d$sp_target, d$sp_code)

# average percent_cover
d_dates <- bind_rows(
  # with sp_target: average across sanctuary-cluster-site-date-target-species
  d %>%
  group_by(nms, cluster, site, date, sp_target, sp_code) %>%
  summarize(
    pct_cover    = mean(percent_cover),
    last_updated = max(last_updated),
    .groups = "drop"),
  # withOUT sp_target: average across sanctuary-cluster-site-date-species
  d %>%
    group_by(nms, cluster, site, date, sp_code) %>%
    summarize(
      pct_cover    = mean(percent_cover),
      last_updated = max(last_updated),
      .groups = "drop") %>%
    mutate(
      sp_target = "ALL"))

# average across sites to sanctuary-cluster-site-year
d_sites <- d_dates %>%
  mutate(
    year = year(date)) %>%
  group_by(nms, cluster, site, year, sp_target, sp_code) %>%
  summarize(
    pct_cover = mean(pct_cover),
    last_updated = max(last_updated),
    .groups = "drop") # View(d_nms)

# average across sites to sanctuary-cluster-year
d_clusters <- d_dates %>%
  mutate(
    year = year(date)) %>%
  group_by(nms, cluster, year, sp_target, sp_code) %>%
  summarize(
    pct_cover = mean(pct_cover),
    last_updated = max(last_updated),
    .groups = "drop") %>%
  mutate(
    site = "ALL") # View(d_clusters)

# average across sites to sanctuary-year
d_sanctuaries <- d_dates %>%
  mutate(
    year = year(date)) %>%
  group_by(nms, year, sp_target, sp_code) %>%
  summarize(
    pct_cover = mean(pct_cover),
    last_updated = max(last_updated),
    .groups = "drop") %>%
  mutate(
    cluster = "ALL",
    site  = "ALL") # View(d_sanctuaries)

# combine all dates, and annual averages of sanctuary and cluster
rocky_cover <- bind_rows(
  d_sites,
  d_clusters,
  d_sanctuaries) # View(d)
# rocky_cover
# table(rocky_cover$last_updated, useNA = "ifany")
# table(rocky_cover$nms,          useNA = "ifany")
# table(rocky_cover$cluster,      useNA = "ifany")
# table(rocky_cover$site,         useNA = "ifany")

usethis::use_data(rocky_cover, overwrite = TRUE)

# rocky_counts ----

# see original [make_nms_spp_sscount](https://github.com/noaa-onms/cinms/blob/28df6cdb65f4c7439a06f5f3496c0baac1747fe0/scripts/rocky.R#L483-L606)

names(d_counts)
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

d <- d_counts %>%
  select(
    site      = marine_site_name,
    date      = min_survey_date,
    sp_target = target_assemblage,
    sp_code   = species_code,
    sp_method = method_code,
    count     = total,
    last_updated) %>%
  left_join(
    rocky_sites,
    by = c("site"))
nrow(d) # 27,660
d <- d %>%
  filter(!is.na(nms))
nrow(d) # 11,849

table(d$sp_code)
# DERIMB EVATRO HENSPP KATTUN LEPTAS PATMIN PISBRE PISGIG PISOCH PYCHEL STRPUR
#    133     92    308   1306    292    351    108    262   8830    166      1

table(d$sp_target)
# sea_star
#    11849
stopifnot(length(unique(d$sp_target)) == 1)

table(d$sp_method)
#  BT BT25 GSES   IP TS30
# 348  172 3593 7735    1

# average count
d_dates <- bind_rows(
  # with sp_method: average across sanctuary-cluster-site-date-method-species
  d %>%
    group_by(nms, cluster, site, date, sp_method, sp_code) %>%
    summarize(
      count        = mean(count),
      last_updated = max(last_updated),
      .groups = "drop"),
  # withOUT sp_method: average across sanctuary-cluster-site-date-species
  d %>%
    group_by(nms, cluster, site, date, sp_code) %>%
    summarize(
      count        = mean(count),
      last_updated = max(last_updated),
      .groups = "drop") %>%
    mutate(
      sp_method = "ALL"))

# average across sites to sanctuary-cluster-site-year
d_sites <- d_dates %>%
  mutate(
    year = year(date)) %>%
  group_by(nms, cluster, site, year, sp_method, sp_code) %>%
  summarize(
    count        = mean(count),
    last_updated = max(last_updated),
    .groups = "drop") # View(d_nms)

# average across sites to sanctuary-cluster-year
d_clusters <- d_dates %>%
  mutate(
    year = year(date)) %>%
  group_by(nms, cluster, year, sp_method, sp_code) %>%
  summarize(
    count        = mean(count),
    last_updated = max(last_updated),
    .groups = "drop") %>%
  mutate(
    site = "ALL") # View(d_clusters)

# average across sites to sanctuary-year
d_sanctuaries <- d_dates %>%
  mutate(
    year = year(date)) %>%
  group_by(nms, year, sp_method, sp_code) %>%
  summarize(
    count        = mean(count),
    last_updated = max(last_updated),
    .groups = "drop") %>%
  mutate(
    cluster = "ALL",
    site  = "ALL") # View(d_sanctuaries)

# combine all dates, and annual averages of sanctuary and cluster
rocky_counts <- bind_rows(
  d_sites,
  d_clusters,
  d_sanctuaries) # View(d)

usethis::use_data(rocky_counts, overwrite = TRUE)


# rocky_species ----
rocky_species <- read_csv(species_csv) %>%
  rename(sp_code = lumping_code)

usethis::use_data(rocky_species, overwrite = TRUE)
