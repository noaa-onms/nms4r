# nms4r
R package of common functions used across National Marine Sanctuaries for infographic development


## developing an R package

Links:
* example [whalesafe4r](https://github.com/BenioffOceanInitiative/whalesafe4r/tree/ecacff311c8eb5040f26f57953b682aeb1d521dc)
* old tutorial [Quick Intro to Package Development with devtools](http://ucsb-bren.github.io/ESM296-3W-2016/wk07_package.html)
* latest technique [Automate Package and Project Setup • usethis](https://usethis.r-lib.org/)

```R
library(usethis)
create_package("~/github/nms4r")
```

- Updated `DESCRIPTION`

### create a function

- Create in R/*.R

### add documentation

- RStudio menu > Code > Insert Roxygen Skeleton
- [Object documentation · R packages](http://r-pkgs.had.co.nz/man.html)


Run:

```r
devtools::document()
```

### test R package

```r
devtools::load_all()
```


### build website

```r
pkgdown::build_site()
```

### install and run locally
