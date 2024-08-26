# onmsR 0.2.3

* Drop dependencies on deprecated `rgdal` and `raster` packages in favor of `terra`. 
This only affects the `ply2erddap()` function.

# onmsR 0.2.2

* Force ERDDAP URL of https://coastwatch.pfeg.noaa.gov/erddap/ vs default `rerddap::eurl()` of https://upwell.pfeg.noaa.gov/erddap/.

# onmsR 0.2.1

* Replaced all instances of "nms4R" with "onmsR".

# onmsR 0.2.0

* Moved Reporting functions to [infographiqR](https://marinebon.org/infographiqR) per [marinebon/infographiqR#60](https://github.com/marinebon/infographiqR/issues/60) and renamed to R package to `onmsR`.

# onmsR 0.1.4

* Added a `NEWS.md` file to track changes to the package.
* Dropped `tidyverse` dependency.
