# LandCover

**NOTE:** Substantial changes are being made to this package in preparation for implementation with a web app intended for use by non-R users. If using this package in R, see updated documentation there. Documentation on this site will be updated once all changes have been finalized. Some functions may not work as expected during this process.

This R package simulates landcover spread and the associated changes to water yield and recharge (or any other landcover-dependent spatial regressions you're interested in).

There are many functions in this package. See the [wiki](https://github.com/natedemaagd/LandCover/wiki) for documentation and examples for specific functions, or click a link below. An R markdown file is available as a tutorial for this package in the R folder above, or at [this](R/LandCover-example.Rmd) link, which provides a fully-executable version of the examples shown in the wiki pages. This package is still in development, but can still be installed in R using the instructions below. Progress on the package can be tracked [here](https://github.com/natedemaagd/LandCover/wiki/Project-Outline).


## Installation and usage
As long as you have the `devtools` package installed, the `LandCover` package in its current state can be installed with the command `devtools::install_github('natedemaagd/LandCover')`. Then, load it like any package using `library(LandCover)`. If you first need to install `devtools`, you may need to restart your R session before it works. There is also a known issue with the `rgdal` dependeny for Mac users with the new Big Sur update, so this packwage will not work correctly until that is fixed.


## Functions list
These functions are listed in the suggested order for your workflow. The examples in the pages mirror those in the [tutorial](R/LandCover-example.Rmd). An alternative to running the functions individually is to use the [`fullSimulation`](https://github.com/natedemaagd/LandCover/wiki/fullSimulation) function.
1. [`rasters_to_dataframe`](https://github.com/natedemaagd/LandCover/wiki/rasters_to_dataframe) Convert `raster`s into a single `data.frame`.
1. [`gls_spatial`](https://github.com/natedemaagd/LandCover/wiki/gls_spatial) Run a spatial GLS model that chooses the best spatial correlation structure based on model AIC.
1. [`gls_spatial_predict`](https://github.com/natedemaagd/LandCover/wiki/gls_spatial_predict) Predict values using results from [`gls_spatial`](https://github.com/natedemaagd/LandCover/wiki/gls_spatial) for a specified landcover. Capable of returning either a vecor or raster of predicted values, for either a specified landcover or all current landcovers in a dataset.
1. [`LandCoverPlot`](https://github.com/natedemaagd/LandCover/wiki/LandCoverPlot) Plot categorical, continuous, and priority plots. This function can be called at various points throughout the workflow, depending on what plots are needed.
1. [`LandCoverSpread`](https://github.com/natedemaagd/LandCover/wiki/LandCoverSpread) Run a simulation of landcover spread and associated changes to the dependent variable.
1. [`unit_converter`](https://github.com/natedemaagd/LandCover/wiki/unit_converter) Converts all dependent variable raster values according to a specified value using results from [`LandCoverSpread`](https://github.com/natedemaagd/LandCover/wiki/LandCoverSpread).
1. [`SimulationPlots`](https://github.com/natedemaagd/LandCover/wiki/SimulationPlots) Plot figures associated with the [`LandCoverSpread`](https://github.com/natedemaagd/LandCover/wiki/LandCoverSpread) function.
