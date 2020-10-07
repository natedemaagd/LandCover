# LandCover
This `R` package simulates landcover spread and the associated changes to water yield and recharge (or any other landcover-dependent spatial regressions you're interested in).

There are many functions in this package. See the [wiki](https://github.com/natedemaagd/LandCover/wiki) for documentation for specific functions, or click a link below. An `R` markdown file is available as a tutorial for this package in the `R` folder above, or at [this](R/LandCover-example.Rmd) link, which provides a fully-executable version of the examples shown in the wiki pages. This package is still in development, but can still be installed in `R` using the instructions below. Progress on the package can be tracked [here](https://github.com/natedemaagd/LandCover/wiki/Project-Outline).


## Installation and usage
The package in its current state can be installed with the command `devtools::install_github('natedemaagd/LandCover')`. Then, load it like any package using `library(LandCover)`.


## Functions list
These functions are listed in the suggested order for your workflow. The examples in the pages mirror those in the [tutorial](R/LandCover-example.Rmd).
1. [`rasters_to_dataframe`](https://github.com/natedemaagd/LandCover/wiki/rasters_to_dataframe) Convert `raster`s into a single `data.frame`.
1. [`gls_spatial`](https://github.com/natedemaagd/LandCover/wiki/gls_spatial) Run a spatial GLS model that chooses the best spatial correlation structure based on model AIC.
1. [`gls_spatial_predict`](https://github.com/natedemaagd/LandCover/wiki/gls_spatial_predict) Predict values using results from `gls_spatial` for a specified landcover. Capable of returning either a vecor or raster of predicted values, for either a specified landcover or all current landcovers in a dataset.
1. [`LandCoverPlot`](https://github.com/natedemaagd/LandCover/wiki/LandCoverPlot) Plot categorical, continuous, and priority plots.
1. [`LandCoverSpread`](https://github.com/natedemaagd/LandCover/wiki/LandCoverSpread) Run a simulation of landcover spread and associated changes to the dependent variable.
1. [`SimulationPlots`](https://github.com/natedemaagd/LandCover/wiki/SimulationPlots) Plot figures associated with the `LandCoverSpread` function.
