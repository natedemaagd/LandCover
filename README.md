# LandCover
R package to simulate landcover spread and the associated changes to water yield and recharge (or any other landcover-dependent spatial regressions you're interested in :bowtie:).

There are many functions in this package. See the [wiki](https://github.com/natedemaagd/LandCover/wiki) for documentation for specific functions, or click a link below. This package is still in development, but can still be installed in `R` using the instructions below. Progress on the package can be tracked [here](https://github.com/natedemaagd/LandCover/wiki/Project-Outline).


## Installation and usage
The package in its current state can be installed with the command `devtools::install_github('natedemaagd/LandCover')`. Then, load it like any library using `library(LandCover)`.


## Functions list
1. [`gls_spatial`](https://github.com/natedemaagd/LandCover/wiki/gls_spatial) Run a spatial GLS model that chooses the best spatial correlation structure based on model AIC.
