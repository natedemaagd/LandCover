# gls_spatial

This function will be the first step in creating a landcover-dependent spatial statistical model. Its main arguments are a `data.frame` (`raster`s must be converted and combined into a `data.frame` before the function can be used; see below for an example how to do this), regression formula, spatial coordinates, and landcover information. It runs a `gls` model using the regression formula, and chooses the best spatial correlation structure from `linear`, `Gaussian`, `Ratio`, `Spherical`, and `Exponential` based on model AIC. See `?gls_spatial()` and `?gls()` for full details.


## Example

Suppose we have a `data.frame` with the variables `x`, `y`, `elevation`, `landcover`, `temp`, and `rain`.
```r
set.seed(42)

# initialize data.frame with coordinates
dat <- expand.grid(x = 1:20, y = 1:20, KEEP.OUT.ATTRS = FALSE)

# create some data: elevation, landcover, and temp/rain dependent on elevation and landcover
dat$elevation <- with(dat, 50 + 2*x + 5*y + rnorm(nrow(dat), sd = 7))
dat$landcover <- rep(c(1, 2), each = nrow(dat)/2)
dat$temp      <- with(dat, (-0.7*(0.5*elevation + 0.3*y - 0.5*x + ifelse(landcover == 'lc1', -30, 0) + rnorm(nrow(dat)))))
dat$rain      <- with(dat, ( 0.7*(2*elevation   + 0.5*y - 1.0*x + ifelse(landcover == 'lc1', +20, 0) + rnorm(nrow(dat)))))
```
<img src="https://github.com/natedemaagd/LandCover/blob/master/Figures/raster_plots.png" alt="raster_plots" width="600">



### 1. Starting data

#### 1.1 If you already have a `data.frame`
Easy! Skip to section 2 below where we run the regressions

#### 1.2 If you are starting with `raster`s
If you start with `raster`s instead of a `data.frame`, you'll need to convert and merge them into a `data.frame` for the function to work. Suppose you have four rasters, `raster_terrain`, `raster_landcover`, `raster_temp`, and `raster_rain`. Below is the code to convert the `data.frame` above into rasters if you'd like to use rasters with the same data (that is, this doesn't usually have to be done. I'm creating the rasters from the data above).
```r
raster_terrain   <- rasterFromXYZ(dat[c('x', 'y', 'elevation')])
raster_landcover <- rasterFromXYZ(dat[c('x', 'y', 'landcover')])
raster_temp      <- rasterFromXYZ(dat[c('x', 'y', 'temp')])
raster_rain      <- rasterFromXYZ(dat[c('x', 'y', 'rain')])
```

To convert the rasters to `data.frame`s, use the `raster::as.data.frame()` function. Be sure to keep the coordinates by specifying `xy = TRUE`!
```r
dat_terrain   <- as.data.frame(raster_terrain,   xy = TRUE)
dat_landcover <- as.data.frame(raster_landcover, xy = TRUE)
dat_temp      <- as.data.frame(raster_temp,      xy = TRUE)
dat_rain      <- as.data.frame(raster_rain,      xy = TRUE)
```

We can then merge these into one `data.frame` by exploiting the `Reduce` function. There is probably an easier way to do this, since `Reduce` requires a second argument that complicates it a little bit. Here is a function that can merge the `data.frame`s, then delete it from your environment.
```r
# create a merge function for `Reduce` to use
merge_func <- function(x, y)
  merge(x, y, by=c("x", "y"), all=TRUE)

# merge the data.frames with `Reduce`
dat <- Reduce(merge_func, list(dat_terrain, dat_landcover, dat_temp, dat_rain))

# remove the temporary merging function, along with the data.frames we used to create the original rasters
rm(merge_func, dat_terrain, dat_landcover, dat_temp, dat_rain)
```

### 2. Running the function
Setting up the data for the example was more complicated than actually running the function once you have a `data.frame`. To run the function, you'll need to specify, at a minimum, the data, the name of the landcover variable in the data, a vector of specific landcover values for which you want regression results, a regression formula, and the error formula (the coordinates from the data). For our example, let's say we want to test both landcover types, `1` and `2`. For each type of landcover, we might be interested in analyzing the relationship between rain, elevation, and temperature. We would set up the function like this:
```r
regression_results <- gls_spatial(data = dat, landcover_varname = 'landcover', landcover_vec = c(1,2),
                                  reg_formula = rain ~ elevation + temp, error_formula = ~ x + y)
```
We specify the name of the landcover variable in our data, and which types of landcover within that variable we want to test. Then, we specify the regression formula using the standard `R` sytax, along with the error formula which contains our coordinate variables. Note that the `landcover_varname` is a character string. Our `landcover_vec` contains numerical values since they came from a raster, but if you start with a `data.frame`, the function will also accept a vector of character strings. By default the function will run in parallel, using `max(# of cores in your computer -1, 5)` cores. It will not use more than 5 cores because there are only 5 types of spatial correlation structures tested.

The function may take a significant amount of time to complete if you have large datasets, even when the function is run in parallel. Depending on the capabilities of your system, the data may be too large to complete the function. You will get an error if this occurs, and you'll have to reduce the size using a representative sample of your data (which is up to you to determine). As each landcover type is completed, the function will return a message such as `"Completed landcover 1 at 2020-09-21 12:27:50"` so you know how far along it is. Once it is done running all the regressions, it will compare the `AIC` of all models within each landcover type, and keep only the best model with the lowest `AIC`. If all models return `NA`, the log likelihood function failed to converge for all correlation structures. If this happens you may try increasing the number of iterations using the `maxIter` or `msMaxIter` options, or decrease the model sensitivity with the `tolerance` option. See `?gls()` or `?gls_spatial()` for more information.

Once the function is done, if you're curious, you can check which correlation structure was best for each of the landcovers:
```r
regression_results[[1]]$modelStruct$corStruct
regression_results[[2]]$modelStruct$corStruct
```

You can also get the regression results with the standard `summary` function:
```r
summary(regression_results[[1]])
summary(regression_results[[2]])
```

To see the effect of the spatial correlation (and difference between `lm()` and `gls()` in general), compare these results to the results of the linear model:
```r
summary(lm(data = dat[dat$landcover == 1,], formula = rain ~ elevation + temp))
summary(lm(data = dat[dat$landcover == 2,], formula = rain ~ elevation + temp))
```
