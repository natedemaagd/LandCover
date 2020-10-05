#' Convert rasters to a data.frame
#'
#' This function converts `raster` objects to a single `data.frame`.
#'
#' @import raster
#'
#' @param ... `raster` objects.
#'
#' @return A `data.frame`
#'
#' @details This function will take all `raster`s and merge them into a single `data.frame`. Note that all provided rasters must have the same extent, resolution, and projection. If you have two rasters, `x` and `y`, simply run `rasters_to_dataframe(x,y)`.
#'
#' @examples
#' # load packages
#' library(LandCover)
#'
#' # create some data: elevation, landcover, and temp/ET dependent on elevation and landcover
#' dat <- expand.grid(x = 1:20, y = 1:20, KEEP.OUT.ATTRS = FALSE)
#' dat$elevation <- with(dat, 50 + 2*x + 5*y + rnorm(nrow(dat), sd = 7))
#' dat$landcover <- ifelse(dat$elevation < median(dat$elevation), 1, 2)
#' dat[dat$x < median(dat$x) & dat$landcover == 2, 'landcover'] <- 3
#' dat$temp      <- with(dat, (120-0.7*(0.5*elevation + 0.3*y - 0.5*x + ifelse(landcover == 'lc1', -30, 0) + rnorm(nrow(dat)))))
#' dat$ET        <- with(dat, (   -0.4*(-2*temp       + 0.5*y - 1.0*x + ifelse(landcover == 'lc1', +20, 0) + rnorm(nrow(dat)))))
#'
#' # create rasters as an example, using the existing data created above (!!!!skip this in your own workflow since you already have rasters!!!!)
#' raster_terrain   <- rasterFromXYZ(dat[c('x', 'y', 'elevation')])
#' raster_landcover <- rasterFromXYZ(dat[c('x', 'y', 'landcover')])
#' raster_temp      <- rasterFromXYZ(dat[c('x', 'y', 'temp')])
#' raster_ET        <- rasterFromXYZ(dat[c('x', 'y', 'ET')])
#'
#' # convert rasters back to data.frame
#' rasters_to_dataframe(raster_terrain, raster_landcover, raster_temp, raster_ET)
#'
#' @export


### FUNCTION:
rasters_to_dataframe <- function(...){

  # create list of rasters
  list_of_rasters <- list(...)

  # convert each raster to a data.frame
  list_of_dataframes <- lapply(list_of_rasters, function(r){ raster::as.data.frame(r, xy = TRUE)})

  # create function to merge data.frames
  merge_func <- function(x,y){

    merge(x, y, by=c("x", "y"), all=TRUE)

  }

  return(Reduce(merge_func, list_of_dataframes))

}

