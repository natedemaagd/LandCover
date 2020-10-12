#' Convert the units of a LandCoverSpread simulation
#'
#' This function converts the units of all rasters (except landcover rasters) returned from the `LandCoverSpread` function.
#'
#'
#' @param LandCoverSpread_results list. Object retured from a call to the `LandCoverSpread` simulation function.
#' @param unit_conversion_factor numerical. The value to be multiplied with all rasters for unit conversion.
#' @param dep_var_modified logical. Indicates whether the `dep_var` was modified in `LandCoverSpread`
#'
#' @return A list with the same length and contents as `LandCoverSpread`.
#'
#' @details This function simply multiplies the values of the rasters of dependent variables by the specified value. If you chose to modify the values in `LandCoverSpread`, you should specify `dep_var_modified = TRUE` (the default behavior).
#'
#' @examples
#' set.seed(1)
#' library(LandCover); library(gridExtra); library(ggplot2); library(raster); library(foreach)
#' library(sp); library(tidyr); library(rgdal); library(tidyverse); library(viridis); library(ggpubr)
#'
#' # initialize data.frame with coordinates
#' dat <- expand.grid(x = 1:20, y = 1:20, KEEP.OUT.ATTRS = FALSE)
#'
#'
#' # create some data: elevation, landcover, and temp/ET dependent on elevation and landcover
#' dat$elevation <- with(dat, 50 + 2*x + 5*y + rnorm(nrow(dat), sd = 7))
#' dat$landcover <- ifelse(dat$elevation < median(dat$elevation), 1, 2)
#' dat[dat$x < median(dat$x) & dat$landcover == 2, 'landcover'] <- 3
#' dat$temp      <- with(dat, (120-0.7*(0.5*elevation + 0.3*y - 0.5*x + ifelse(landcover == 'lc1', -30, 0) + rnorm(nrow(dat)))))
#' dat$ET        <- with(dat, (   -0.4*(-2*temp       + 0.5*y - 1.0*x + ifelse(landcover == 'lc1', +20, 0) + rnorm(nrow(dat)))))
#'
#' # run regression
#' regression_results <- gls_spatial(data = dat, landcover_varname = 'landcover', landcover_vec = c(1,2), reg_formula = ET ~ elevation + temp, error_formula = ~x+y)
#'
#' # predict values of ET before and after invasion
#' pred_values <- gls_spatial_predict(data = dat, regression_results = regression_results, landcover_varname = 'landcover', landcover_invasive = 1, landcover_susceptible = 2,
#'                                    dep_varname = 'ET', x_coords_varname = 'x', y_coords_varname = 'y')
#'
#' # get landcover raster
#' lc_raster <- rasterFromXYZ(dat[c('x', 'y', 'landcover')])
#'
#' # run landcover simulation
#' landcover_sim <- LandCoverSpread(infest_val = 1, suscep_val = 2, spread_rate = 0.05, birdcell = 0, simlength = 15, simulation_count = 100,
#'                                  lc_raster = lc_raster, dep_var_raster_initial = pred_values$`Predicted values raster, current landcover`,
#'                                  dep_var_raster_pred = pred_values$`Predicted values raster, post-invasion`,
#'                                  dep_var_modifier = 0.80, silent = TRUE)
#'
#' # convert the units - multiply all dep_var raster values by 2
#' landcover_sim_units_converted <- unit_converter(LandCoverSpread_results = landcover_sim, unit_conversion_factor = 2)
#'
#' @export


### FUNCTION:
unit_converter <- function(LandCoverSpread_results, unit_conversion_factor, dep_var_modified = TRUE){

  # convert dep_var_rasters
  list_of_dep_var_rasters <- lapply(LandCoverSpread_results$list_of_dep_var_rasters, function(x){ x * unit_conversion_factor })

  # if dep_var was modified, convert its units as well
  if(dep_var_modified){

    list_of_dep_var_rasters_modified <- lapply(LandCoverSpread_results$list_of_dep_var_rasters_modified, function(x){ x * unit_conversion_factor })

  }

  # convert dep_var change from year 0
  list_of_dep_var_rasters_change_from_year_0 <- lapply(LandCoverSpread_results$list_of_dep_var_rasters_change_from_year_0, function(x){ x * unit_conversion_factor })

  # if dep_var was modified, convert its units as well
  if(dep_var_modified){

    list_of_dep_var_rasters_change_from_year_0_modified <- lapply(LandCoverSpread_results$list_of_dep_var_rasters_change_from_year_0_modified, function(x){ x * unit_conversion_factor })

  }

  # convert cumulative change
  raster_dep_var_cumulative_change <- LandCoverSpread_results$raster_dep_var_cumulative_change * unit_conversion_factor

  # if dep_var was modified, convert its units as well
  if(dep_var_modified){

    raster_dep_var_cumulative_change_modified <- LandCoverSpread_results$raster_dep_var_cumulative_change_modified * unit_conversion_factor

  }

  # return results
  if(dep_var_modified){

    results_list <- list(LandCoverSpread_results$list_of_landcover_rasters,
                         list_of_dep_var_rasters,                    list_of_dep_var_rasters_modified,
                         list_of_dep_var_rasters_change_from_year_0, list_of_dep_var_rasters_change_from_year_0_modified,
                         raster_dep_var_cumulative_change,           raster_dep_var_cumulative_change_modified)

    names(results_list) <- c('list_of_landcover_rasters',
                             'list_of_dep_var_rasters',                    'list_of_dep_var_rasters_modified',
                             'list_of_dep_var_rasters_change_from_year_0', 'list_of_dep_var_rasters_change_from_year_0_modified',
                             'raster_dep_var_cumulative_change',           'raster_dep_var_cumulative_change_modified')

    return(results_list)

  } else {

    results_list <- list(list_of_landcover_rasters,
                         list_of_dep_var_rasters,
                         list_of_dep_var_rasters_change_from_year_0,
                         raster_dep_var_cumulative_change)

    names(results_list) <- c('list_of_landcover_rasters',
                             'list_of_dep_var_rasters',
                             'list_of_dep_var_rasters_change_from_year_0',
                             'raster_dep_var_cumulative_change')

    return(results_list)

  }

}
