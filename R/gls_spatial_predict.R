#' Predict values from `gls_spatial()` output
#'
#' After creating `gls` object(s) with the `gls_spatial()` function, this will create predicted values of the dependent variable from your regression, for the specified landcovers. Particularly useful once your landcover files have changed and you want to predict what effect this may have on the dependent variable.
#'
#' @import nlme doParallel foreach parallel raster
#' @importFrom foreach %dopar%
#'
#' @param data `data.frame` with spatial data
#' @param regression_results object of class `gls` from the `gls_spatial` function
#' @param landcover_varname character string. The name of the landcover variable in `data`
#' @param landcover_invasive value. Numerical or character value of the invasive landcover.
#' @param landcover_susceptible value. Numerical or character value(s) of the susceptible landcover(s). If more than one susceptible landcover, provide a vector `c()`.
#' @param dep_varname character string. Name of the dependent variable in `data`.
#' @param x_coords_varname character string. Name of the x-coordinate variable in `data`.
#' @param y_coords_varname character string. Name of the y-coordinate variable in `data`.
#'
#' @return A list of predicted values for current landcover, invaded landcover, and the associated rasters.
#'
#' @details This GLS predict function relies on a model created with the gls_spatial() function. It will not work if it is run independently (or before) the gls_spatial() function has been used to create the `regression_results` parameter. It returns a list of four objects: (1)a vector of predicted values under the current landcover; (2) a vector of predicted values assuming the invasive landcover takes the place of all susceptible landcovers; (3) a raster of predicted values under the current landcover; and (4) a raster of predicted values assuming the invasive landcover takes the place of all susceptible landcovers.
#'
#' @examples
#' set.seed(1)
#' # load packages
#' library(LandCover); library(foreach); library(raster)
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
#' # run the gls model
#' regression_results <- gls_spatial(data = dat, landcover_varname = 'landcover', landcover_vec = c(1,2),
#'                                   reg_formula = ET ~ elevation + temp, error_formula = ~ x + y)
#'
#' # get predicted values data
#' predicted_values <- gls_spatial_predict(data = dat, regression_results = regression_results, landcover_varname = 'landcover', landcover_invasive = 1, landcover_susceptible = 2, dep_varname = 'ET',
#'                                         x_coords_varname = 'x', y_coords_varname = 'y')
#'
#' @export


### FUNCTION:
gls_spatial_predict <- function(data, regression_results, landcover_varname, landcover_invasive, landcover_susceptible, dep_varname, x_coords_varname, y_coords_varname){


  # get predicted values with current landcover for invasive and susceptible landcovers
  pred_val_current <- NA
  for(i in 1:nrow(data)){

    # if you didn't run a regression for that landcover, return the actual value
    if(!(as.character(data[, landcover_varname][[i]]) %in% names(regression_results[[1]]))){ pred_val_current[[i]] <- data[i, dep_varname]}

    # otherwise, return the predicted value
    else{ pred_val_current[[i]] <- predict(regression_results[[1]][names(regression_results[[1]]) == as.character(data[, landcover_varname][[i]])][[1]], newdata = data[i,])}

  }


  # get predicted when landcover invades
  pred_val_invaded <- NA
  for(i in 1:nrow(data)){

    # if you didn't run a regression for that landcover, return NA
    if(!(as.character(data[, landcover_varname][[i]]) %in% names(regression_results[[1]]))){ pred_val_invaded[[i]] <- data[i, dep_varname]}

    # otherwise, return the predicted value
    else{ pred_val_invaded[[i]] <- predict(regression_results[[1]][names(regression_results[[1]]) == as.character(landcover_invasive)][[1]], newdata = data[i,])}

  }


  # get change in dependent variable
  pred_val_change <- pred_val_invaded - pred_val_current


  # convert to rasters
  pred_val_current_raster <- rasterFromXYZ(cbind(data[c(x_coords_varname, y_coords_varname)], pred_val_current))
  pred_val_invaded_raster <- rasterFromXYZ(cbind(data[c(x_coords_varname, y_coords_varname)], pred_val_invaded))
  pred_val_change_raster  <- rasterFromXYZ(cbind(data[c(x_coords_varname, y_coords_varname)], pred_val_change))


  # return results
  results <- list(pred_val_current, pred_val_invaded, pred_val_change, pred_val_current_raster, pred_val_invaded_raster, pred_val_change_raster)

  names(results) <- c('Predicted values, current landcover',        'Predicted values, post-invasion',        'Predicted values, change',
                      'Predicted values raster, current landcover', 'Predicted values raster, post-invasion', 'Predicted values raster, change')

  return(results)

}

