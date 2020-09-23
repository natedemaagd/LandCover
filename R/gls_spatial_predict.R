#' Predict values from `gls_spatial()` output
#'
#' After creating `gls` object(s) with the `gls_spatial()` function, this will create predicted values of the dependent variable from your regression, for the specified landcovers. Particularly useful once your landcover files have changed and you want to predict what effect this may have on the dependent variable.
#'
#' @import nlme doParallel foreach parallel raster
#' @importFrom foreach %dopar%
#'
#' @param data `data.frame` with spatial data
#' @param reg_results object of class `gls` from the `gls_spatial` function
#' @param landcover_varname character string. The name of the landcover variable in `data`
#' @param landcover_val numerical or character. Value of landcover for which the new dependent variable values are to be predicted. Either a specific landcover from your data, or `'ALL'`.
#' @param return_raster logial. Do you want the result returned as a raster? Default is `FALSE`, and a vector is returned. If `TRUE`, you must specify the `x_coords` and `y_coords` from the data.
#' @param x_coords vector. A vector of length `nrow(data)` specifying the x-coordinates of the raster. Required if `return_raster = TRUE`.
#' @param y_coords vector. A vector of length `nrow(data)` specifying the y-coordinates of the raster. Required if `return_raster = TRUE`.
#'
#' @return A vector with predicted values of the dependent variable from gls_spatial()
#'
#' @details This GLS predict function relies on a model created with the gls_spatial() function. It will not work if it is run independently (or before) the gls_spatial() function has been used to create the `reg_results` parameter.
#'
#' @examples
#' set.seed(1)
#' # load packages
#' library(LandCover); library(foreach); library(raster)
#'
#' # initialize data.frame with coordinates
#' dat <- expand.grid(x = 1:20, y = 1:20, KEEP.OUT.ATTRS = FALSE)
#'
#' # create some data: elevation, landcover, and temp/ET dependent on elevation and landcover
#' dat$elevation <- with(dat, 50 + 2*x + 5*y + rnorm(nrow(dat), sd = 7))
#' dat$landcover <- ifelse(dat$elevation < median(dat$elevation), 1, 2)
#' dat$temp      <- with(dat, (120-0.7*(0.5*elevation + 0.3*y - 0.5*x + ifelse(landcover == 'lc1', -30, 0) + rnorm(nrow(dat)))))
#' dat$ET        <- with(dat, (   -0.4*(-2*temp       + 0.5*y - 1.0*x + ifelse(landcover == 'lc1', +20, 0) + rnorm(nrow(dat)))))
#'
#' # run the gls model
#' regression_results <- gls_spatial(data = dat, landcover_varname = 'landcover', landcover_vec = c(1,2),
#'                                   reg_formula = ET ~ elevation + temp, error_formula = ~ x + y)
#'
#' # get the expected values of present-day ET, using both types of landcover
#' dat$ET_predicted_ALL <- gls_spatial_predict(data = dat, reg_results = regression_results, landcover_varname = 'landcover', landcover_val = 'ALL')
#'
#' # create raster of predicted values using one landcover type
#' ET_predicted_lc1_raster  <- gls_spatial_predict(data = dat, reg_results = regression_results, landcover_varname = 'landcover', landcover_val = 1,
#'                                                 return_raster = TRUE, x_coords = dat$x, y_coords = dat$y)
#'
#' @export


### FUNCTION:
gls_spatial_predict <- function(data, reg_results, landcover_varname, landcover_val, return_raster = FALSE, x_coords = NULL, y_coords = NULL){

  # if `landcover == 'ALL'`, return vector of predicted values for current landcover
  if(landcover_val == 'ALL'){

    # get all unique landcover values
    landcover_vals <- unique(data[,landcover_varname])

    # create data.frame to hold all predicted values: one column per landcover type
    pred_values <- foreach(i = 1:length(landcover_vals)) %dopar% {

      predict(object = reg_results[names(reg_results) == as.character(landcover_vals[[i]])][[1]], newdata = data[data[,landcover_varname] == landcover_vals[[i]],])

    }
    pred_values <- lapply(pred_values, as.vector)  # remove attributes

    # order `pred_values` into one vector
       # get landcover type for each row in the data.frame
       landcover_indices <- list()
       for(j in 1:length(landcover_vals)){ landcover_indices[[j]] <- which(data[,landcover_varname] == landcover_vals[[j]])}

       # unlist the lists
       landcover_indices <- unlist(landcover_indices)
       pred_values       <- unlist(pred_values)

    # add ordered pred_values to the data using the landcover_indices
    pred_values <- pred_values[order(landcover_indices)]

  # otherwise, if `landcover` is a specific value, just use regression results for that one
  } else {

    # get predicted values
    pred_values <- predict(object = reg_results[names(reg_results) == as.character(landcover_val)][[1]], newdata = data)
    pred_values <- as.vector(pred_values)

  }

  # if raster = TRUE, return a raster instead of a vector, using the coordinates from `dat`
  if(return_raster == FALSE){return(pred_values)} else {return(raster::rasterFromXYZ(xyz = data.frame(x = x_coords, y = y_coords, z = pred_values)))}

}

