#' Predict values from `gls_spatial()` output
#'
#' After creating `gls` object(s) with the `gls_spatial()` function, this will create predicted values of the dependent variable from your regression, for the specified landcovers. Particularly useful once your landcover files have changed and you want to predict what effect this may have on the dependent variable.
#'
#' @import nlme doParallel foreach parallel raster
#' @importFrom foreach %dopar%
#'
#' @param data `data.frame` with spatial data
#' @param reg_results object of class `gls` from the `gls_spatial` function
#' @param lc value of landcover for which the new dependent variable values are to be predicted
#'
#' @return A vector with predicted values of the dependent variable from gls_spatial()
#'
#' @details This GLS predict function relies on a model created with the gls_spatial() function. It will not work if it is run independently (or before) the gls_spatial() function has been used to create the `reg_results` parameter.
#'
#' @examples
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
#' # add vector of predicted values of var3 to the data.frame
#' dat$ET_predicted <- foreach(i = 1:nrow(dat), .combine = 'c') %dopar% {
#'                              gls_spatial_predict(data = dat[i,], reg_results = regression_results, landcover = dat[i,'landcover'])
#'                             }
#'
#' # create raster of predicted values using one landcover type
#' ET_predict_raster <- gls_spatial_predict(data = dat, reg_results = regression_results, landcover = 1, return_raster = TRUE, x_coords = dat$x, y_coords = dat$y)
#'
#' @export


### FUNCTION:
gls_spatial_predict <- function(data, reg_results, landcover, return_raster = FALSE, x_coords = NULL, y_coords = NULL){

  # get predicted values
  pred_values <- predict(object = reg_results[names(reg_results) %in% as.character(landcover)][[1]], newdata = data)

  # if raster = TRUE, return a raster instead of a vector, using the coordinates from `dat`
  if(return_raster == FALSE){return(pred_values)} else {return(raster::rasterFromXYZ(xyz = data.frame(x = x_coords, y = y_coords, z = pred_values)))}

}

