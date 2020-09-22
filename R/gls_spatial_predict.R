#' Run spatial GLS models with optimal spatial correlation structure.
#'
#' This function runs a GLS model on a spatial dataset. It chooses among five types of correlation structure, and returns the model with the lowest AIC.
#'
#' @import nlme doParallel foreach parallel
#' @importFrom foreach %dopar%
#'
#' @param data `data.frame` with spatial data
#' @param reg_results object of class `gls` from the `gls_spatial` function
#' @param lc vector of landcovers for which the new dependent variable values are to be predicted
#'
#' @return A vector with predicted values of the dependent variable from gls_spatial()
#'
#' @details
#'
#' @examples
#' # create data.frame with x,y coordinates, landcover variable, and two covariates
#' dat <- data.frame(x = rnorm(100), y = rnorm(100), lc = rep(c('a','b'), each = 5), var1 = rnorm(100), var2 = rnorm(100))
#'
#' # add a dependent variable to `dat`
#' dat$var3 <- with(dat, var1 + 2*var2 + rnorm(100))
#'
#' # run the gls model
#' gls_results <- gls_spatial(data = dat, landcover_varname = 'lc', landcover_vec = c('a', 'b'), reg_formula = var3 ~ var1 + var2, error_formula = ~x+y)
#'
#' # add predicted values of var3 to the data.frame
#' dat$var3_predict <- gls_spatial_predict(data = dat, reg_results = gls_results, landcover = unique(dat$lc))
#'
#' @export


### FUNCTION:
gls_spatial_predict <- function(data, reg_results, landcover){

  predict(object = reg_results[names(reg_results) %in% as.character(landcover)][[1]], newdata = data)

}

