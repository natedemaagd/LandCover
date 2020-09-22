#' Predict values from `gls_spatial()` output
#'
#' After creating `gls` object(s) with the `gls_spatial()` function, this will create predicted values of the dependent variable from your regression, for the specified landcovers. Particularly useful once your landcover files have changed and you want to predict what effect this may have on the dependent variable.
#'
#' @import nlme doParallel foreach parallel
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

