#' Run spatial GLS models with optimal spatial correlation structure.
#'
#' This function runs a GLS model on a spatial dataset. It chooses among five types of correlation structure, and returns the model with the lowest AIC.
#'
#' @import nlme doParallel foreach parallel
#' @importFrom foreach %dopar%
#'
#' @param data `data.frame` with spatial data
#' @param reg_results object of class `gls` from the `gls_spatial` function
#' @param lc value of landcover for which the new dependent variable values are to be predicted
#'
#' @return A list whose elements are the optimal GLS model. See details.
#'
#' @details Landcovers with large numbers of observations will take a long time to run. It is strongly recommended to use 5 cores in parallel. The code runs the landcover types sequentially, and the spatial GLS models for each landcover in parallel. The function will output the landcover and time of completion as it progresses, unless `silent = TRUE`. If there are too many observations for your system, you must take a representative subset. Each list element in the output corresponds to a landcover type. The objects are of class `gls` and will report the correlation structure that resulted in the lowest model AIC. If an element in the output list is `NA`, the log likelihood model failed to converge using all correlation structures. Try increasing the number of iterations or increasing the tolerance.
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
#' @export


### FUNCTION:
gls_spatial_predict <- function(data, reg_results, lc, reg_formula){

  eval(parse(text=paste0('reg_formula <- formula(reg_formula)')))

  predict(object = reg_results[names(reg_results) == as.character(lc)][[1]], newdata = data)

}

