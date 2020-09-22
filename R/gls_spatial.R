#' Run spatial GLS models with optimal spatial correlation structure.
#'
#' This function runs a GLS model on a spatial dataset. It chooses among five types of correlation structure, and returns the model with the lowest AIC.
#'
#' @import nlme doParallel foreach parallel
#' @importFrom foreach %dopar%
#'
#' @param data `data.frame` with spatial data
#' @param landcover_varname character string specifying the landcover variable from `data`
#' @param landcover_vec vector of all landcover types in `data$landcover_varname` to be analyzed
#' @param reg_formula regression formula to be used, as in `lm()`, with columns from `data` (e.g. `c ~ a + b`)
#' @param error_formula one-sided formula specifying coordinate columns from `data` (e.g. `~ x + y`)
#' @param num_cores numerical. Number of cores for parallel processing, max 5
#' @param maxIter numerical. Max iterations for GLS optimization, see `?nlme::glsControl()`
#' @param msMaxIter numerical. Max iterations for optimization step inside GLS optimization, see `?nlme::glsControl()`
#' @param tolerance numerical. Tolerance for the convergence criterion, see `?nlme::glsControl()`
#' @param silent logical. Hides notifications for completion of each landcover type. Default is `FALSE`
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
gls_spatial <- function(data, landcover_varname, landcover_vec, reg_formula, error_formula, num_cores = parallel::detectCores() - 1, maxIter = 50, msMaxIter = 50, tolerance = 1e-6, silent = FALSE){



  ### definitions ###

  # set up GLS
  nlme::glsControl(maxIter = maxIter, msMaxIter = msMaxIter, tolerance = tolerance)

  # define regression equation
  reg_formula <- formula(reg_formula)

  # define correlation types
  corr_types <- list(nlme::corLin(  form = formula(error_formula), nugget = TRUE, value = c(200, 0.1)),
                     nlme::corGaus( form = formula(error_formula), nugget = TRUE, value = c(200, 0.1)),
                     nlme::corRatio(form = formula(error_formula), nugget = TRUE, value = c(200, 0.1)),
                     nlme::corSpher(form = formula(error_formula), nugget = TRUE, value = c(200, 0.1)),
                     nlme::corExp(  form = formula(error_formula), nugget = TRUE, value = c(200, 0.1)))



  ### setup ###

  # register number of cores, max is length of correlation types
  doParallel::registerDoParallel(cores = max(c(num_cores, length(corr_types))))

  # rename landcover variable name
  colnames(data)[colnames(data) == landcover_varname] <- 'LC_var'



  ### run regression for each landcover ###

  # list to store all results
  reg_results <- list()

  # run regression for each type of land cover
  for(i in 1:length(landcover_vec)){

    # for each land cover type, run regression on each spatial correlation type
    reg_results[[i]] <- foreach::foreach(j = 1:length(corr_types)) %dopar% {

      # try to run the regression for correlation type j
      gls_j <- try(nlme::gls(model       = formula(reg_formula),
                             data        = data[data$LC_var == landcover_vec[[i]],],
                             correlation = corr_types[[j]]),
                   silent = TRUE)

      # if regression results return an error, return NA; otherwise return the regression result
      ifelse(is(gls_j, 'try-error'), return(NA), return(gls_j))

    }

    if(isTRUE(silent)) print(paste0('Completed landcover ', landcover_vec[[i]], ' at ', Sys.time()))

  }



  ### keep only the results with the lowest AIC for each landcover ###

  # get AICs for each land cover
  AICs <- foreach::foreach(i = 1:length(reg_results)) %dopar% {

    sapply(reg_results[[i]], function(lc_results){ ifelse(is.na(lc_results), NA, stats::AIC(lc_results))[[1]]})

  }

  # get the minimum AIC from each landcover model
  AIC_min <- sapply(AICs, which.min)

  # get final list of regression results, one for each landcover, corresponding to the model with lowest AIC
  reg_results_final <- list()
  for(i in 1:length(reg_results)){ reg_results_final[[i]] <- reg_results[[i]][[AIC_min[[i]]]]}

  # rename elements of `reg_results_final` to match the landcover codes
  names(reg_results_final) <- landcover_vec

  # return the regression results
  return(reg_results_final)

}

