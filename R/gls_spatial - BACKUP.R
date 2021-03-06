#' Run spatial GLS models with optimal spatial correlation structure.
#'
#' This function runs a GLS model on a spatial dataset. It chooses among five types of correlation structure, and returns the model with the lowest AIC.
#'
#' @import nlme doParallel foreach parallel viridis
#' @importFrom foreach %dopar%
#'
#' @param data `data.frame` with spatial data
#' @param landcover_varname character string specifying the landcover variable from `data`
#' @param landcover_vec vector of all landcover types in `data$landcover_varname` to be analyzed. This should include the landcover that is spreading along with all landcovers that are susceptible to the spread.
#' @param reg_formula regression formula to be used, as in `lm()`, with columns from `data` (e.g. `c ~ a + b`)
#' @param error_formula one-sided formula specifying coordinate columns from `data` (e.g. `~ x + y`)
#' @param num_cores numerical. Number of cores for parallel processing, max 5
#' @param maxIter numerical. Max iterations for GLS optimization, see `?nlme::glsControl()`
#' @param msMaxIter numerical. Max iterations for optimization step inside GLS optimization, see `?nlme::glsControl()`
#' @param tolerance numerical. Tolerance for the convergence criterion, see `?nlme::glsControl()`
#' @param silent logical. Hides notifications for completion of each landcover type. Default is `FALSE`
#'
#' @return A list of two objects. The first list element is a list of regression results for each landcover type. The second list element is a residual density plot, separated by landcover type. See details.
#'
#' @details Landcovers with large numbers of observations will take a long time to run. It is strongly recommended to use 5 cores in parallel. The code runs the landcover types sequentially, and the spatial GLS models for each landcover in parallel. The function will output the landcover and time of completion as it progresses, unless `silent = TRUE`. If there are too many observations for your system, you must take a representative subset. Each list element in the output corresponds to a landcover type. The objects are of class `gls` and will report the correlation structure that resulted in the lowest model AIC. If an element in the output list is `NA`, the log likelihood model failed to converge using all correlation structures. Try increasing the number of iterations or increasing the tolerance.
#'
#' @examples
#' # load packages
#' library(LandCover); library(foreach)
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
#' @export


### FUNCTION:
gls_spatial <- function(data, landcover_varname, landcover_vec, reg_formula, error_formula, num_cores = parallel::detectCores() - 1, maxIter = 50, msMaxIter = 50, tolerance = 1e-6, silent = FALSE){



  ### definitions ###

  # set up GLS
  nlme::glsControl(maxIter = maxIter, msMaxIter = msMaxIter, tolerance = tolerance)

  # define regression equation
  reg_formula <- formula(reg_formula)

  # define correlation types
  corr_types <- list(nlme::corLin(  form = formula(error_formula)),
                     nlme::corGaus( form = formula(error_formula)),
                     nlme::corRatio(form = formula(error_formula)),
                     nlme::corSpher(form = formula(error_formula)),
                     nlme::corExp(  form = formula(error_formula)))



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

    if(isFALSE(silent)) print(paste0('Completed landcover ', landcover_vec[[i]], ' at ', Sys.time()))

  }



  ### keep only the results with the lowest RMSE for each landcover ###

  # get RMSEs for each land cover regression

  RMSE_fcn <- function(error) { sqrt(mean(error^2)) }   # RMSE function

  RMSEs <- foreach::foreach(i = 1:length(reg_results)) %dopar% {

    RMSEs_i <- list()
    for(j in 1:length(reg_results[[i]])){
      reg_ij <- reg_results[[i]][[j]]
      RMSEs_i[[j]] <- if(is.na(reg_ij)[[1]]){NA} else {RMSE_fcn(reg_ij$residuals)}
    }

    return(RMSEs_i)

  }

  # get the minimum RMSE from each landcover model
  RMSE_min <- sapply(RMSEs, which.min)

  # get final list of regression results, one for each landcover, corresponding to the model with lowest AIC
    # if regression has too few data points, it will return an error. If there is an error, return NA for that regression
  reg_results_final <- list()
  for(i in 1:length(reg_results)){ reg_results_final[[i]] <- try(reg_results[[i]][[RMSE_min[[i]]]], silent = TRUE)}
  for(i in 1:length(reg_results)){ reg_results_final[[i]] <- if(class(reg_results_final[[i]]) == 'try-error'){ reg_results_final[[i]] <- NA} else {reg_results_final[[i]] <- reg_results_final[[i]]} }

  # rename elements of `reg_results_final` to match the landcover codes
  names(reg_results_final) <- landcover_vec

  # replace `formula(reg_formula)` with the actual formula (needed for `gls_spatial_predict`) only if regression was run
  for(i in 1:length(reg_results_final)){
    if(!is.na(reg_results_final[[i]][[1]])){
      reg_results_final[[i]]$call[[2]] <- formula(reg_formula)
    } else {
      reg_results_final[[i]] <- NA
    }
  }

  # remove regression with no results and take note of them
  reg_failed_LCs <- names(which(is.na(reg_results_final)))
  reg_failed_LCs <- paste0('Regressions failed for landcover(s): ', reg_failed_LCs, '. Check that there are sufficient data points!')
  reg_results_final <- reg_results_final[lengths(reg_results_final)>1]



  ### plot distribution of residuals ###

  # get all residuals
  resids <- lapply(reg_results_final, function(x){ as.data.frame(x$residuals) })

  # add landcover column
  for(i in 1:length(resids)){ resids[[i]]$landcover <- names(resids)[[i]] }

  # rbind
  resids <- do.call('rbind', resids)
  colnames(resids) <- c('Residuals', 'Landcover')

  # plot
  resids_plot <- ggplot(data = resids) + geom_histogram(aes(Residuals, color = Landcover, fill = Landcover), alpha = 0.8) +
    theme(text = element_text(size = 15)) +
    geom_vline(xintercept = 0, linetype = 'longdash') +
    labs(y = 'Number of points') +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    scale_x_continuous(limits = c(min(resids$Residuals, na.rm = TRUE), max(resids$Residuals, na.rm = TRUE)))



  ### return the regression results and resid plot ###

  # combine results and plot
  reg_results_final <- list(reg_results_final, resids_plot, reg_failed_LCs)

  # name list elements
  names(reg_results_final) <- c('Regression results', 'Residuals plot', "Failed regressions")

  return(reg_results_final)

}

