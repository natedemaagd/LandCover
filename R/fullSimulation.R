#' Run full simulation, accepting all defaults
#'
#' This function runs the entire simulation
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
#'
#' @return A list of all objects returned by the individual functions
#'
#' @details
#'
#' @examples
#' TO DO
#'
#' @export


### FUNCTION:
fullSimulation <- function(data, landcover_varname, landcover_vec, reg_formula, error_formula, num_cores = parallel::detectCores() - 1, ){



  ##### run regression #####

  regression_results <- gls_spatial(data=data, landcover_varname=landcover_varname, landcover_vec=landcover_vec, reg_formula=reg_formula, error_formula=error_formula, num_cores=num_cores, )

}

