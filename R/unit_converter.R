#' Convert the units of a LandCoverSpread simulation
#'
#' This function converts the units of all rasters (except landcover rasters) returned from the `LandCoverSpread` function.
#'
#'
#' @param fullSimulation_results list. Object returned from the `fullSimulation` or `fullSimulationApp` function.
#' @param unit_conversion_factor numerical. The value to be multiplied with all rasters for unit conversion.
#' @param dep_var_modified logical. Indicates whether the `dep_var` was modified in `LandCoverSpread`
#'
#' @return A list with the same length and contents as `fullSimulation`.
#'
#' @details This function simply multiplies the values of the values of the results of the simulation. If you modified the dependent variable in the simulation, you should specify `dep_var_modified = TRUE`.
#'
#' @examples
#'
#' @export


### FUNCTION:
fullSimulation_unit_converter <- function(fullSimulation_results, unit_conversion_factor, dep_var_modified = TRUE){


  #### modify DepvarSpread ----

  # dependent variable
  fullSimulation_results$DepvarSpread$depvar_list <- lapply(fullSimulation_results$DepvarSpread$depvar_list, function(x){ x * unit_conversion_factor })
  if(dep_var_modified){
    fullSimulation_results$DepvarSpread$depvar_list_modified <- lapply(fullSimulation_results$DepvarSpread_modified$depvar_list, function(x){ x * unit_conversion_factor })
  }

  # dependent variable change from year 0
  fullSimulation_results$DepvarSpread$depvar_list_change_from_year_0 <- lapply(fullSimulation_results$DepvarSpread$depvar_list_change_from_year_0, function(x){ x * unit_conversion_factor })
  if(dep_var_modified){
    fullSimulation_results$DepvarSpread$depvar_list_change_from_year_0_modified <- lapply(fullSimulation_results$DepvarSpread_modified$depvar_list_change_from_year_0, function(x){ x * unit_conversion_factor })
  }

  # dependent variable cumulative change
  fullSimulation_results$DepvarSpread$depvar_cumulative_change <- lapply(fullSimulation_results$DepvarSpread$depvar_cumulative_change, function(x){ x * unit_conversion_factor })
  if(dep_var_modified){
    fullSimulation_results$DepvarSpread$depvar_cumulative_change_modified <- lapply(fullSimulation_results$DepvarSpread_modified$depvar_cumulative_change, function(x){ x * unit_conversion_factor })
  }


  #### modify SimulationPlots


}
