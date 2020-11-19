#' Run full simulation, accepting all defaults
#'
#' This function runs the entire simulation by wrapping all other functions into a single function, accepting all default customization options
#'
#' @import nlme doParallel foreach parallel viridis
#' @importFrom foreach %dopar%
#'
#' @param data `data.frame` with spatial data
#' @param landcover_varname character string specifying the landcover variable from `data`
#' @param landcover_vec vector of all landcover types in `data$landcover_varname` to be analyzed. This should include the landcover that is spreading along with all landcovers that are susceptible to the spread.
#' @param landcover_invasive value. Numerical or character value of the invasive landcover.
#' @param landcover_susceptible value. Numerical or character value(s) of the susceptible landcover(s). If more than one susceptible landcover, provide a vector `c()`.
#' @param reg_formula regression formula to be used, as in `lm()`, with columns from `data` (e.g. `c ~ a + b`)
#' @param error_formula one-sided formula specifying coordinate columns from `data` (e.g. `~ x + y`)
#' @param dep_varname character string. Name of the dependent variable in `data`.
#' @param x_coords_varname character string. Name of the x-coordinate variable in `data`.
#' @param y_coords_varname character string. Name of the y-coordinate variable in `data`.
#' @param spread_rate numerical. Value between 0 and 1 indicating the annual spread rate of the invading landcover.
#' @param birdcell numerical. Value between 0 and 1 indicating the probability a random cell can be invaded, regardless of adjacency to existing invaded pixels.
#' @param simlength integer. Number of years the simulation should run.
#' @param simulation_count integer. Length of simulation bootstrap.
#' @param dep_var_modifier numerical. A scalar to optionally return a list of rasters with modified dep_var rasters (e.g. multiply water yield rasters to obtain recharge rasters)
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
fullSimulation <- function(data,
                           shp_reg = NULL,
                           shp_app = NULL,
                           dat_sample = NULL,
                           landcover_varname,
                           landcover_vec,
                           reg_formula,
                           error_formula,
                           landcover_invasive,
                           landcover_susceptible,
                           dep_varname,
                           x_coords_varname,
                           y_coords_varname,
                           spread_rate,
                           birdcell,
                           simlength,
                           simulation_count,
                           dep_var_modifier,
                           num_cores = parallel::detectCores() - 1){




  ##### define variables

  # datSubset
  data=data
  shp_reg=shp_reg
  shp_app=shp_app
  dat_sample=dat_sample

  # gls_spatial
  data=data
  landcover_varname=landcover_varname
  landcover_vec=landcover_vec
  reg_formula=reg_formula
  error_formula=error_formula
  num_cores=num_cores

  # gls_spatial_predict
  landcover_invasive=landcover_invasive
  landcover_susceptible=landcover_susceptible

  # LandCoverSpread
  spread_rate=spread_rate
  birdcell=birdcell
  simlength=simlength
  simulation_count=simulation_count
  dep_var_modifier=dep_var_modifier




  ##### run full simulation

  # dat subset
  if(!is.null(shp_reg)){ data_subset <- datSubset(data=data, x=x_coords_varname, y=y_coords_varname, shp_reg=shp_reg, shp_app=shp_app, sample=sample) }

  # gls_spatial
  if(!is.null(shp_reg)){ data = data_subset$RegressionData}  # if a subset was used, only use the regression data subset
  regression_results <- gls_spatial(data=data, landcover_varname=landcover_varname, landcover_vec=landcover_vec, reg_formula=reg_formula, error_formula=error_formula, num_cores=num_cores, silent = TRUE)

  # gls_spatial_predict
  if(!is.null(shp_reg)){ data = dat_subset$SimulationData}  # if a subset was used, only use the simulation data subset
  predVals <- gls_spatial_predict(data=data, regression_results=regression_results, landcover_varname=landcover_varname, landcover_invasive=landcover_invasive, landcover_susceptible=landcover_susceptible,
                                  dep_varname=dep_varname, x_coords_varname=x_coords_varname, y_coords_varname=y_coords_varname)

  # LandCoverSpread
  lc_raster <- rasterFromXYZ(data[c('x', 'y', landcover_varname)])  # convert landcover to raster
  landcover_sim <- LandCoverSpread(infest_val=landcover_invasive, suscep_val=landcover_susceptible, spread_rate=spread_rate, birdcell=birdcell, simlength=simlength, simulation_count=simulation_count,
                                   lc_raster=lc_raster, dep_var_raster_initial=predVals$`Predicted values raster, current landcover`,
                                   dep_var_raster_pred=predVals$`Predicted values raster, post-invasion`,
                                   dep_var_modifier=dep_var_modifier, silent = TRUE)

  # SimulationPlots
  simPlots <- SimulationPlots(sim_results=landcover_sim, infest_val=landcover_invasive,
                              font_size = 15, n_grid = 6)

  # LandCoverPlot priority maps
  priorityPlots        <- list(LandCoverPlot(predVals$`Predicted values raster, change`,               value_type = 'priority', decimal_points = 2),
                               LandCoverPlot(landcover_sim$raster_dep_var_cumulative_change,           value_type = 'priority', decimal_points = 2),
                               LandCoverPlot(landcover_sim$raster_dep_var_cumulative_change_modified,  value_type = 'priority', decimal_points = 2))
  names(priorityPlots) <- c('Priority map, change in dep var', 'Priority map, cumulative dep var', 'Priority map, cumulative modified dep var')




  ##### return results

  results_list        <- list(regression_results, predVals, landcover_sim, simPlots, priorityPlots)
  names(results_list) <- c('gls_spatial', 'gls_spatial_predict', 'LandCoverSpread', 'SimulationPlots', 'PriorityPlots')

  return(results_list)


}

