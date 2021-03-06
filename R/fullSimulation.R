#' Run full simulation, accepting all defaults
#'
#' This function runs the entire simulation by wrapping all other functions into a single function, accepting all default customization options
#'
#' @import nlme doParallel foreach parallel viridis rgdal sp readxl
#' @importFrom foreach %dopar%
#'
#' @param data_as_directories logical. `TRUE` if data and shapefiles must be loaded from files. `FALSE` if data and shapefiles are `R` objects already loaded in the environment.
#' @param data `data.frame` if data is in environement, or character string specifying location of xlsx data with directory if `data_as_directories = TRUE`
#' @param shp_reg_directory character string specifying directory of shapefile outlining the area of `data` to be used for the regression. `NULL` if `data_as_directories = FALSE`
#' @param shp_reg_layer character string specifying layer (.shp) filename corresponding to `shp_reg_directory`, or spatial polygon object if `data_as_directories = FALSE`
#' @param shp_app_directory character string specifying directory of shapefile outlining the area of `data` to which the final simulation will be applied, if different from `shp_reg`.  `NULL` if `data_as_directories = FALSE`
#' @param shp_app_layer character string specifying layer (.shp) filename corresponding to `shp_app_directory`, or spatial polygon object if `data_as_directories = FALSE`
#' @param convertFromUTM logical. Set to `TRUE` if you are subsetting your data with shapefile(s) and your shapefile(s) are in UTM coordinates instead of lon/lat.
#' @param dat_sample numerical. If specified, will take a sample of `n` observations for the regression (suggested if `shp_reg` has lots of observations)
#' @param landcover_varname character string specifying the landcover variable from `data`
#' @param landcover_invasive value. Numerical or character value of the invasive landcover.
#' @param landcover_susceptible value. Numerical or character value(s) of the susceptible landcover(s). If more than one susceptible landcover, provide a vector `c()`.
#' @param reg_formula regression formula to be used, as in `lm()`, with columns from `data` (e.g. `c ~ a + b`)
#' @param x_coords_varname character string. Name of the x-coordinate variable in `data`.
#' @param y_coords_varname character string. Name of the y-coordinate variable in `data`.
#' @param spread_rate numerical. Value between 0 and 1 indicating the annual spread rate of the invading landcover.
#' @param birdcell numerical. Value between 0 and 1 indicating the probability a random cell can be invaded, regardless of adjacency to existing invaded pixels.
#' @param simlength integer. Number of years the simulation should run.
#' @param simulation_count integer. Length of simulation bootstrap.
#' @param dep_var_modifier numerical. A scalar to optionally return a list of rasters with modified dep_var rasters (e.g. multiply water yield rasters to obtain recharge rasters)
#' @param covar_adjustment list. List specifying change in covariate values. See ?gls_spatial_predict.
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
fullSimulation <- function(data_as_directories = FALSE,
                           data,
                           shp_reg_directory = NULL,
                           shp_reg_layer = NULL,
                           shp_app_directory = NULL,
                           shp_app_layer = NULL,
                           convertFromUTM = FALSE,
                           dat_sample = NULL,
                           landcover_varname,
                           reg_formula,
                           landcover_invasive,
                           landcover_susceptible,
                           x_coords_varname,
                           y_coords_varname,
                           spread_rate,
                           birdcell,
                           simlength,
                           simulation_count = 100,
                           dep_var_modifier,
                           covar_adjustment = NA,
                           num_cores = parallel::detectCores() - 1){




  ##### define variables

  # if data are directories, load from directories. Otherwise, load from environment
  if(data_as_directories){

    data = readxl::read_xlsx(data)
    if(exists('shp_reg_directory')){ if(!is.null(shp_reg_directory)){ shp_reg = readOGR(dsn = shp_reg_directory, layer = shp_reg_layer) }}
    if(exists('shp_app_directory')){ if(!is.null(shp_app_directory)){ shp_app = readOGR(dsn = shp_app_directory, layer = shp_app_layer) }}

  } else {

    data=data
    shp_reg=shp_reg_layer
    shp_app=shp_app_layer

  }

  # if a shapefile is missing, set its loaded version as NA
  if(is.null(shp_app_layer)){shp_app <- NULL}
  if(is.null(shp_reg_layer)){shp_reg <- NULL}

  # datSubset
  dat_sample=dat_sample

  # gls_spatial
  landcover_varname=landcover_varname
  landcover_vec=c(landcover_invasive, landcover_susceptible)  # determine which LCs to run regression on based on provided invasive and susceptible landcovers
  reg_formula=reg_formula
  error_formula=paste0('~', x_coords_varname, '+', y_coords_varname)  # construct error formula from provided x and y coordinate variable names
  num_cores=num_cores

  # gls_spatial_predict
  landcover_invasive=landcover_invasive
  landcover_susceptible=landcover_susceptible
  dep_varname=sub("\\~.*", "", gsub(" ", "", Reduce(paste, deparse(reg_formula))))  # take dep_varname from the regression formula already provided
  covar_adjustment=covar_adjustment

  # LandCoverSpread
  spread_rate=spread_rate
  birdcell=birdcell
  simlength=simlength
  simulation_count=simulation_count
  dep_var_modifier=dep_var_modifier




  ##### run full simulation



  ### dat subset - subset if specified, and then only if requested sample size is less than the number of pixels in `shp_reg`

  # if regression shapefile provided, sample data
  if(!is.null(shp_reg_layer)){

    data_regression <- datSubset(data=data, x_coords_varname=x_coords_varname, y_coords_varname=y_coords_varname, shp_reg=shp_reg, shp_app=shp_app, sample=dat_sample)
    data_regression <- data_regression$RegressionData
  }

  # if landcover application shapefile provided, subset data
  if(!is.null(shp_app_layer)){

    if(is.null(shp_reg)){
      data_app <- datSubset(data=data, x_coords_varname=x_coords_varname, y_coords_varname=y_coords_varname, shp_reg=shp_app, shp_app=shp_app, sample=dat_sample)
      data_app <- data_app$SimulationData
    }

    if(!is.null(shp_reg)){
      data_app <- datSubset(data=data, x_coords_varname=x_coords_varname, y_coords_varname=y_coords_varname, shp_reg=shp_reg, shp_app=shp_app, sample=dat_sample)
      data_regression <- data_app$RegressionData
      data_app        <- data_app$SimulationData
    }

  }

  # if no shapefiles are provided, sample only using sample size, if it's provided
  if(is.null(shp_reg_layer) & is.null(shp_app_layer)){

    data_regression <- data
    data_app        <- data

    if(!is.null(dat_sample) & dat_sample < nrow(data_regression)){
      data_regression <- data_regression[sample(nrow(data_regression), dat_sample),]
    }

  }

  # if one or the other shapefiles aren't provided...
  if( is.null(shp_reg_layer) & !is.null(shp_app_layer)){
    data_regression <- data_app
  }
  if(!is.null(shp_reg_layer) &  is.null(shp_app_layer)){
    shp_app  <- shp_reg
    data_app <- data_regression
  }




  # convert all data from tibbles to data.frames
  data <- as.data.frame(data)
  data_regression <- as.data.frame(data_regression)
  data_app <- as.data.frame(data_app)

  # convert landcover codes to character
  data[,landcover_varname] <- as.character(data[,landcover_varname])
  data_regression[,landcover_varname] <- as.character(data_regression[,landcover_varname])
  data_app[,landcover_varname] <- as.character(data_app[,landcover_varname])
  landcover_invasive    <- as.character(landcover_invasive)
  landcover_susceptible <- as.character(landcover_susceptible)

  # gls_spatial
  regression_results <- gls_spatial(data=data_regression, landcover_varname=landcover_varname, landcover_vec=landcover_vec, reg_formula=reg_formula, error_formula=error_formula, num_cores=num_cores, silent = TRUE)

  # gls_spatial_predict
  predVals <- gls_spatial_predict(data=data_app, regression_results=regression_results, landcover_varname=landcover_varname, landcover_invasive=landcover_invasive, landcover_susceptible=landcover_susceptible,
                                  dep_varname=dep_varname, x_coords_varname=x_coords_varname, y_coords_varname=y_coords_varname, covar_adjustment=covar_adjustment)

  # LandCoverSpread
  lc_raster <- raster::rasterFromXYZ(data_app[c('x', 'y', landcover_varname)])  # convert landcover to raster
  landcover_sim <- LandCoverSpread(infest_val=landcover_invasive, suscep_val=landcover_susceptible, spread_rate=spread_rate, birdcell=birdcell, simlength=simlength, simulation_count=simulation_count,
                                   lc_raster=lc_raster, dep_var_raster_initial=predVals$`Predicted values raster, current landcover`,
                                   dep_var_raster_pred=predVals$`Predicted values raster, post-invasion`,
                                   dep_var_modifier=dep_var_modifier, silent = TRUE)

  # ChangeLandcover_to_ChangeDepVar
  depvar_sim <- ChangeLandcover_to_ChangeDepVar(landcover_list=landcover_sim$list_of_landcover_rasters, infest_val=landcover_invasive, suscep_val=landcover_susceptible,
                                                dep_var_raster_initial=predVals$`Predicted values raster, current landcover`, dep_var_raster_pred=predVals$`Predicted values raster, post-invasion`,
                                                dep_var_modifier=dep_var_modifier)

  # SimulationPlots
  simPlots <- SimulationPlots(landcover_sim_results=landcover_sim, depvar_sim_results=depvar_sim, infest_val=landcover_invasive, suscep_val=landcover_susceptible,
                              font_size = 15, n_grid = 6)

  # LandCoverPlot priority maps
  priorityPlots        <- list(LandCoverPlot(predVals$`Predicted values raster, change`,    value_type = 'priority', decimal_points = 2),
                               LandCoverPlot(depvar_sim$depvar_cumulative_change,           value_type = 'priority', decimal_points = 2),
                               LandCoverPlot(depvar_sim$depvar_cumulative_change_modified,  value_type = 'priority', decimal_points = 2))
  names(priorityPlots) <- c('Priority map, change in dep var', 'Priority map, cumulative dep var', 'Priority map, cumulative modified dep var')

  # plot original landcover and shapefile(s)

    # first create new data - full region with landcover type (susceptible, invasive, or other)
    data_lctypes <- data
    data_lctypes$`Landcover type` <- ifelse(data_lctypes[,landcover_varname] == landcover_invasive, 'Invasive', ifelse(data_lctypes[,landcover_varname] %in% landcover_susceptible, 'Susceptible', 'Other'))

  if(is.null(shp_reg) & is.null(shp_app)){
    landcover_shp_plot <- ggplot(data = data_lctypes, aes_string(x = x_coords_varname, y = y_coords_varname)) + geom_raster(aes(fill = `Landcover type`), alpha = 0.7) + coord_equal() +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(),
            text = element_text(size = 15)) +
      scale_fill_viridis_d()
  }
  if(!is.null(shp_reg) & !is.null(shp_app)){
    landcover_shp_plot <- ggplot(data = data_lctypes, aes_string(x = x_coords_varname, y = y_coords_varname)) + geom_raster(aes(fill = `Landcover type`), alpha = 0.7) + coord_equal() +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(),
            text = element_text(size = 15)) +
      scale_fill_viridis_d() +
      geom_polygon(data = shp_reg, aes(x = long, y = lat), color = 'red',  fill = 'transparent') +
      geom_polygon(data = shp_app, aes(x = long, y = lat), color = 'blue', fill = 'transparent') +
      labs(caption = 'Red line is regression region, blue region is simulation region')
  }




  ##### return results

  results_list        <- list(landcover_shp_plot, regression_results, predVals, landcover_sim, depvar_sim, simPlots, priorityPlots)
  names(results_list) <- c('preview_plot', 'gls_spatial', 'gls_spatial_predict', 'LandCoverSpread', 'DepvarSpread', 'SimulationPlots', 'PriorityPlots')

  return(results_list)


}

