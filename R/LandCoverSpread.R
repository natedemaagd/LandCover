#' Run simulation of landcover spread and its effect on a dependent variable
#'
#' This function runs a simulation of landcover spread, using bootstrapped results. It returns a list of landcover rasters and a list of the rasters for your dependent variable; one for each year.
#'
#' @import nlme doParallel foreach parallel sp tidyr rgdal raster
#' @importFrom foreach %dopar%
#'
#' @param infest_val numerical. Value of the landcover that will be spreading.
#' @param suscep_val numerical. Vector of landcover values that are susceptible to the spread.
#' @param spread_rate numerical. Value between 0 and 1 indicating the annual spread rate of the invading landcover.
#' @param birdcell numerical. Value between 0 and 1 indicating the probability a random cell can be invaded, regardless of adjacency to existing invaded pixels.
#' @param simlength integer. Number of years the simulation should run.
#' @param simulation_count integer. Length of simulation bootstrap.
#' @param lc_raster raster. The initial landcover raster.
#' @param dep_var_raster_initial raster. The initial raster for the dependent variable. It is recommended you use a raster of predicted values rather than raw data (i.e. use `gls_spatial_predict()` with `landcover_val == 'ALL'` specified).
#' @param dep_var_raster_pred raster. A raster with predicted values for the dependent variable in all pixels susceptible to invasion. Usually a raster from `gls_spatial_predict()`.
#' @param dep_var_modifier numerical. A scalar to optionally return a list of rasters with modified dep_var rasters (e.g. multiply water yield rasters to obtain recharge rasters)
#' @param silent logical. Suppress printing notification when each simulation is done. Default is `FALSE`.
#'
#' @return Lists of rasters (landcover and dependent variable) and, optionally, raster files to a directory of your choosing.
#'
#' @details The output is a list of rasters: landcover over time, dep var over time, modified dep var over time (if specified), change in dep var over time, change in modified dep var over time (if specified), cumulative change in dep var, cumulative change in modified dep var (if specified)
#'
#' @examples
#' # load packages
#' library(LandCover); library(foreach)
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
#' # predict values of ET before and after invasion
#' pred_values <- gls_spatial_predict(data = dat, regression_results = regression_results, landcover_varname = 'landcover', landcover_invasive = 1, landcover_susceptible = 2,
#'                                    dep_varname = 'ET', x_coords_varname = 'x', y_coords_varname = 'y')
#'
#' # get landcover raster
#' lc_raster <- rasterFromXYZ(dat[c('x', 'y', 'landcover')])
#'
#' # run landcover simulation
#' landcover_sim <- LandCoverSpread(infest_val = 1, suscep_val = 2, spread_rate = 0.05, birdcell = 0, simlength = 15, simulation_count = 100,
#'                                  lc_raster = lc_raster, dep_var_raster_initial = pred_values$`Predicted values raster, current landcover`,
#'                                  dep_var_raster_pred = pred_values$`Predicted values raster, post-invasion`,
#'                                  dep_var_modifier = 0.80, silent = TRUE)
#'
#' @export


### FUNCTION:

LandCoverSpread <- function(infest_val, suscep_val, spread_rate, birdcell, simlength, simulation_count, lc_raster, dep_var_raster_initial, dep_var_raster_pred, dep_var_modifier = NA, silent = FALSE) {



  ##### function for one year #####

  growrate = spread_rate
  spreadrate = spread_rate

  one_year <- function(
    x,
    infested_value,
    susceptible_values,
    growth_rate = growrate,
    bird_rate = birdcell,
    old_aet = NULL,
    pred_aet = NULL,
    counter_layer = NULL
  ) {
    current_cells <- which(values(x) == infested_value)
    new_cells <- ceiling(length(current_cells) * growth_rate)

    adjacent_cells <- adjacent(x, which(values(x) == infested_value), directions = 8, pairs = FALSE)
    flippable_cells <- adjacent_cells[values(x)[adjacent_cells] %in% susceptible_values]
    if (length(flippable_cells) == 0) {
      return(list(
        #starting_cell_size = length(current_cells),
        #new_cells = new_cells,
        #flipped_cells = flipped_cells,
        #bird_cells = bird_cells,
        #bird_flipped_cells = bird_flips,
        infested_cell_count = sum(values(x) == infested_value, na.rm = TRUE),
        result = x,
        counter_layer = counter_layer,
        new_aet = old_aet
      ))
    }
    flipped_cells <- sample(flippable_cells, min(length(flippable_cells), new_cells))

    bird_cells <- sample(
      which(values(x) %in% c(infested_value, susceptible_values)),
      ceiling(length(current_cells) * bird_rate)
    )
    # bird_cells <- sample(which(values(x) %in% susceptible_values), birds) # definitely flip birds
    bird_flips <- bird_cells[which(values(x)[bird_cells] %in% susceptible_values)]

    y <- x
    y[flipped_cells] <- infested_value
    y[bird_flips] <- infested_value

    all_flipped_cells <- unique(c(flipped_cells, bird_flips))
    if (!is.null(counter_layer)) {
      counter_layer[all_flipped_cells] <- counter_layer[all_flipped_cells] + 1
    }

    old_aet[flipped_cells] <- values(pred_aet)[flipped_cells]
    old_aet[bird_flips] <- values(pred_aet)[bird_flips]

    return(list(
      #starting_cell_size = length(current_cells),
      #new_cells = new_cells,
      #flipped_cells = flipped_cells,
      #bird_cells = bird_cells,
      #bird_flipped_cells = bird_flips,
      infested_cell_count = sum(values(y) == infested_value, na.rm = TRUE),
      result = y,
      counter_layer = counter_layer,
      new_aet = old_aet
    ))
  }




  ##### run one iteration of the simulation #####

  one_simulation <- function(x, periods, list_of_counters, old_aet, pred_aet) {
    # extend all raster extents
    x <- extend(x, extend(old_aet, pred_aet))
    old_aet <- extend(old_aet, x)
    pred_aet <- extend(pred_aet, x)

    sum_aet = rep(NA, times = periods)
    infested_cells = rep(NA, times = periods)
    mean_aet = rep(NA, times = periods)
    median_aet = rep(NA, times = periods)

    # run one simulation
    for (i in 1:periods) {
      one_year_result <- one_year(x, infested_value = infest_val, susceptible_values = suscep_val,
                                  growth_rate = growrate, bird_rate = birdcell,
                                  counter_layer = list_of_counters[[i]],
                                  old_aet = old_aet, pred_aet = pred_aet)
      sum_aet[i] <- sum(values(one_year_result$new_aet), na.rm = TRUE)
      infested_cells[i] <- one_year_result$infested_cell_count
      mean_aet[i] <- mean(values(one_year_result$new_aet), na.rm = TRUE)
      median_aet[i] <- median(values(one_year_result$new_aet), na.rm = TRUE)
      list_of_counters[[i]] <- one_year_result$counter_layer
      list_of_ETrasters[[i]] <- one_year_result$new_aet

      # use values from one_year() result for use in the next iteration
      x <- one_year_result$result
      old_aet = one_year_result$new_aet
    }

    return(list(
      sum_aet = sum_aet,
      infested_cells = infested_cells,
      mean_aet = mean_aet,
      median_aet = median_aet,
      list_of_counters = list_of_counters,
      list_of_ETrasters = list_of_ETrasters,
      et = old_aet
    ))
  }




  ##### counter and ET raster list #####

  create_counter_list <- function(x, periods) {
    counter_layer <- x
    counter_layer[which(!is.na(values(counter_layer)))] <- 0

    list_of_counters = list()
    for (i in 1:periods) {
      list_of_counters <- c(list_of_counters, counter_layer)
    }
    return(list_of_counters)
  }

  create_ETraster_list <- function(x, periods) {
    ETraster_layer <- x

    list_of_ETrasters = list()
    for (i in 1:periods) {
      list_of_ETrasters <- c(list_of_ETrasters, ETraster_layer)
    }
    return(list_of_ETrasters)
  }




  ##### initiate output vectors #####

  list_of_counters  <- create_counter_list(lc_raster, periods = simlength)
  list_of_ETrasters <- create_ETraster_list(dep_var_raster_pred, periods = simlength)
  sum_aet_result <- c()
  infested_cell_counts <- c()
  mean_aet_result <- c()
  median_aet_result <- c()
  et_raster <- c()




  ##### for-loop simulation #####

  for (i in 1:simulation_count) {
    simulation_result <- one_simulation(lc_raster, periods = simlength, list_of_counters, old_aet = dep_var_raster_initial, pred_aet = dep_var_raster_pred)
    list_of_counters <- simulation_result$list_of_counters
    list_of_ETrasters <- simulation_result$list_of_ETrasters
    sum_aet_result <- rbind(sum_aet_result, simulation_result$sum_aet)
    infested_cell_counts <- rbind(infested_cell_counts, simulation_result$infested_cells)
    mean_aet_result <- rbind(mean_aet_result, simulation_result$mean_aet)
    median_aet_result <- rbind(median_aet_result, simulation_result$median_aet)
    et_raster[[i]] <- simulation_result$et
    if(isFALSE(silent)){print(paste0('Simulation ', i, ' of ', simulation_count, ' complete!'))}
  }




  ##### update counters #####

  list_of_summed_counters = list()
  for (i in 1:simlength) {
    list_of_summed_counters <- c(list_of_summed_counters, raster :: calc(stack(list_of_counters[1:i]), fun = sum))
  }




  ##### update results #####

  infested_value = infest_val
  susceptible_values = suscep_val
  spread_rate = spreadrate
  bird_rate = birdcell
  last_raster <- lc_raster
  summary_list_of_rasters <- list(last_raster)
  infested_cells <- rep(NA, 100)
  for (i in 1:simlength) {
    infested_cell_count <- sum(values(last_raster) == infested_value, na.rm = TRUE)
    adjacent_target_count <- ceiling(infested_cell_count * spread_rate)
    bird_target_count <- ceiling(infested_cell_count * bird_rate)

    # spread
    adjacent_cells <- adjacent(last_raster, which(values(last_raster) == infested_value),
                               directions = 8, pairs = FALSE)
    flippable_cells <- adjacent_cells[values(last_raster)[adjacent_cells] %in% susceptible_values]
    summed_counters_copy <- values(list_of_summed_counters[[i]])
    summed_counters_copy[-flippable_cells] <- 0
    adjacent_targets <- order(summed_counters_copy, decreasing = TRUE)[1:adjacent_target_count]
    last_raster[adjacent_targets[adjacent_targets %in% flippable_cells]] <- infested_value

    # birds
    bird_targets <- sample(which(values(last_raster) %in% c(infested_value, susceptible_values)), bird_target_count)
    last_raster[bird_targets] <- infested_value

    infested_cells[i] <- sum(values(last_raster) == infested_value, na.rm = TRUE)

    #writeRaster(last_raster, paste0("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/DWS/Land_cover/Data/Processed/Simulation_results_05pct_250m_LAI_CONSTANT_kohala/TIFs_05pct_250m/finalRRaster",simulation_label, 2017 + i, ".tif"), overwrite = TRUE)
    summary_list_of_rasters <- c(summary_list_of_rasters, last_raster)
  }




  ##### results #####


  # collect results so far
  list_of_lc_rasters      <- summary_list_of_rasters
  list_of_dep_var_rasters <- c(dep_var_raster_initial, list_of_ETrasters)


  # create list of rasters: change in dep_var relative to year 0
  list_of_dep_var_rasters_change_from_year_0 <- list()

    # first raster in list (year 0) should be raster of 0s
    raster_0s <- dep_var_raster_initial
    values(raster_0s) <- 0
    list_of_dep_var_rasters_change_from_year_0[[1]] <- raster_0s

  for(i in 2:(simlength+1)){

    list_of_dep_var_rasters_change_from_year_0[[i]] <- list_of_dep_var_rasters[[i]] - dep_var_raster_initial

  }


  # raster of cumulative change
  raster_dep_var_cumulative_change <- Reduce('+', list_of_dep_var_rasters_change_from_year_0)


  # return final results (with modified dep var if dep_var_modifier == TRUE)
  if(!is.na(dep_var_modifier)){

    # initiate list of modified depvar rasters (just the original depvar rasters)
    list_of_dep_var_rasters_modified                    <- c(dep_var_raster_initial, list_of_ETrasters)  # levels
    list_of_dep_var_rasters_change_from_year_0_modified <- list_of_dep_var_rasters_change_from_year_0    # changes

    # modify them
    list_of_dep_var_rasters_modified                    <- lapply(list_of_dep_var_rasters_modified,                    function(r){ r * dep_var_modifier })  # levels
    list_of_dep_var_rasters_change_from_year_0_modified <- lapply(list_of_dep_var_rasters_change_from_year_0_modified, function(r){ r * dep_var_modifier })  # changes
    raster_dep_var_cumulative_change_modified           <- raster_dep_var_cumulative_change * dep_var_modifier                                               # cumulative change

    # return results
    final_results <- list(list_of_landcover_rasters                           = list_of_lc_rasters,
                          list_of_dep_var_rasters                             = list_of_dep_var_rasters,
                          list_of_dep_var_rasters_modified                    = list_of_dep_var_rasters_modified,
                          list_of_dep_var_rasters_change_from_year_0          = list_of_dep_var_rasters_change_from_year_0,
                          list_of_dep_var_rasters_change_from_year_0_modified = list_of_dep_var_rasters_change_from_year_0_modified,
                          raster_dep_var_cumulative_change                    = raster_dep_var_cumulative_change,
                          raster_dep_var_cumulative_change_modified           = raster_dep_var_cumulative_change_modified)

    return(final_results)


  # if depvar modifier is missing, just return the landcover and depvar rasters
  } else {

    final_results <- list(list_of_landcover_rasters                           = list_of_lc_rasters,
                          list_of_dep_var_rasters                             = list_of_dep_var_rasters,
                          list_of_dep_var_rasters_change_from_year_0          = list_of_dep_var_rasters_change_from_year_0,
                          raster_dep_var_cumulative_change                    = raster_dep_var_cumulative_change)

    return(final_results)

  }

}
