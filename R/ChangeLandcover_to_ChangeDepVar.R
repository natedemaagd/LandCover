#' Run simulation of landcover spread and its effect on a dependent variable
#'
#' This function runs a simulation of landcover spread, using bootstrapped results. It returns a list of landcover rasters and a list of the rasters for your dependent variable; one for each year.
#'
#' @import nlme doParallel foreach parallel sp tidyr rgdal raster viridis
#' @importFrom foreach %dopar%
#'
#' @param landcover_list A list of yearly landcover rasters, typically from the LandCoverSpread function
#' @param infest_val numerical. Value of the landcover that will be spreading.
#' @param suscep_val numerical. Vector of landcover values that are susceptible to the spread.
#' @param dep_var_raster_initial raster. The initial raster for the dependent variable. It is recommended you use a raster of predicted values rather than raw data (i.e. use `gls_spatial_predict()` with `landcover_val == 'ALL'` specified).
#' @param dep_var_raster_pred raster. A raster with predicted values for the dependent variable in all pixels susceptible to invasion. Usually a raster from `gls_spatial_predict()`.
#' @param dep_var_modifier numerical. A scalar to optionally return a list of rasters with modified dep_var rasters (e.g. multiply water yield rasters to obtain recharge rasters)
#'
#' @return TO DO
#'
#' @details TO DO
#'
#' @examples
#' TO DO
#'
#' @export


ChangeLandcover_to_ChangeDepVar <- function(landcover_list, infest_val, suscep_val, dep_var_raster_initial, dep_var_raster_pred, dep_var_modifier){


  #### change yearly depvar values based on landcover ----

  # initiate list - one raster for each year
  depvar_list <- list(dep_var_raster_initial)[rep(1,length(landcover_list))]

  # for each year i, adjust depvar value if landcover changes from suscep to infest relative to first year
  for(i in 2:length(landcover_list)){

    # change value to NA if the cell is invaded
    depvar_list[[i]][landcover_list[[i]] == as.numeric(infest_val) & landcover_list[[1]] %in% as.numeric(suscep_val)] <- NA

    # fill all NAs with the predicted new dep var value
    depvar_list[[i]] <- cover(depvar_list[[i]], dep_var_raster_pred)
  }


  #### change in depvar relative to year 0

  # create list of rasters: change in dep_var relative to year 0
  depvar_list_change_from_year_0 <- list()

  # first raster in list (year 0) should be raster of 0s
  for(i in 1:(length(landcover_list))){

    depvar_list_change_from_year_0[[i]] <- depvar_list[[i]] - dep_var_raster_initial

  }


  #### raster of cumulative change ----
  depvar_cumulative_change <- Reduce('+', depvar_list_change_from_year_0)


  #### return final results ----

  #(with modified dep var if dep_var_modifier == TRUE)
  if(!is.na(dep_var_modifier)){

    # initiate list of modified depvar rasters (just the original depvar rasters)
    depvar_list_modified                    <- depvar_list                     # levels
    depvar_list_change_from_year_0_modified <- depvar_list_change_from_year_0  # changes

    # modify them
    depvar_list_modified                    <- lapply(depvar_list_modified,                    function(r){ r * dep_var_modifier })  # levels
    depvar_list_change_from_year_0_modified <- lapply(depvar_list_change_from_year_0_modified, function(r){ r * dep_var_modifier })  # changes
    depvar_cumulative_change_modified       <- depvar_cumulative_change * dep_var_modifier                                           # cumulative change

    # return results
    final_results <- list(depvar_list                             = depvar_list,
                          depvar_list_modified                    = depvar_list_modified,
                          depvar_list_change_from_year_0          = depvar_list_change_from_year_0,
                          depvar_list_change_from_year_0_modified = depvar_list_change_from_year_0_modified,
                          depvar_cumulative_change                = depvar_cumulative_change,
                          depvar_cumulative_change_modified       = depvar_cumulative_change_modified)

    return(final_results)


  # if depvar modifier is missing, just return the landcover and depvar rasters
  } else {

    final_results <- list(depvar_list                    = depvar_list,
                          depvar_list_change_from_year_0 = depvar_list_change_from_year_0,
                          depvar_cumulative_change       = depvar_cumulative_change)

    return(final_results)

  }

}
