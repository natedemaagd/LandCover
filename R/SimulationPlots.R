#' Plot figures associated with simulation of landcover spread
#'
#' This plots the results of the landcover simulation data created by `LandCoverSpread`.
#'
#' @import sp tidyr rgdal raster ggplot2 viridis ggpubr
#'
#' @param sim_results list. The results of the `LandCoverSpread` function.
#' @param infest_val numerical. The value of the invasive landcover.
#' @param suscep_val numerical. Vector of landcover values that are susceptible to the spread.
#' @param dep_var_modified logical. If `TRUE`, creates line graphs with added modified dependent variable from the simulation. Requires `dep_var_modifier` to be specified in `LandCoverSpread`.
#' @param color_labels character vector. If `dep_var_modified == TRUE`, specifies the labels for the dependent variable and the modified dependent variable.
#' @param font_size value. Font size of text in the figure.
#' @param line_thickness value. For the line graph, the thickness of the lines.
#' @param flip_colors logical. Flips the colors of the raster plots
#' @param decimal_places numerical. Number of decimal places to report on the legend of continuous plots.
#' @param n_grid numerical. If you want a grid of timelapse figures, specify the number of plots.
#'
#' @return A list of landcover plots and line plots associated with the landcover spread simulation.
#'
#' @details Included in the resulting list is a list of yearly landcover plots, a list of yearly dependent variable plots, and a line graph of yearly changes both the dependent variable and, if specified, the modified dependent variable.
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

SimulationPlots <- function(sim_results, infest_val, suscep_val, dep_var_modified = TRUE,
                            dep_var_label = 'dep_var',
                            dep_var_modified_label = 'dep_var_modified',
                            font_size = 15, line_thickness = 0.8,
                            line_colors = viridis(2), color_labels = c('Dep. var.', 'Modified dep. var.'),
                            flip_colors = FALSE,
                            decimal_places = 2,
                            infest_label = 'Invasive',
                            suscep_label = 'Susceptible',
                            n_grid = NA){




  ##### create list of landcover plots over time #####

  # initiate list
  lc_plot_timelapse <- list()

  # fill list
  for(i in 1:length(sim_results$list_of_landcover_rasters)){

    # create data.frame from raster
    df <- as.data.frame(sim_results$list_of_landcover_rasters[[i]], xy = TRUE)


    # convert to invasive or non-invasive, then remove cells that are NA
    df$landcover_category <- ifelse(df[,3] %in% infest_val, infest_label,
                             ifelse(df[,3] %in% suscep_val, suscep_label, 'Other'))

    # get colors for landcovers
    lc_colors <- viridis(length(unique(df$landcover_category)))
    if(isTRUE(flip_colors)){ lc_colors = rev(lc_colors) }

    # plot
    lc_plot_timelapse[[i]] <- ggplot(data = df[!is.na(df[,3]),]) +

      geom_raster(aes(x, y, fill = landcover_category)) +

      scale_fill_manual(values = lc_colors[1:length(unique(df$landcover_category))], name = 'Landcover') +

      coord_equal() +

      labs(title = paste0('Year ', i-1)) +

      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), text = element_text(size = font_size))

  }




  ##### create list of dep_var plots over time #####

  # get range of values for limits of color scale
  color_limits <- range(c(sapply(sim_results$list_of_dep_var_rasters_change_from_year_0, function(r){range(values(r))})), na.rm = TRUE)

  # set color scale according to whether the colors are flipped
  if(isFALSE(flip_colors)){

    fill_low  = '#440154FF'
    fill_mid  = 'lightgray'
    fill_high = '#FDE725FF'

  } else {

    fill_low  = '#FDE725FF'
    fill_mid  = 'lightgray'
    fill_high = '#440154FF'

  }

  # initiate list
  dep_var_plot_timelapse <- list()
  df_list                <- list()

  # fill list
  for(i in 1:length(sim_results$list_of_dep_var_rasters_change_from_year_0)){

    # create data.frame from raster
    df_list[[i]] <- as.data.frame(sim_results$list_of_dep_var_rasters_change_from_year_0[[i]], xy = TRUE)

    # create breaks and labels for the plots, while checking to see if any change in dependent variable
    legend_breaks = c(min(color_limits), mean(color_limits), max(color_limits))
    if(length(unique(legend_breaks))==1){ legend_breaks[[1]] = legend_breaks[[1]]-1*10^{-1*decimal_places}
                                          legend_breaks[[3]] = legend_breaks[[3]]+1*10^{-1*decimal_places} }
    legend_labels = format(round(legend_breaks, digits = decimal_places), nsmall = decimal_places)


    # plot
    df <- df_list[[i]]
    colnames(df)[[3]] <- 'layer'

    dep_var_plot_timelapse[[i]] <- ggplot(data = df[!is.na(df$layer),]) +

      geom_raster(aes(x, y, fill = layer)) +

      scale_fill_gradient2(name = dep_var_label, low = fill_low, mid = fill_mid, high = fill_high,
                           limits = range(legend_breaks),
                           breaks = legend_breaks,
                           labels = ) +

      coord_equal() +

      labs(title = paste0('Year ', i-1), fill = paste0('Change in ', dep_var_label, 'from year 0')) +

      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), text = element_text(size = font_size))

  }




  ##### create list of dep_var_modified plots over time, if specified #####

  if(isTRUE(dep_var_modified)){

    # get range of values for limits of color scale
    color_limits <- range(c(sapply(sim_results$list_of_dep_var_rasters_change_from_year_0_modified, function(r){range(values(r))})), na.rm = TRUE)

    # set color scale according to whether the colors are flipped
    if(isFALSE(flip_colors)){

      fill_low  = '#440154FF'
      fill_mid  = 'lightgray'
      fill_high = '#FDE725FF'

    } else {

      fill_low  = '#FDE725FF'
      fill_mid  = 'lightgray'
      fill_high = '#440154FF'

    }

    # initiate list
    dep_var_modified_plot_timelapse <- list()
    df_list                         <- list()

    # fill list
    for(i in 1:length(sim_results$list_of_dep_var_rasters_change_from_year_0_modified)){

      # create data.frame from raster
      df_list[[i]] <- as.data.frame(sim_results$list_of_dep_var_rasters_change_from_year_0_modified[[i]], xy = TRUE)

      # create breaks and labels for the plots, while checking to see if any change in dependent variable
      legend_breaks = c(min(color_limits), mean(color_limits), max(color_limits))
      if(length(unique(legend_breaks))==1){ legend_breaks[[1]] = legend_breaks[[1]]-1*10^{-1*decimal_places}
      legend_breaks[[3]] = legend_breaks[[3]]+1*10^{-1*decimal_places} }
      legend_labels = format(round(legend_breaks, digits = decimal_places), nsmall = decimal_places)

      # plot
      df <- df_list[[i]]
      colnames(df)[[3]] <- 'layer'

      dep_var_modified_plot_timelapse[[i]] <- ggplot(data = df[!is.na(df$layer),]) +

        geom_raster(aes(x, y, fill = layer)) +

        scale_fill_gradient2(name = dep_var_modified_label, low = fill_low, mid = fill_mid, high = fill_high,
                             limits = range(legend_breaks),
                             breaks = legend_breaks,
                             labels = legend_labels) +

        coord_equal() +

        labs(title = paste0('Year ', i-1), fill = paste0('Change in ', dep_var_label, 'from year 0')) +

        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), text = element_text(size = font_size))

    }

  }





  ##### create line graphs #####

  # if depvar is modified, create line graph with depvar and modified depvar
  if(isTRUE(dep_var_modified)){

    # get data
    LineGraphData <- data.frame(Year             = 0:(length(sim_results$list_of_landcover_rasters)-1                                                   ),
                                dep_var          = sapply(sim_results$list_of_dep_var_rasters_change_from_year_0,          function(r){ sum(values(r)) }),
                                dep_var_modified = sapply(sim_results$list_of_dep_var_rasters_change_from_year_0_modified, function(r){ sum(values(r)) }))


    # melt data
    LineGraphData_melt <- with(LineGraphData, data.frame(Year     = c(Year, Year),
                                                         variable = rep(color_labels, each = nrow(LineGraphData)),
                                                         value    = c(dep_var, dep_var_modified)))


    # plot
    LineGraph <- ggplot(data = LineGraphData_melt) + geom_line(aes(x = Year, y = value, color = variable), size = line_thickness) +

      scale_color_manual(values = line_colors, name = NULL) +

      theme(text = element_text(size = font_size)) +

      labs(y = dep_var_label)


  # if depvar isn't modified, just create line graph of depvar
  } else {

    # adjust colors if they're specified
    if(is.na(line_colors)){ line_colors = viridis(2)} else { line_colors = line_colors}

    # get data
    LineGraphData <- data.frame(Year             = 0:(length(sim_results$list_of_landcover_rasters)-1                                                   ),
                                dep_var          = sapply(sim_results$list_of_dep_var_rasters_change_from_year_0,          function(r){ sum(values(r)) }))


    # plot
    LineGraph <- ggplot(data = LineGraphData) + geom_line(aes(x = Year, y = dep_var), size = line_thickness) +

      scale_color_manual(values = line_colors) +

      theme(legend.position = NULL, text = element_text(size = font_size)) +

      labs(y = dep_var_label)

  }




  ##### grid plots #####

  # if n_grid is not NA, plot the lc grid
  if(!is.na(n_grid)){

    # get sequence from 1 to n_grid for plot indicies
    plot_indices <- round(seq(1, length(lc_plot_timelapse), length.out = n_grid), digits = 0)

    # create list of plots
    lc_plot_list <- list()
    for(i in 1:length(plot_indices)){

      lc_plot_list[[i]] <- lc_plot_timelapse[[ plot_indices[[i]] ]]

    }

    # plot grid
    lc_timelapse_grid <- ggpubr::ggarrange(plotlist = lc_plot_list, common.legend = TRUE, legend = 'right')


  }

  # if n_grid is not NA, plot the dep var grid
  if(!is.na(n_grid)){

    # get sequence from 1 to n_grid for plot indicies
    plot_indices <- round(seq(1, length(dep_var_plot_timelapse), length.out = n_grid), digits = 0)

    # create list of plots
    dep_var_plot_list <- list()
    for(i in 1:length(plot_indices)){

      dep_var_plot_list[[i]] <- dep_var_plot_timelapse[[ plot_indices[[i]] ]]

    }

    # plot grid
    dep_var_timelapse_grid <- ggpubr::ggarrange(plotlist = dep_var_plot_list, common.legend = TRUE, legend = 'right')


  }

  # if n_grid is not NA and dep_var_modified is not NA, plot the dep var modified grid
  if(!is.na(n_grid) & isTRUE(dep_var_modified)){

    # get sequence from 1 to n_grid for plot indicies
    plot_indices <- round(seq(1, length(dep_var_modified_plot_timelapse), length.out = n_grid), digits = 0)

    # create list of plots
    dep_var_modified_plot_list <- list()
    for(i in 1:length(plot_indices)){

      dep_var_modified_plot_list[[i]] <- dep_var_modified_plot_timelapse[[ plot_indices[[i]] ]]

    }

    # plot grid
    dep_var_modified_timelapse_grid <- ggpubr::ggarrange(plotlist = dep_var_modified_plot_list, common.legend = TRUE, legend = 'right')


  }





  ##### return results #####

  # return results no modified dep var and no grid
  if(isFALSE(dep_var_modified) & is.na(n_grid)){

    sim_plots_results <- list(lc_plot_timelapse               = lc_plot_timelapse,
                              dep_var_plot_timelapse          = dep_var_plot_timelapse,
                              line_graph                      = LineGraph)

    return(sim_plots_results)

  }

  # return results no modified dep var but with grid
  if(isFALSE(dep_var_modified) & !is.na(n_grid)){

    sim_plots_results <- list(lc_plot_timelapse               = lc_plot_timelapse,
                              lc_timelapse_grid               = lc_timelapse_grid,
                              dep_var_plot_timelapse          = dep_var_plot_timelapse,
                              dep_var_timelapse_grid          = dep_var_timelapse_grid,
                              line_graph                      = LineGraph)

    return(sim_plots_results)

  }

  # return results with modified dep_var but no grid
  if(isTRUE(dep_var_modified) & is.na(n_grid)){

    sim_plots_results <- list(lc_plot_timelapse               = lc_plot_timelapse,
                              dep_var_plot_timelapse          = dep_var_plot_timelapse,
                              dep_var_modified_plot_timelapse = dep_var_modified_plot_timelapse,
                              line_graph                      = LineGraph)

    return(sim_plots_results)

  }

  # return results with modified dep_var and with grid
  if(isTRUE(dep_var_modified) & !is.na(n_grid)){

    sim_plots_results <- list(lc_plot_timelapse               = lc_plot_timelapse,
                              lc_timelapse_grid               = lc_timelapse_grid,
                              dep_var_plot_timelapse          = dep_var_plot_timelapse,
                              dep_var_timelapse_grid          = dep_var_timelapse_grid,
                              dep_var_modified_plot_timelapse = dep_var_modified_plot_timelapse,
                              dep_var_modified_timelapse_grid = dep_var_modified_timelapse_grid,
                              line_graph                      = LineGraph)

    return(sim_plots_results)

  }

}
