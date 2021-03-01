#' Create various plots associated with the landcover data.
#'
#' This function is meant to create various plots associted with the landcover data, both before and after the simulation of the spread. Note that plots related specifically to the timelapse itself (yearly landcover, line graphs) are better plotted with the `SimulationPlots` function.
#'
#' @import ggplot2 rasterVis RColorBrewer viridis
#'
#' @param raster a `raster` object. The raster you wish to plot
#' @param value_type character string. Specifies whether the values you are plotting are `'continuous'`, `'categorical'`, or `'priority'`. Default is `'continuous'`. See details.
#' @param blank_background logical. Do you want to remove the plot background (i.e. grid lines, tick marks, legend titles, etc.)? Default is `TRUE`
#' @param legend_title character string. The legend title. Default is blank.
#' @param font_size numerical. Font size of text in the plot. Default is 11.
#' @param break_at_zero logical. Should categories be split at 0? Works for continuous and priority plots. Default is `FALSE`.
#' @param priority_categories numerical. If `value_type = 'priority'`, specifies the number of non-zero priority categories to plot. Default is 5. If `priority_colors` are not specified, max value is 9.
#' @param priority_outlier_value numerical. A value specifying an additional priority category for outliers. Can be either positive or negative.
#' @param decimal_points numerical. Specifies the number of decimal points to report in the legend. Default is 0.
#' @param flip_colors logical. Should the priority colors be flipped? Default is `FALSE`. Useful for negative values.
#' @param ... Pass additional arguments to adjust the legend and coloring (breaks, labels, limits, color/fill, etc.)
#' @param RColorBrewer_type character string. For `value_type = 'categorical'`, specify the `RColorBrewer` `type`. See `?RColorBrewer`. Default is `'qual'`
#' @param RColorBrewer_palette character string. For `value_type = 'categorical'`, specify the `RColorBrewer` `palette`. See `?RColorBrewer`. Default is `'Dark2'`
#' @param continuous_type character string. For `value_type = 'continuous'`, specify color scale. See `?scale_fill_continuous`. Default is colorblind-friendly `viridis`.
#' @param continuous_break0_low character string. For `value_type = 'continuous'` and `break_at_zero = TRUE`, set the low color. See `?scale_color_gradient2`.
#' @param continuous_break0_high character string. For `value_type = 'continuous'` and `break_at_zero = TRUE`, set the high color. See `?scale_color_gradient2`.
#' @param continuous_break0_mid character string. For `value_type = 'continuous'` and `break_at_zero = TRUE`, set the color for values of 0. See `?scale_color_gradient2`.
#' @param categorical_direction numerical. For `value_type = 'categorical'`, flip the direction of the color scale by setting the value to `-1`.
#'
#' @return A a `ggplot` object
#'
#' @details See ?ggplot and ?scale_fill_continuous and ?scale_fill_manual for details about plotting with ggplot and adjusting the legend and colors
#'
#' @examples
#' set.seed(1)
#' library(LandCover); library(gridExtra); library(ggplot2); library(raster); library(foreach); library(rasterVis)
#'
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
#'
#' # run regression
#' regression_results <- gls_spatial(data = dat, landcover_varname = 'landcover', landcover_vec = c(1,2), reg_formula = ET ~ elevation + temp, error_formula = ~x+y)
#'
#' # get predicted values data
#' predicted_values <- gls_spatial_predict(data = dat, regression_results = regression_results, landcover_varname = 'landcover', landcover_invasive = 1, landcover_susceptible = 2, dep_varname = 'ET',
#'                                         x_coords_varname = 'x', y_coords_varname = 'y')
#'
#' LandCoverPlot(ET_predicted_lc1_raster)
#' @export


### FUNCTION:
LandCoverPlot <- function(raster, value_type = 'continuous', blank_background = TRUE, legend_title = element_blank(), font_size = 15, break_at_zero = FALSE, priority_categories = 5, priority_outlier_value = NA,
                          decimal_points = 0,
                          flip_colors = FALSE,
                          RColorBrewer_type = 'qual', RColorBrewer_palette = 'Dark2',
                          continuous_type = 'viridis',
                          continuous_break0_low = '#440154FF',
                          continuous_break0_high = '#FDE725FF',
                          continuous_break0_mid  = 'lightgray',
                          categorical_direction = 1){


  # initial plot
  main_plot <- rasterVis::gplot(raster) +

    coord_equal() +

    labs(fill = legend_title)


  ### adjust legend based on user inputs


  # plot continuous values - break at zero
  if(value_type == 'continuous' & isTRUE(break_at_zero)){

    main_plot <- main_plot + geom_raster(aes(fill = value)) + scale_fill_gradient2(low = continuous_break0_low, high = continuous_break0_high, mid = continuous_break0_mid,
                                                                                   limits = c(min(values(raster), na.rm = TRUE),
                                                                                              max(values(raster), na.rm = TRUE)),
                                                                                   breaks = c(min(values(raster), na.rm = TRUE),
                                                                                              (min(values(raster), na.rm = TRUE) + max(values(raster), na.rm = TRUE))/2,
                                                                                              max(values(raster), na.rm = TRUE)),
                                                                                   labels = format(round(c(min(values(raster), na.rm = TRUE),
                                                                                                           (min(values(raster, na.rm = TRUE)) + max(values(raster), na.rm = TRUE))/2,
                                                                                                           max(values(raster), na.rm = TRUE)),
                                                                                                         digits = decimal_points),
                                                                                                   nsmall = decimal_points))

  }

  # plot continuous values - no break at zero
  if(value_type == 'continuous' & isFALSE(break_at_zero)){

    main_plot <- main_plot + geom_raster(aes(fill = value)) + scale_fill_continuous(type = continuous_type,
                                                                                    limits = c(min(values(raster), na.rm = TRUE),
                                                                                               max(values(raster), na.rm = TRUE)),
                                                                                    breaks = c(min(values(raster), na.rm = TRUE),
                                                                                               (min(values(raster), na.rm = TRUE) + max(values(raster), na.rm = TRUE))/2,
                                                                                               max(values(raster), na.rm = TRUE)),
                                                                                    labels = format(round(c(min(values(raster), na.rm = TRUE),
                                                                                                            (min(values(raster), na.rm = TRUE) + max(values(raster), na.rm = TRUE))/2,
                                                                                                            max(values(raster), na.rm = TRUE)),
                                                                                                          digits = decimal_points),
                                                                                                    nsmall = decimal_points))

  }


  # plot categorical values

  n_vals <- length(unique(values(raster)))

  if(value_type == 'categorical'){

    main_plot <- main_plot + geom_raster(aes(fill = as.character(round(value)))) + scale_fill_viridis_d(direction = categorical_direction)

  }




  ##### priority plots #####


  # plot priority categories - no outlier cutoff provided
  if(value_type == 'priority' & is.na(priority_outlier_value)){


    # get all values of the raster
    raster_val = raster::values(raster)

    # get breaks based on specified number of priority categories
    raster_val_breaks = quantile(raster_val[raster_val != 0 & !is.na(raster_val)], probs = seq(0, 1, length.out = priority_categories + 1), na.rm = TRUE)
    if(break_at_zero) raster_val_breaks = c(0, raster_val_breaks); raster_val_breaks = raster_val_breaks[order(raster_val_breaks)]

    # replace raster_val with interval from rater_val_breaks
    raster_val_interval <- findInterval(raster_val, raster_val_breaks, all.inside = TRUE)

    # create new raster with the priority values rather than continuous values
    raster2 <- raster
    values(raster2) <- raster_val_interval

    # replace values of 0 in case one category includes values above and below 0
    raster2[raster == 0] <- 0

    # create labels based on priority category cutoffs
    legend_labels <- list()
    for(i in 1:( length(raster_val_breaks)-1 )){

      if(i == 1){
        legend_labels[[i]] <- paste0(format(round(raster_val_breaks[[i]], digits = decimal_points), nsmall = decimal_points), ' to ',
                                     format(round(raster_val_breaks[[i+1]], digits = decimal_points), nsmall = decimal_points))
      }

      else{
        legend_labels[[i]] <- paste0(format(round(raster_val_breaks[[i]]+ 1*10^(-1*decimal_points), digits = decimal_points), nsmall = decimal_points), ' to ',
                                     format(round(raster_val_breaks[[i+1]], digits = decimal_points), nsmall = decimal_points))
      }


    }


    # add 0 category to legend_labels
    legend_labels <- c('No change', legend_labels)


    # create vector of colors depending on if a outlier value is provided and if legend is to be split at 0
    if(!is.na(priority_outlier_value) & !break_at_zero){
      priority_colors <- c('lightgray', viridis::viridis(priority_categories+1))
    } else if(is.na(priority_outlier_value) & !break_at_zero){
      priority_colors <- c('lightgray', viridis::viridis(priority_categories))
    } else if(is.na(priority_outlier_value) & break_at_zero){
      priority_colors <- c('lightgray', viridis::viridis(priority_categories+1))
    } else if(!is.na(priority_outlier_value) & break_at_zero){
      priority_colors <- c('lightgray', viridis::viridis(priority_categories+2))
    }


    # convert raster2 to a data.frame to get rid of NAs
    raster2_df <- as.data.frame(raster2, xy = TRUE)
    raster2_df <- raster2_df[!is.na(raster2_df[,3]),]
    colnames(raster2_df) <- c('x', 'y', 'value')


    # plot priority raster according to whether the colors are flipped
    if(isTRUE(flip_colors)){

      main_plot <- ggplot(raster2_df) +
        geom_raster(aes(x = x, y = y, fill = as.character(value))) +
        coord_equal() +
        labs(fill = legend_title) +
        scale_fill_manual(labels = legend_labels, values = c(priority_colors[[1]], rev(priority_colors[2:length(priority_colors)])), na.translate = FALSE)

    } else {

      main_plot <- ggplot(raster2_df) +
        geom_raster(aes(x = x, y = y, fill = as.character(value))) +
        coord_equal() +
        labs(fill = legend_title) +
        scale_fill_manual(labels = legend_labels, values = priority_colors, na.translate = FALSE)

    }

  }


  # plot priority categories - outlier cutoff provided
  if(value_type == 'priority' & !is.na(priority_outlier_value)){


    # get all values of the raster
    raster_val = raster::values(raster)

    # get breaks based on specified number of priority categories
    raster_val_breaks = quantile(raster_val[raster_val != 0 & !is.na(raster_val)], probs = seq(0, 1, length.out = priority_categories + 1), na.rm = TRUE)
    if(break_at_zero) raster_val_breaks = c(0, raster_val_breaks); raster_val_breaks = raster_val_breaks[order(raster_val_breaks)]

    # add outlier value to the breaks
    raster_val_breaks <- c(raster_val_breaks, priority_outlier_value)
    raster_val_breaks <- raster_val_breaks[order(raster_val_breaks)]

    # replace raster_val with interval from rater_val_breaks
    raster_val_interval <- findInterval(raster_val, raster_val_breaks, all.inside = TRUE)

    # create new raster with the priority values rather than continuous values
    raster2 <- raster
    values(raster2) <- raster_val_interval

    # replace values of 0 in case one category includes values above and below 0
    raster2[raster == 0] <- 0


    # create labels based on priority category cutoffs
    legend_labels <- list()
    for(i in 1:( length(raster_val_breaks)-1 )){

      if(i ==1){
        legend_labels[[i]] <- paste0(format(round(raster_val_breaks[[i]], digits = decimal_points), nsmall = decimal_points), ' to ',
                                     format(round(raster_val_breaks[[i+1]], digits = decimal_points), nsmall = decimal_points))
      }

      else{
        legend_labels[[i]] <- paste0(format(round(raster_val_breaks[[i]]+ 1*10^(-1*decimal_points), digits = decimal_points), nsmall = decimal_points), ' to ',
                                     format(round(raster_val_breaks[[i+1]], digits = decimal_points), nsmall = decimal_points))
      }


    }


    # add 0 category to legend_labels and add 'Outlier' in place of the outlier values, according to whether it's a positve or negative value
    if(priority_outlier_value < 0){

      legend_labels <- c('No change', 'Low-value outlier', legend_labels[-1])

    } else {

      legend_labels <- c('No change', legend_labels[-length(legend_labels)], 'High-value outlier')

    }


    # create vector of colors depending on if a outlier value is provided and if legend is to be split at 0
    if(!is.na(priority_outlier_value) & !break_at_zero){
      priority_colors <- c('lightgray', viridis::viridis(priority_categories+1))
    } else if(is.na(priority_outlier_value) & !break_at_zero){
      priority_colors <- c('lightgray', viridis::viridis(priority_categories))
    } else if(is.na(priority_outlier_value) & break_at_zero){
      priority_colors <- c('lightgray', viridis::viridis(priority_categories+1))
    } else if(!is.na(priority_outlier_value) & break_at_zero){
      priority_colors <- c('lightgray', viridis::viridis(priority_categories+1))
    }


    # convert raster2 to a data.frame to get rid of NAs
    raster2_df <- as.data.frame(raster2, xy = TRUE)
    raster2_df <- raster2_df[!is.na(raster2_df[,3]),]
    colnames(raster2_df) <- c('x', 'y', 'value')


    # plot priority raster according to whether the colors are flipped
    if(isTRUE(flip_colors)){

      main_plot <- ggplot(raster2_df) +
        geom_raster(aes(x = x, y = y, fill = as.character(value))) +
        coord_equal() +
        labs(fill = legend_title) +
        scale_fill_manual(labels = legend_labels, values = priority_colors, na.value='transparent')

    } else {

      main_plot <- ggplot(raster2_df) +
        geom_raster(aes(x = x, y = y, fill = as.character(value))) +
        coord_equal() +
        labs(fill = legend_title) +
        scale_fill_manual(labels = legend_labels, values = priority_colors, na.value='transparent')

    }

  }


  # remove background if requested
  if(blank_background == TRUE){

    main_plot <- main_plot + theme(panel.background = element_blank(),
                                   axis.text = element_blank(),
                                   axis.ticks = element_blank(),
                                   axis.title = element_blank(),
                                   text = element_text(size = font_size))

  } else {

    main_plot <- main_plot + theme(text = element_text(size = font_size))

  }

  return(main_plot)

}

