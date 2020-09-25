#' Predict values from `gls_spatial()` output
#'
#' After creating `gls` object(s) with the `gls_spatial()` function, this will create predicted values of the dependent variable from your regression, for the specified landcovers. Particularly useful once your landcover files have changed and you want to predict what effect this may have on the dependent variable.
#'
#' @import ggplot2 rasterVis RColorBrewer
#'
#' @param raster a `raster` object. The raster you wish to plot
#' @param value_type character string. Specifies whether the values you are plotting are `'continuous'`, `'categorical'`, or `'priority'`. Default is `'continuous'`. See details.
#' @param blank_background logical. Do you want to remove the plot background (i.e. grid lines, tick marks, legend titles, etc.)? Default is `TRUE`
#' @param legend_title character string. The legend title. Default is blank.
#' @param font_size numerical. Font size of text in the plot. Default is 11.
#' @param break_at_zero logical. Should categories be split at 0? Works for continuous and priority plots. Default is `FALSE`.
#' @param priority_categories numerical. If `value_type = priority`, specifies the number of non-zero priority categories to plot. Default is 5. If `priority_colors` are not specified, max value is 9.
#' @param priority_outlier_value numerical. A value specifying an additional priority category for outliers. Can be either positive or negative.
#' @param decimal_points numerical. Specifies the number of decimal points to report in the legend. Default is 0.
#' @param priority_colors vector. If `value_type = priority`, a vector of colors of `length(priority_categories)+1` to customize priority category colors. The first value is for 'No change', and the rest are for the priority categories. Default is rainbow colors, with values of 0 being grayed out. If `priority_outlier_value` is provided, an additional color must be specified for the outlier category. That is, you must provide a vector of `length(priority_categories)+2` colors.
#' @param ... Pass additional arguments to adjust the legend and coloring (breaks, labels, limits, color/fill, etc.)
#' @param RColorBrewer_type character string. For `value_type = categorical`, specify the `RColorBrewer` `type`. See `?RColorBrewer`. Default is `'qual'`
#' @param RColorBrewer_palette character strong. For `value_type = categorical`, specify the `RColorBrewer` `palette`. See `?RColorBrewer`. Default is `'Dark2'`
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
#' dat$temp      <- with(dat, (120-0.7*(0.5*elevation + 0.3*y - 0.5*x + ifelse(landcover == 'lc1', -30, 0) + rnorm(nrow(dat)))))
#' dat$ET        <- with(dat, (   -0.4*(-2*temp       + 0.5*y - 1.0*x + ifelse(landcover == 'lc1', +20, 0) + rnorm(nrow(dat)))))
#'
#'
#' # run regression
#' regression_results <- gls_spatial(data = dat, landcover_varname = 'landcover', landcover_vec = c(1,2), reg_formula = ET ~ elevation + temp, error_formula = ~x+y)
#'
#' # predict ET assuming all pixels are landcover type 1, return raster
#' ET_predicted_lc1_raster  <- gls_spatial_predict(data = dat, reg_results = regression_results, landcover_varname = 'landcover', landcover_val = 1,
#'                                                 return_raster = TRUE, x_coords = dat$x, y_coords = dat$y)
#'
#' LandCoverPlot(ET_predicted_lc1_raster)
#' @export


### FUNCTION:
LandCoverPlot <- function(raster, value_type = 'continuous', blank_background = TRUE, legend_title = element_blank(), font_size = 11, break_at_zero = FALSE, priority_categories = 5, priority_outlier_value = NA, decimal_points = 0,
                          priority_colors = if(!is.na(priority_outlier_value)){c('lightgray', rev(rainbow(priority_categories+1)))} else {c('lightgray', rev(rainbow(priority_categories)))},
                          RColorBrewer_type = 'qual', RColorBrewer_palette = 'Dark2', ...){


  # initial plot
  main_plot <- gplot(raster) +

    coord_equal() +

    labs(fill = legend_title)


  ### adjust legend based on user inputs


  # plot continuous values - break at zero
  if(value_type == 'continuous' & isTRUE(break_at_zero)){

    main_plot <- main_plot + geom_raster(aes(fill = value)) + scale_fill_gradient2(...)

  }

  # plot continuous values - no break at zero
  if(value_type == 'continuous' & isFALSE(break_at_zero)){

    main_plot <- main_plot + geom_raster(aes(fill = value)) + scale_fill_continuous(...)

  }


  # plot categorical values

  n_vals <- length(unique(values(raster)))

  if(value_type == 'categorical'){

    main_plot <- main_plot + geom_raster(aes(fill = as.character(round(value)))) + scale_fill_brewer(type = RColorBrewer_type, palette = RColorBrewer_palette)

  }




  ##### priority plots #####


  # plot priority categories - no outlier cutoff provided
  if(value_type == 'priority' & is.na(priority_outlier_value)){


    # get all values of the raster
    raster_val = values(raster)[values(raster) > 0]


    # get breaks based on specified number of priority categories
    raster_val_breaks = quantile(raster_val, probs = seq(0, 1, length.out = priority_categories + 1), na.rm = TRUE)


    # create new raster with the priority values rather than continuous values
    raster2 <- raster
    values(raster2)[values(raster2) > 0] <- findInterval(raster_val, raster_val_breaks, all.inside = TRUE)


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


    # add 0 category to legend_labels
    legend_labels <- c('No change', legend_labels)


    # plot priority raster
    main_plot <- rasterVis::gplot(raster2) +
      geom_raster(aes(fill = as.character(value))) +
      coord_equal() +
      labs(fill = legend_title) +
      scale_fill_manual(labels = legend_labels, values = priority_colors)

  }


  # plot priority categories - outlier cutoff provided
  if(value_type == 'priority' & !is.na(priority_outlier_value)){


    # get all values of the raster
    raster_val = values(raster)[values(raster) != 0]


    # get breaks based on specified number of priority categories
    raster_val_breaks = quantile(raster_val, probs = seq(0, 1, length.out = priority_categories + 1), na.rm = TRUE)


    # add outlier value to the breaks
    raster_val_breaks <- c(raster_val_breaks, priority_outlier_value)
    raster_val_breaks <- raster_val_breaks[order(raster_val_breaks)]


    # create new raster with the priority values rather than continuous values
    raster2 <- raster
    values(raster2)[values(raster2) != 0] <- findInterval(raster_val, raster_val_breaks, all.inside = TRUE)


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


    # add 0 category to legend_labels, and add 'Outlier' in place of the outlier values, according to whether it's a positve or negative value
    if(priority_outlier_value < 0){

      legend_labels <- c('No change', 'Low-value outlier', legend_labels[-1])

    } else {

      legend_labels <- c('No change', legend_labels[-length(legend_labels)], 'High-value outlier')

    }


    # plot priority raster
    main_plot <- gplot(raster2) +
      geom_raster(aes(fill = as.character(value))) +
      coord_equal() +
      labs(fill = legend_title) +
      scale_fill_manual(labels = legend_labels, values = priority_colors)

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

