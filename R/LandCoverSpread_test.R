set.seed(1)
library(LandCover); library(gridExtra); library(ggplot2); library(raster); library(foreach)
library(sp); library(tidyr); library(rgdal); library(tidyverse); library(viridis); library(ggpubr)




# initialize data.frame with coordinates
dat <- expand.grid(x = 1:20, y = 1:20, KEEP.OUT.ATTRS = FALSE)
# create some data: elevation, landcover, and temp/ET dependent on elevation and landcover
dat$elevation <- with(dat, 50 + 2*x + 5*y + rnorm(nrow(dat), sd = 7))
dat$landcover <- ifelse(dat$elevation < median(dat$elevation), 1, 2)
dat[dat$x < median(dat$x) & dat$landcover == 2, 'landcover'] <- 3
dat$temp      <- with(dat, (120-0.7*(0.5*elevation + 0.3*y - 0.5*x + ifelse(landcover == 'lc1', -30, 0) + rnorm(nrow(dat)))))
dat$ET        <- with(dat, (   -0.4*(-2*temp       + 0.5*y - 1.0*x + ifelse(landcover == 'lc1', +20, 0) + rnorm(nrow(dat)))))




# run regression
regression_results <- gls_spatial(data = dat, landcover_varname = 'landcover', landcover_vec = c(1,2), reg_formula = ET ~ elevation + temp, error_formula = ~x+y)




# predict values of ET before and after invasion
pred_values <- gls_spatial_predict(data = dat, regression_results = regression_results, landcover_varname = 'landcover', landcover_invasive = 1, landcover_susceptible = 2,
                                   dep_varname = 'ET', x_coords_varname = 'x', y_coords_varname = 'y', covar_adjustment  = list('temp', 80))
# add predicted values to `dat`
dat$ET_predicted_current <- pred_values$`Predicted values, current landcover`
dat$ET_predicted_invaded <- pred_values$`Predicted values, post-invasion`
# add change in ET based on predicted values
dat$ET_change <- dat$ET_predicted_invaded - dat$ET_predicted_current









# get landcover raster
lc_raster <- rasterFromXYZ(dat[c('x', 'y', 'landcover')])

# run landcover simulation
landcover_sim <- LandCoverSpread(infest_val = 1, suscep_val = 2, spread_rate = 0.05, birdcell = 0, simlength = 15,
                                 simulation_count = 100,  lc_raster = lc_raster,
                                 dep_var_raster_initial = pred_values$`Predicted values raster, current landcover`,
                                 dep_var_raster_pred = pred_values$`Predicted values raster, post-invasion`,
                                 dep_var_modifier = 0.80, silent = TRUE)




# get landcover and dep var rasters
l2 <- as.data.frame(landcover_sim$list_of_landcover_rasters[[2]], xy = TRUE)
l3 <- as.data.frame(landcover_sim$list_of_landcover_rasters[[3]], xy = TRUE)
l4 <- as.data.frame(landcover_sim$list_of_landcover_rasters[[4]], xy = TRUE)
l5 <- as.data.frame(landcover_sim$list_of_landcover_rasters[[5]], xy = TRUE)
d2 <- as.data.frame(landcover_sim$list_of_dep_var_rasters_change_from_year_0[[2]], xy = TRUE)
d3 <- as.data.frame(landcover_sim$list_of_dep_var_rasters_change_from_year_0[[3]], xy = TRUE)
d4 <- as.data.frame(landcover_sim$list_of_dep_var_rasters_change_from_year_0[[4]], xy = TRUE)
d5 <- as.data.frame(landcover_sim$list_of_dep_var_rasters_change_from_year_0[[5]] , xy = TRUE)

# convert dep var values to negative, 0, or positive
num_to_sign <- function(x){ifelse(x < 0, 'negative', ifelse(x == 0, '0', 'positive'))}
d2$layer <- num_to_sign(d2$layer)
d3$layer <- num_to_sign(d3$layer)
d4$layer <- num_to_sign(d4$layer)
d5$layer <- num_to_sign(d5$layer)

# plot
l2 <- ggplot(l2) + geom_raster(aes(x=x, y=y, fill=landcover)) + coord_equal()
l3 <- ggplot(l3) + geom_raster(aes(x=x, y=y, fill=landcover)) + coord_equal()
l4 <- ggplot(l4) + geom_raster(aes(x=x, y=y, fill=landcover)) + coord_equal()
l5 <- ggplot(l5) + geom_raster(aes(x=x, y=y, fill=landcover)) + coord_equal()
d2 <- ggplot(d2) + geom_raster(aes(x=x, y=y, fill=layer)) + coord_equal()
d3 <- ggplot(d3) + geom_raster(aes(x=x, y=y, fill=layer)) + coord_equal()
d4 <- ggplot(d4) + geom_raster(aes(x=x, y=y, fill=layer)) + coord_equal()
d5 <- ggplot(d5) + geom_raster(aes(x=x, y=y, fill=layer)) + coord_equal()

cowplot::plot_grid(l2, d2, l3, d3, l4, d4, l5, d5, ncol = 2, nrow = 4)
