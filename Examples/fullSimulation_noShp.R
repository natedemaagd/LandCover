
library(LandCover); library(raster); library(ggplot2)

fullSim <- fullSimulation(data_as_directories = TRUE,
                          data = "D:/OneDrive - hawaii.edu/Documents/Projects/Packages/LandCover/Examples/Data/dat1.xlsx",
                          landcover_varname     = 'landcover',
                          landcover_invasive    = 1,
                          landcover_susceptible = 2,
                          reg_formula           = ET ~ elevation + temp,
                          x_coords_varname      = 'x',
                          y_coords_varname      = 'y',
                          spread_rate           = 0.02,
                          birdcell              = 0,
                          simlength             = 15,
                          simulation_count      = 100,
                          dep_var_modifier      = 0.80,
                          dat_sample = 5000,
                          covar_adjustment = list('temp', 0))   # changes all `temp` values to 0 for pixels that are invaded


plot(fullSim$SimulationPlots$lc_timelapse_grid)
plot(fullSim$PriorityPlots$`Priority map, cumulative dep var`)



#### compare landcover and dep var from year to year ----

# get landcover and dep var rasters
l2 <- as.data.frame(fullSim$LandCoverSpread$list_of_landcover_rasters[[2]], xy = TRUE)
l3 <- as.data.frame(fullSim$LandCoverSpread$list_of_landcover_rasters[[3]], xy = TRUE)
l4 <- as.data.frame(fullSim$LandCoverSpread$list_of_landcover_rasters[[4]], xy = TRUE)
l5 <- as.data.frame(fullSim$LandCoverSpread$list_of_landcover_rasters[[5]], xy = TRUE)
d2 <- as.data.frame(fullSim$LandCoverSpread$list_of_dep_var_rasters_change_from_year_0[[2]], xy = TRUE)
d3 <- as.data.frame(fullSim$LandCoverSpread$list_of_dep_var_rasters_change_from_year_0[[3]], xy = TRUE)
d4 <- as.data.frame(fullSim$LandCoverSpread$list_of_dep_var_rasters_change_from_year_0[[4]], xy = TRUE)
d5 <- as.data.frame(fullSim$LandCoverSpread$list_of_dep_var_rasters_change_from_year_0[[5]] , xy = TRUE)

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
