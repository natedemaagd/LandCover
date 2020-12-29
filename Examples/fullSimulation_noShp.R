
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


plot(fullSim$preview_plot)
plot(fullSim$SimulationPlots$lc_timelapse_grid)
plot(fullSim$PriorityPlots$`Priority map, cumulative dep var`)
