library(nlme); library(doParallel); library(foreach); library(parallel); library(viridis); library(rgdal); library(sp); library(readxl); library(LandCover); library(ggplot2); library(raster)


# fullSimulation_noShp
data_as_directories = TRUE
data = "D:/OneDrive - hawaii.edu/Documents/Projects/Packages/LandCover/Examples/Data/dat1.xlsx"
shp_reg_directory = NULL
shp_reg_layer = NULL
shp_app_directory = NULL
shp_app_layer = NULL
convertFromUTM = FALSE
dat_sample = NULL
landcover_varname = 'landcover'
reg_formula = ET ~ elevation + temp
landcover_invasive = 1
landcover_susceptible = 2
x_coords_varname = 'x'
y_coords_varname = 'y'
spread_rate = 0.05
birdcell = 0
simlength = 15
simulation_count = 100
dep_var_modifier = 0.8
covar_adjustment = list('temp', 0)
num_cores = 5



# fullSimulation_Shp
data_as_directories = TRUE
data = "D:/OneDrive - hawaii.edu/Documents/Projects/Packages/LandCover/Examples/Data/dat2.xlsx"
shp_reg_directory = 'D:/OneDrive - hawaii.edu/Documents/Projects/Packages/LandCover/Examples/Data/shp1-LargeRegion'
shp_reg_layer = 'shp1-LargeRegion'
shp_app_directory = 'D:/OneDrive - hawaii.edu/Documents/Projects/Packages/LandCover/Examples/Data/shp2-SmallRegion'
shp_app_layer = 'shp2-SmallRegion'
convertFromUTM = FALSE
dat_sample = NULL
landcover_varname     = 'landcover'
landcover_invasive    = 1
landcover_susceptible = 2
reg_formula           = ET ~ elevation + temp
x_coords_varname      = 'x'
y_coords_varname      = 'y'
spread_rate           = 0.05
birdcell              = 0
simlength             = 15
simulation_count      = 100
dep_var_modifier      = 0.80
covar_adjustment = list('temp', 0)
unit_converter = 100
num_cores = 5



# fullSimulationApp
data = readxl::read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/Packages/LandCover/Examples/Data/dat2.xlsx")
shp_reg = rgdal::readOGR('D:/OneDrive - hawaii.edu/Documents/Projects/Packages/LandCover/Examples/Data/shp1-LargeRegion', 'shp1-LargeRegion')
shp_app = rgdal::readOGR('D:/OneDrive - hawaii.edu/Documents/Projects/Packages/LandCover/Examples/Data/shp2-SmallRegion', 'shp2-SmallRegion')
convertFromUTM = FALSE
landcover_varname     = 'landcover'
landcover_invasive    = 1
landcover_susceptible = 2
reg_formula           = ET ~ elevation + temp
x_coords_varname      = 'x'
y_coords_varname      = 'y'
spread_rate           = 0.02
birdcell              = 0
simlength             = 15
simulation_count      = 100
dep_var_modifier      = 0.80
dat_sample = 5000
covar_adjustment = list('temp', 0)
unit_converter = 100
num_cores = 5
