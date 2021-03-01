library(nlme); library(doParallel); library(foreach); library(parallel); library(viridis); library(rgdal); library(sp); library(readxl); library(LandCover); library(ggplot2); library(raster); library(rasterVis)


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



# Hanaula ranch
data = allvars
shp_app = shp_app
shp_reg = shp_moku_overlap
landcover_varname = "LC"
reg_formula = AET~LAI+U+T+Rnet+SM
landcover_invasive = 32
landcover_susceptible = c(8,10,13)
dat_sample = 5000
x_coords_varname = 'POINT_X'
y_coords_varname = 'POINT_Y'
spread_rate = 0.05
birdcell = 0
simlength = 50
simulation_count = 100
dep_var_modifier = 0.55
num_cores = 5
covar_adjustment = list('LAI',median_LAI)
unit_converter = 1
outlier_value = -100
zero_break = TRUE



# Kauai simulation_by_moku
k=1
data = dat_split[[k]]; landcover_varname = 'LC'; reg_formula = AET ~ LAI + U + T + Rnet + SM; landcover_invasive = 32; landcover_susceptible = c(8,10,13);
x_coords_varname = 'POINT_X'; y_coords_varname = 'POINT_Y';
spread_rate = 0.05; birdcell = 0; simlength = 50;
dep_var_modifier=1; unit_converter=1;
covar_adjustment = list('LAI', median(dat_split[[k]][dat_split[[k]]$LC==32,'LAI'], na.rm = TRUE)); dat_sample = 300



# kauai kwa
data = allvars; shp_app = shp_reg_region; shp_reg = shp_reg_region; landcover_varname = "LC"; reg_formula = AET~LAI+U+T+Rnet+SM; landcover_invasive = 32; landcover_susceptible = c(8,9,10,13);
dat_sample = 800; x_coords_varname = 'POINT_X'; y_coords_varname = 'POINT_Y'; spread_rate = 0.05; birdcell = 0; simlength = 50; simulation_count = 100; dep_var_modifier = 0.36;
num_cores = 5; covar_adjustment = list('LAI',median_LAI); unit_converter = 1; zero_break = FALSE; outlier_value = NA; dep_var_plot_label = 'mm/yr'; dep_var_plot_label_cumulative = 'mm'
