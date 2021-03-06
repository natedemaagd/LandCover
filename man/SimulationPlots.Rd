% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimulationPlots.R
\name{SimulationPlots}
\alias{SimulationPlots}
\title{Plot figures associated with simulation of landcover spread}
\usage{
SimulationPlots(
  landcover_sim_results,
  depvar_sim_results,
  infest_val,
  suscep_val,
  dep_var_modified = TRUE,
  dep_var_label = "dep_var",
  dep_var_modified_label = "dep_var_modified",
  font_size = 15,
  line_thickness = 0.8,
  line_colors = viridis(3)[1:2],
  line_color_labels = c("Dep. var.", "Modified dep. var."),
  line_plot_axis_label = "Dep. var.",
  flip_colors = FALSE,
  decimal_places = 2,
  infest_label = "Invasive",
  suscep_label = "Susceptible",
  positive_vals_only = TRUE,
  n_grid = NA
)
}
\arguments{
\item{landcover_sim_results}{list. The results of the \code{LandCoverSpread} function.}

\item{depvar_sim_results}{list. The results of the \code{ChangeLandcover_to_ChangeDepVar} function.}

\item{infest_val}{numerical. The value of the invasive landcover.}

\item{suscep_val}{numerical. Vector of landcover values that are susceptible to the spread.}

\item{dep_var_modified}{logical. If \code{TRUE}, creates line graphs with added modified dependent variable from the simulation. Requires \code{dep_var_modifier} to be specified in \code{LandCoverSpread}.}

\item{font_size}{value. Font size of text in the figure.}

\item{line_thickness}{value. For the line graph, the thickness of the lines.}

\item{line_color_labels}{character vector. If \code{dep_var_modified == TRUE}, specifies the labels for the dependent variable and the modified dependent variable.}

\item{flip_colors}{logical. Flips the colors of the raster plots}

\item{decimal_places}{numerical. Number of decimal places to report on the legend of continuous plots.}

\item{positive_vals_only}{logical. When plotting the line graph, should the negative values be removed when aggregating by year?}

\item{n_grid}{numerical. If you want a grid of timelapse figures, specify the number of plots.}
}
\value{
A list of landcover plots and line plots associated with the landcover spread simulation.
}
\description{
This plots the results of the landcover simulation data created by \code{LandCoverSpread}.
}
\details{
Included in the resulting list is a list of yearly landcover plots, a list of yearly dependent variable plots, and a line graph of yearly changes both the dependent variable and, if specified, the modified dependent variable.
}
\examples{
# load packages
library(LandCover); library(foreach)

# initialize data.frame with coordinates
dat <- expand.grid(x = 1:20, y = 1:20, KEEP.OUT.ATTRS = FALSE)

# create some data: elevation, landcover, and temp/ET dependent on elevation and landcover
dat$elevation <- with(dat, 50 + 2*x + 5*y + rnorm(nrow(dat), sd = 7))
dat$landcover <- ifelse(dat$elevation < median(dat$elevation), 1, 2)
dat$temp      <- with(dat, (120-0.7*(0.5*elevation + 0.3*y - 0.5*x + ifelse(landcover == 'lc1', -30, 0) + rnorm(nrow(dat)))))
dat$ET        <- with(dat, (   -0.4*(-2*temp       + 0.5*y - 1.0*x + ifelse(landcover == 'lc1', +20, 0) + rnorm(nrow(dat)))))

# run the gls model
regression_results <- gls_spatial(data = dat, landcover_varname = 'landcover', landcover_vec = c(1,2),
                                  reg_formula = ET ~ elevation + temp, error_formula = ~ x + y)

# predict values of ET before and after invasion
pred_values <- gls_spatial_predict(data = dat, regression_results = regression_results, landcover_varname = 'landcover', landcover_invasive = 1, landcover_susceptible = 2,
                                   dep_varname = 'ET', x_coords_varname = 'x', y_coords_varname = 'y')

# get landcover raster
lc_raster <- rasterFromXYZ(dat[c('x', 'y', 'landcover')])

# run landcover simulation
landcover_sim <- LandCoverSpread(infest_val = 1, suscep_val = 2, spread_rate = 0.05, birdcell = 0, simlength = 15, simulation_count = 100,
                                 lc_raster = lc_raster, dep_var_raster_initial = pred_values$`Predicted values raster, current landcover`,
                                 dep_var_raster_pred = pred_values$`Predicted values raster, post-invasion`,
                                 dep_var_modifier = 0.80, silent = TRUE)

}
