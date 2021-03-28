#' Run spatial GLS models with optimal spatial correlation structure.
#'
#' This function runs a GLS model on a spatial dataset. It chooses among five types of correlation structure, and returns the model with the lowest AIC.
#'
#' @import rgdal sp readxl
#' @importFrom foreach %dopar%
#'
#' @param data `data.frame` that can be converted to a spatial `data.frame`. Coordinates must be in latitude and longitude (decimal degrees)
#' @param x_coords_varname chr string specifying the x-coordinates in `data`
#' @param y_coords_varname chr string specifying the y-coordinates in `data`
#' @param shp_reg `shapefile` outlining the area of `data` to be used for the regression
#' @param shp_app `shapefile` outlining the area of `data` to which the final simulation will be applied, if different from `shp_reg`
#' @param sample integer. If the regression data are large, you may wish to sample the rows for the `gls_spatial` function.
#' @param convertFromUTM logical. Set to `TRUE` if your shapfile(s) are in UTM coordinates.
#'
#' @return A list of two `data.frames`. The first is a `data.frame` with all points subsetted using the `shp_reg` shapefile. The second is a `data.frame` with all points subsetted using the `shp_app` or `shp_reg` shapefile, as appropriate.
#'
#' @details Subsets a `data.frame` according to provided `shapefiles`. The
#'
#' @examples
#'
#' @export


### FUNCTION:
datSubset <- function(data, x_coords_varname, y_coords_varname, shp_reg = NULL, shp_app = NULL, sample = NULL, convertFromUTM = FALSE) {

  # convert data to spatial points using shp_reg if available, otherwise use shp_app
  if(!is.null(shp_reg)){
    datSp <- sp::SpatialPointsDataFrame(coords = data[c(x_coords_varname, y_coords_varname)], data = data, proj4string = shp_reg@proj4string)
  } else {
    datSp <- sp::SpatialPointsDataFrame(coords = data[c(x_coords_varname, y_coords_varname)], data = data, proj4string = shp_app@proj4string)
  }

  # ensure CRS are identical between shapefiles, then subset the spatial points according to shp_reg and shp_app
  common_crs <- CRS("+proj=longlat +datum=WGS84")
  if(!is.null(shp_app)){shp_app <- sp::spTransform(shp_app, common_crs)}
  if(!is.null(shp_reg)){shp_reg <- sp::spTransform(shp_reg, common_crs)}
  datSp <- sp::spTransform(datSp, common_crs)
                         datSpReg <- datSp[shp_reg,]
  if(!is.null(shp_app)){ datSpApp <- datSp[shp_app,]} else { datSpApp <- datSpReg }

  # if `sample` is specified, sample the data by landcover type
  if(!is.null(sample)){

    # split data by landcover type
    datSpReg_split <- split(datSpReg@data, datSpReg@data$LC_split_var)

    # sample each landcover
    datSpReg_split <- lapply(datSpReg_split, function(df){ df[sample(1:nrow(df), min(sample, nrow(df))),]})

    # recombine into data.frame
    datSpReg <- do.call(rbind, datSpReg_split)

    # convert back to spatial object
    if(!is.null(shp_reg)){
      datSpReg <- sp::SpatialPointsDataFrame(coords = datSpReg[c(x_coords_varname, y_coords_varname)], data = datSpReg, proj4string = shp_reg@proj4string)
    } else {
      datSpReg <- sp::SpatialPointsDataFrame(coords = datSpReg[c(x_coords_varname, y_coords_varname)], data = datSpReg, proj4string = shp_app@proj4string)
    }
  }

  # return results
  results <- list(datSpReg, datSpApp)
  results <- lapply(results, function(df){ as.data.frame(df@data)})
  names(results) <- c('RegressionData', 'SimulationData')

  return(results)

}

