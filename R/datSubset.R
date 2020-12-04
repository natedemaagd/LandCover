#' Run spatial GLS models with optimal spatial correlation structure.
#'
#' This function runs a GLS model on a spatial dataset. It chooses among five types of correlation structure, and returns the model with the lowest AIC.
#'
#' @import rgdal sp readxl
#' @importFrom foreach %dopar%
#'
#' @param data `data.frame` that can be converted to a spatial `data.frame`. Coordinates must be in latitude and longitude (decimal degrees)
#' @param x chr string specifying the x-coordinates in `data`
#' @param y chr string specifying the y-coordinates in `data`
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
datSubset <- function(data, x, y, shp_reg, shp_app = NULL, sample = NULL, convertFromUTM = FALSE) {

  # convert from UTM to lat/lon, if specified
  if(convertFromUTM){ shp_reg <- spTransform(shp_reg, CRS("+proj=longlat +datum=WGS84")) }

  # convert data to spatial points
  datSp <- sp::SpatialPointsDataFrame(coords = data[c(x, y)], data = data, proj4string = shp_reg@proj4string)

  # convert CRS of shp_app, if present
  if(!is.null(shp_app)){ shp_app <- sp::spTransform(shp_app, CRSobj = shp_reg@proj4string) }

  # subset the spatial points according to shp_reg and shp_app
                         datSpReg <- datSp[shp_reg,]
  if(!is.null(shp_app)){ datSpApp <- datSp[shp_app,]} else { datSpApp <- datSpReg }

  # if `sample` is specified, sample the regression data
  if(!is.null(sample)) datSpReg <- datSpReg[sample(nrow(datSpReg), sample),]

  # return results
  results <- list(datSpReg, datSpApp)
  results <- lapply(results, as.data.frame)
  names(results) <- c('RegressionData', 'SimulationData')

  return(results)

}

