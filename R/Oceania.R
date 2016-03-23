#'@name Oceania
#'
#'@title Vectorial layer: Oceanian continent
#'
#'@description Map of the Oceanian continent. Shapefile of countries borders. This map was
#'extracted from data(wrld_simpl) from maptools package [1]. The \code{'Oceania'} object uses the WGS84
#' datum.
#'
#' @docType data
#'
#' @usage data(Oceania)
#'
#' @format Format class \code{'SpatialPolygonsDataFrame'} [package \code{'sp'} [2]].
#'
#' @keywords datasets
#'
#' @references 
#' [1] Bivand  et al. (2015) maptools: Tools for Reading and Handling Spatial Objects. CRAN. Version 0.8-37. Available online at \url{http://r-forge.r-project.org/projects/maptools/}
#' [2] Pebesma et al. (2015) sp: Classes and methods for spatial data.  CRAN. Version 1.2-2. Available online at \url{https://cran.r-project.org/web/packages/sp/}
#'
#' @source \url{http://r-forge.r-project.org/projects/maptools/}
#'
#' @examples
#' \donttest{
#' data(Oceania)
#' plot(Oceania)}
'Oceania'
