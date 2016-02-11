#'Altitudes
#'
#'
#'Elevation data for the North Andes Block with a resolution of 0.1 decimal degrees.
#'Three columns decimalLongitude, decimalLatitude and elevation. The North
#'Andes Block was divided each 0.1 decimal degree, the elevations associated to
#'each 0.1 decimal degree were extracted using the \code{\link{elevFromGg}}
#'function. This elevation was downloaded from The Google Maps Elevation API [1] and was used for the example in R-Alarcon and Miranda-Esquivel (submitted) [2].
#'
#' @docType data
#'
#' @usage data(Altitudes)
#'
#' @format
#'  A data frame with 28063 observations on the following 3 variables:
#'   decimalLatitude, decimalLongitude and elevation.
#'
#'
#'
#'
#' @keywords datasets
#'
#' @references 
#'[1] R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R
#'package to cleaning geographical data from electronic biodatabases.
#'
#'[2] The Google Maps Elevation API.  Available online at \url{https://developers.google.com/maps/documentation/elevation/intro}
#'
#'
#' @examples
#' \donttest{
#' data(Altitudes)
#' str(Altitudes)
#' data(America)
#' coordinates(Altitudes) <- Altitudes[, c('decimalLongitude', 'decimalLatitude')]
#' plot(America)
#' plot(Altitudes, add=T)}
'Altitudes'
