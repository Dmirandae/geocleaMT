#'@name countryToCentroid
#'
#'@title From country name to centroid
#'
#'@description  From country name in English to the centroid (Coordinate) of the polygon that
#'represents this country.
#'
#'@param country data.frame class. a vector with country names in English. See
#'details.
#'
#'
#'@return A data frame object with the centroid assigned to each country.
#'
#'
#'@details
#'To see the  valid names in English the user can run the command
#'data(countryNames).
#'
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'
#'@note See:
#'R-Alarcon V, Miranda-Esquivel DR. (submitted) geocleaMT: An R package to cleaning
#'geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{countryToISO}}
#
#'





countryToCentroid <- function(country=NULL){
  
  data(wrld_simpl)
  table <- cbind(country, 'centroidLon' = NaN, 'centroidLat'= NaN)
  for (i in 1:nrow(table)) {
    table$country <- table[i]
    position <- which(wrld_simpl$NAME == paises[i])
    if (!is.null(position)) {
      table$centroidLon <- wrld_simpl$LON[position]
      table$centroidLat <- wrld_simpl$LAT[position]
      }else{
        table$centroidLon <- 'None'
        table$centroidLat <- 'None'
      }
  }
  
  return(table)

}
