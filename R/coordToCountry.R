#'@name coordToCountry
#'
#'@title From coordinate in decimal degree format to country name (English)
#'
#'@description Give a coordinate in decimal degree format to assign the
#'corresponding country name in English.
#'
#'@param coordinatesTable  Data.Frame class. A data frame with at least two columns
#'called \code{decimalLatitude} and \code{'decimalLongitude'}.
#'
#'@details
#'If the coordinate is outside the continents, this can not be assigned and
#'will it will return a \code{'NaN'}.
#'
#'@return A table with three columns (decimalLatitude, decimalLongitude, country)
#'
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'@note See:
#'R-Alarcon V. and Miranda-Esquivel DR.(submmited) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{coordToISO}}
#'



coordToCountry <- function(coordinatesTable=NULL) {
  data(wrld_simpl)
  world <- wrld_simpl
  coord.table <- as.data.frame(coordinatesTable)
  coord.table <- cbind(as.data.frame(coordinatesTable),'Country.coord' = NaN)
  coordinatesTable(coord.table) <- coord.table[,c('decimalLongitude','decimalLatitude')]
  proj4string(coord.table)  <- proj4string(world)
  for (i in 1:nrow(coord.table)) {
    point.in.polygon <- over(coord.table[i,], as(world, 'SpatialPolygons'))
    if (is.na(point.in.polygon)) {
      coord.table$Country.coord[c] <- NaN
    }else{
      coord.table$Country.coord[i] <- as.character(world$NAME[point.in.polygon])
    }
  }
  
  return(coord.table)

}
