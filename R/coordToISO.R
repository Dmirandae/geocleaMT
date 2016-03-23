#'@name coordToISO
#'
#'@title From coordinate in decimal degree format to International Organization for Standardization (ISO 3166-1 alpha-2/alpha-3)
#'
#'@description Give a coordinate in decimal degree format to assign the
#'corresponding International Organization for Standardization (ISO 3166-1 alpha-2/alpha-3) [1].
#'
#'@param coordinates.tab  Data Frame class. A data frame with at least two columns
#'called \code{decimalLatitude} and \code{'decimalLongitude'}.
#'
#'
#'@param iso numeric (2 or 3). To define the code whether ISO-2 (iso=2, default value) or ISO-3 (iso=3).
#'
#'@details
#'If the coordinate is outside the continent, this can not be assigned and
#'will be assigned as NaN. If you want to see the oficial ISO codes, you could use the data(ISO2) or data(ISO3) comands.
#'
#'@return A table with three columns (decimalLatitude, decimalLongitude, countryCode)
#'
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'@note See:
#'R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{coordToCountry}}
#'
#'@references
#'[1]ISO 2015. International standard for country codes and codes for their subdivisions. Country codes - iso 3166.  International Organization for Standardization. Available online at \url{http://www.iso.org/iso/country_codes}


coordToISO<-function(coordinates.tab=NULL, iso = 2){
 
  data(wrld_simpl)
  world <- wrld_simpl
  coord.table <- as.data.frame(coordinates)
  if(iso==3){
	coord.table <- cbind(as.data.frame(coordinates), 'CountryCode'= NaN)
	}
  if(iso==2){
	coord.table <- cbind(as.data.frame(coordinates), 'CountryCode'= NaN)
	}
  coordinates(coord.table) <- coord.table[, c('decimalLongitude', 'decimalLatitude')]
  proj4string(coord.table) <- proj4string(world)
  for (i in 1:nrow(coord.table)){
    point.in.polygon <- over(coord.table[i, ], as(world, 'SpatialPolygons'))
    if(is.na(point.in.polygon)){
      if(iso==3){ 
		coord.table$CountryCode3[c] <- NaN 
		}
      if(iso==2){
		coord.table$CountryCode2[c] <- NaN
		}
      message(paste(i,'There are coordinates withou ISO data, please check the data point as it could be at the sea/ocean', sep=' '))
    }else{
      if(iso==3){coord.table$CountryCode3[i]<-as.character(world$ISO3[point.in.polygon])}
      if(iso==2){coord.table$CountryCode3[i]<-as.character(world$ISO2[point.in.polygon])}
      
    }
    
  }
  
  return(coord.table)
  
}
