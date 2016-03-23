#'@name countryToISO 
#'
#'@title From country name to International Organization for Standardization (ISO 3166-1 alpha-2 / alpha-3)
#'
#'@description  From country name in English to International Organization for Standardization
#'(ISO 3166-1 alpha-2 / alpha-3) [1].
#'
#'@param country Data.Frame class. a vector with countries names in English. See
#'details.
#'
#'
#'@param iso numeric (2 or 3). To define the code whether ISO-2 (iso=2, default value) or ISO-3 (iso=3).
#'
#'
#'@return A data frame object with the ISO 3166-1 alpha-2/alpha-3 assigned to each country.
#'
#'
#'@details
#'To see the valid names in English the user can use the command
#'data(countryNames).
#'
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'
#'@note See:
#'R-Alarcon V, Miranda-Esquivel DR.(submitted) geocleaMT: An R package to cleaning
#'geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{countryToCentroid}}

#
#'@references
#'[1] ISO 2015. International standard for country codes and codes for their subdivisions. Country codes - iso 3166.  International Organization for Standardization. Available online at \url{http://www.iso.org/iso/country_codes}.


countryToISO <- function(country=NULL, iso = 2) {
  data(wrld_simpl)
  if(iso==2){table <- cbind(country, 'CountryCode2'= NaN)}
  if(iso==3){table <- cbind(country, 'CountryCode3'= NaN)}
  
  for (i in 1:length(country)) {
    table$Country[i] <- country[i]
    position <- which(wrld_simpl$NAME == country[i])
    if (!is.null(position)) {
      if(iso==3){table$CountryCode3[i] <- wrld_simpl$ISO3[position]}
      if(iso==2){table$CountryCode2[i] <- wrld_simpl$ISO2[position]}
      
    }else{
      if(iso==3){table$CountryCode3[i] <- 'None'}
      if(iso==2){table$CountryCode2[i] <- 'None'}
      
    }
  }
  return(table)
}
