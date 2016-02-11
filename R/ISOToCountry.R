#'@name ISOTocountry
#'
#'@title Convert from the International Organization for Standardization (ISO 3166-1 alpha-2 or alpha-3) code
#' to country name (English).
#'
#'@description Given the lenght of the ISO country code provided (2=ISO-2 or 3=ISO-3[1]), the function will assign the corresponding country name in English.
#'
#'@param countryCode Data.Frame class. A vector with the ISO-2or ISO-3 Codes.
#'
#'
#'@return A table with two columns (countryCode, country)
#'
#'
#'@details The user can check the ISO 3166-1 alpha-2 or alpha-3 code, valid for each country using
#' the line of code : data(ISO2) or data(ISO3).
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'@note See:
#'R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{countryToCentroid}}
#'
#'@references
#'[1] ISO 2015. International standard for country codes and codes for their subdivisions. Country codes - iso 3166. International Organization for Standardization. Available online at http://www.iso.org/iso/country_codes .


ISOTocountry <- function(countryCode = NULL) {

  iso <- nchar(countryCode)

  if (all(iso != c(2,3))) {
    stop("Wrong ISO length")
    }

    data(wrld_simpl)
    world <-wrld_simpl

    table <- cbind('countryCode' = countryCode, 'country' = NaN)

  for (i in 1:length(countryCode)) {
    table$countryCode[i] <- countryCode[i]
    if(iso==2){position <- which(world@data$ISO2 == countryCode[i])}
    if(iso==3){position <- which(world@data$ISO3 == countryCode[i])}
    if (!is.null(position)) {
      table$country[i] <- world@data$NAME[position]
    }else{
      table$country[i] <- 'None'
    }

  }
  return(table)
}
