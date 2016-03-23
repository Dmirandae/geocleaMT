#'@title Useful species
#'
#'@description Get  species with a minimum number of occurrences.
#'
#'
#'@param data Vector of characters. Name of the input file.
#' 
#'@param rd.frmt  Vector of characters. The file format to read. 
#'By default it will be read  as a  R object using the
#' \code{'readRDS'} argument, but it can be read as plain text using the
#' \code{'readTXT'} argument. See details.
#'
#'@param path Vector of characters. Path to the input file.
#'
#'@param cut.off Numeric. Minimal (absolute) number of occurrences for a given 
#'species, the default value is 3, as the minimal number of points to draw a polygon.
#'
#'@param wrt.frmt  Character. Output file format,  by default it uses a \code{'.RDS'}
#'format, although it could save a txt table. See details.
#'
#'@param save.useful.in String. Output file path for useful species (those with 
#'occurrences larger than the \code{cut.off} value. See details.
#'
#'@param save.useless.in String. Output file path for useless species (those with 
#'occurrences smaller than the \code{cut.off} value. See details.
#'
#'@details
#'To check input/output formats see \code{\link{readAndWrite}}.
#'Column names must follow Darwin Core2[1] standard. See data('ID_DarwinCore).
#'
#'@return A data.frame object and a file for each species.
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'@note See:
#'R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{readAndWrite}}
#'
#'@references
#'[1] Wieczorek, J. et al. 2012. Darwin core: An evolving community-developed biodiversity data standard. 
#' PloS One 7: e29715.



usefulSp <- function(data            = NULL, 
                     path            = NULL, 
                     cut.off         = 3, 
                     rd.frmt         = 'readRDS',
                     wrt.frmt        = 'saveRDS',
                     save.useful.in  = NULL,
                     save.useless.in = NULL){
  tab.info <- as.data.frame(matrix(NA, length(data), 3))
  colnames(tab.info) <- c('Sp', 'Total.Occ', 'State')
  for (i in 1:length(data)){
    sp <- readAndWrite(action = 'read',frmt = rd.frmt ,
                         path = path,name = data[i])
    tab.info$Sp[i] <- data[i]
    tab.info$Total.Occ[i] <- nrow(sp)
      if (nrow(sp) >= cut.off) {
        readAndWrite(action = 'write', frmt = wrt.frmt ,
                       path = save.useful.in, name = data[i], object = sp)
        tab.info$State[i] <- 'in'
      } else {
        readAndWrite(action = 'write', frmt = wrt.frmt ,
                       path = save.useless.in, name = data[i], object = sp) 
        tab.info$State[i] <- 'out'
      }
    print(paste('Species:',data[i]))
  }
  return(tab.info)
}
