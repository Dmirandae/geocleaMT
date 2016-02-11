#'@name splitGeoref
#'
#'@title  Separate geo-referenced records
#'
#'@description Get only georeferenced records from a database
#' downloaded from GBIF[1].
#'
#'@param data Vector of characters. Name of the input file.
#'
#'@param rd.frmt Vector of characters. The file format to read. 
#'By default it will be read  as a  R object using 
#' \code{'readRDS'} argument, but it can be read as plain text using 
#' \code{'readTXT'} argument. See details.
#'
#'@param path Vector of characters. Path to the data file(s).
#'
#'@param min.occ Numeric vector. Minimal number of georeferenced occurrences 
#'for that a species can be considered. If the argument is 3, only species 
#'with 3 or more occurrences will be considered in the process.
#'
#'
#'@param round.coord Integer. Number of digits to keep in coordinates.
#'  
#'@param wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using 
#' \code{'saveRDS'} argument, but it can be saved as plain text using 
#' \code{'saveTXT'} argument. See details.
#'
#'@param save.min.occ.in Character. Path to save the output file of species 
#'that do not fulfill the minimum required number of occurrences in the parameter 
#'\code{min.occ}. Default format is  \code{'.RDS'}. Se details.
#'
#'@param save.ungeoref.in Character. Path to save ungeoreferenced data,
#'a file for each species. Default format is  \code{'.RDS'}. Se details.
#'
#'
#'@param save.georef.in Vector of characters. Path to write the output 
#'file for each species with georeferenced records.
#'
#'@details For more details about the formats to read and/or write, see 
#'\code{\link{readAndWrite}} function.
#' 
#'The headers of the input file must follow the Darwin Core standard [2]. 
#'The user can see the guide using data('ID_DarwinCore) command.
#'
#'@return   By each species, if this is the case, two output files 
#'data.frame class: The first one  have the georeferenced and
#' the second one have the ungeoreferenced
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
#'[1] Global Biodiversity Information Facility. Available  online at \url{ http://www.gbif.org/}.
#'
#'[2] Wieczorek, J. et al. 2012. Darwin core: An evolving community-developed biodiversity data standard. 
#' PloS One 7: e29715. 
#'

splitGeoref <- function(data             = NULL,
                        rd.frmt          ='readRDS',
                        path             = NULL,
                        min.occ          = 3,
                        round.coord      = 4,
                        wrt.frmt       = 'saveRDS',
                        save.min.occ.in  = NULL,
                        save.georef.in   = NULL, 
                        save.ungeoref.in = NULL) {
  #! tabla de informacion 
  tab.info <- as.data.frame(matrix(NA, length(data), 4))
  colnames(tab.info) <- c('Species', 'georef', 'ungeoref', 'Min.occ')
  #! para cada uno de las species en data
  for (i in 1:length(data)) {
    #! Asigne el nombre de la especie
    tab.info$Species[i] <- data[i]
    #! leer archivo
    sp <- readAndWrite(action = 'read', frmt = rd.frmt,
                         path = path, name = data[i])
    importHeaders <- c('species','decimalLatitude','decimalLongitude',
                     'countryCode','continent','country','county',
                     'locality','stateProvince')
    failHeader <- importHeaders[which(!importHeaders %in% names(sp))]
    
    if (!length(failHeader) == 0) {
      for (t in 1:length(failHeader)) {
    
        sp <- cbind(sp,failHeader[t])
        x <- which(colnames(sp) == 'failHeader[t]')
        colnames(sp)[x] <- failHeader[t]
      }
    }
    #! si el numero de ocurrencias es mayor al minimo necesario para que una
    #! especie sea considerada util: continue, sino escribala como archivo que no
    #! sirve
    if (nrow(sp) >= min.occ ) {
      tab.info$Min.occ[i] <- 0
      #! Eminine duplicados 
      sp.temp1 <- subset(sp,!duplicated(sp[,c('species','decimalLatitude',
                                             'decimalLongitude','countryCode',
                                             'continent','country','county',
                                             'locality','stateProvince')]))
      #! separe las ocurrencias georeferenciadas.
      #! separar lo que no este asignado o este asignado a 0 para latitud y longitud
      georef <- subset(sp.temp1, !is.na(as.numeric(as.character(sp.temp1$decimalLatitude))
                        & as.numeric(as.character(sp.temp1$decimalLongitude)))
                        & !(sp.temp1$decimalLatitude == 0 & sp.temp1$decimalLongitude == 0))
      #! Redondear coordenadas
      georef$decimalLatitude <- round(as.numeric(as.character(georef$decimalLatitude)), 
                                digits = round.coord)
      georef$decimalLongitude <- round(as.numeric(as.character(georef$decimalLongitude)),
                                 digits = round.coord)
      #! Elimine duplicados despues del redondeo
      georef <- as.data.frame(subset(georef,!duplicated(georef[,c('decimalLatitude',
                                                          'decimalLongitude')])))
      #! si hay ocurrencias georeferenciadas, guardar como ocurrencias georeferenciadas
      if (!nrow(georef) == 0 ) {
        readAndWrite(action = 'write', frmt = wrt.frmt, 
                       path = save.georef.in , name = data[i], object = georef)
        } 
      #! Asigne informacion de georeferenciados en la tabla de informacion
      tab.info$georef[i] <- length(georef$species)
      #######! Separe los no georeferenciados (No asignados o asignados como '0')
      ungeoref <- subset(sp.temp1,is.na(as.numeric(as.character(sp.temp1$decimalLatitude))
                                           & as.numeric(as.character(sp.temp1$decimalLongitude)))
                            | sp.temp1$decimalLatitude == 0 & sp.temp1$decimalLongitude == 0)
      #! Si hay registros no georeferenciados, guarde comono georeferenciados
      if (!nrow(ungeoref) == 0) {
        readAndWrite(action = 'write', frmt = wrt.frmt, 
                       path = save.ungeoref.in, name = data[i], 
                       object = ungeoref)
        tab.info$ungeoref[i] <- length(ungeoref$species)
      }
      #! si desde el inicio no cumple con el un minimo numero de ocurrencias, guarde como Min.occ
      } else {
        readAndWrite(action = 'write', frmt = wrt.frmt,
                       path = save.min.occ.in, name = data[i], object = sp)
        tab.info$Min.occ[i] <- nrow(sp)
      }
    print(paste('Sp',i,':',data[i],sep = ' '))
  }
  
  return(tab.info)
  
}
