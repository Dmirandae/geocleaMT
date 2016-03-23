#'@name elevFromGg
#'
#'@title  Assign elevation to a coordinates list
#'
#'@description Given a coordinates vector, download the elevation data 
#' from The Google Maps Elevation API [1].
#'
#'@param data Vector of characters. Name of the input file.
#'
#'@param rd.frmt Vector of characters. The file format to read. 
#'By default it will be read  as a  R object using the
#' \code{'readRDS'} argument, but it can be read as plain text using the
#' \code{'readTXT'} argument. See details.
#'
#'@param path Vector of characters. Path to the input file.
#'
#'@param API.Key If the user has a google's API key, it must be used here,
#' otherwise no more than 2500 elevations could be assigned.
#'
#'See details.
#'
#'@param starts.in Numeric vector. Row number where should start the process.
#'
#'@param round.coord Integer. Number of digits to keep in coordinates.
#'  
#'@param wrt.frmt Vector of characters. Format to save the output
#'file. By default it will be written  as a  R object using 
#' \code{'saveRDS'} argument, but it can be saved as plain text using 
#' \code{'saveTXT'} argument. See details.
#'
#'@param save.name Vector of characters. Name of the output file. 
#'
#'@param save.assigned.in Vector of characters. Path to 
#'the output file with the coordinates whose elevation was assigned. 
#'Output file will be called <alt.assigned>.
#'
#'@param save.unassigned.in Vector of characters. Path to 
#' the output file with the coordinates whose elevation was not assigned. 
#'Output file will be called <alt.unassigned>. See details.
#'
#'@param save.temp.in Vector of characters. Path to the temporal file 
#'as backup of the process. If the process is stopped, the user could reset it 
#'assigning the row number in \code{start.in} parameter and the process will start again
#'using temporal file.
#'
#'@details 
#'For more details about the formats to read and/or write, see 
#'\code{\link{readAndWrite}} function.
#'
#'The coordinates whose elevations were not assigned could be checked with
#'\code{\link{assignElevation}}. This function can download directly from
#' Google Maps Elevation API[1]
#'
#' Additional information at: 
#'\url{https://developers.google.com/maps/documentation/elevation/#api_key}
#'  
#'
#'The headers of the input file must follow the Darwin Core standard [2]. 
#'The user can see the guide using data('ID_DarwinCore) command.
#'
#'@return A table data.frame class with coordinates and their respective elevation. 
#'A table with coordinates whose elevations were not assigned. A table as backup of the process.
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'@note See:
#'R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to cleaning geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{assignElevation}}
#'
#'@references
#'[1] The Google Maps Elevation API.  Available online at \url{https://developers.google.com/maps/documentation/elevation/intro}
#'
#'[2] Wieczorek, J. et al. 2012. Darwin core: An evolving community-developed biodiversity data standard. 
#' PloS One 7: e29715.
#'

elevFromGg <- function(data     = NULL,
                                rd.frmt           = 'readRDS',
                                path      = NULL,
                                API.Key              = NULL, 
                                starts.in             = 1,
                                round.coord =           4,
                                save.name            = 'data',
                                wrt.frmt        = 'writeRDS',
                                save.assigned.in = NULL,
                                save.unassigned.in = NULL,
                                save.temp.in = NULL) {
  
  if (!starts.in==1){
    data  <- readAndWrite(action = 'read', frmt = 'readRDS',
                                 path =save.temp.in, name = 'Alt.assigned.temp')
  } else {
    #! lear archivo inicial.
    #! definir columnas
    data <- readAndWrite(action = 'read', frmt = rd.frmt ,
                         path = path, name = data)
     if( any(colnames(data)=='elevation')){
     data <- cbind(data, 'resolution' = NA)
    
     }else{data <- cbind(data, 'elevation' = NA, 'resolution' = NA)
    }
  }
  Tab.info<-as.data.frame(matrix(NA, 1, 4))
  colnames(Tab.info) <- c('Total.Occ','Unique.Occ','Occ.Assigned','Occ.Unassigned')
  Tab.info$Total.Occ <- nrow(data)
  #! sauqe solo los unicos y prgunte los unicos
  
  data.uni <-data[!duplicated(data[,c('decimalLatitude','decimalLongitude')]),]
  Tab.info$Unique.Occ <- nrow(data.uni)
  #! haga una secuencia que permita que el sistema duerma por un segundo para 
  #! no congestionar el servidor
  seq.tem <- seq(1, nrow(data.uni), 10)
  for (i in starts.in:nrow(data.uni)) {
    
      if (i %in% seq.tem){
  
        #! cada 10 puntos espere un segundo para volver a preguntar
        Sys.sleep(time = 1)
        readAndWrite(action = 'write',frmt = 'saveRDS',
                     path = save.temp.in,name = 'Alt.assigned.temp',object = data)
      }
      #! pregunte al API 
      if(!is.null(API.Key)) {
        API.google<-paste('http://maps.googleapis.com/maps/api/elevation/json?locations=',
                 round(as.numeric(as.character(data.uni$decimalLatitude[i])), round.coord), ',', 
                 round(as.numeric(as.character(data.uni$decimalLongitude[i])), round.coord),
                 '&sensor=false&key=', API.Key,sep='')
        } else {
          API.google<-paste('http://maps.googleapis.com/maps/api/elevation/json?locations=',
                   round(as.numeric(as.character(data$decimalLatitude[i])), round.coord), ',', 
                   round(as.numeric(as.character(data$decimalLongitude[i])), round.coord),
                   '&sensor=false',sep='')
        }
      Call.API <- fromJSON(API.google)
      if (is.null(Call.API)){
        message(cat(' The server does not respond, please check your coordinate and/or try later \n'))
      } else {
		  #! Asigne altura a cada coordenada
        elevation <- Call.API$results[[1]]$elevation
        resolution <- Call.API$results[[1]]$resolution
        #! cuales de la lsita complet son iguales a la coordena descargada
        x <- which(as.numeric(as.character(data$decimalLatitude)) ==
                     as.numeric(as.character(data.uni$decimalLatitude[i]))
                 & as.numeric(as.character(data$decimalLongitude)) == 
                   as.numeric(as.character(data.uni$decimalLongitude[i])))
        #! asigne la altura a la lista
        data$elevation[x] <- round(as.numeric(as.character(elevation)), 3)
        data$resolution[x] <- round(as.numeric(as.character(resolution)), 3)
        print(paste('Coordinate',i, 'was assigned',sep = ' '))
      }
    
    
  }
    #! revise si hay coordenadas sin asignar, separelas y guardelas en 
    #! otro archivo
    na.altitude <- which(is.na(data$elevation))
    if ( !length( na.altitude ) == 0) {
      unassigned<-data[na.altitude, ]
      readAndWrite(action = 'write', frmt = wrt.frmt, 
                     path   = save.unassigned.in,
                     name   = paste(save.name, 'unassigned', sep = '-'),
                     object = unassigned)
      message(cat(paste('You have coordinates that are not assigned. 
                      Please, check these coordinates in the ', 
                      paste('\'', save.name, 'unassigned', '\'', sep = '')
                      , 'file','\n',sep = ' ')))
    }
    #! guarde las alturas asignadas
    if (length(na.altitude)==0){
    Tab.info$Occ.Unassigned <- 0
      assigned <- data
    }else{
      assigned <- data[-na.altitude, ]
       Tab.info$Occ.Unassigned <- length(na.altitudes)
    }
    
    readAndWrite(action = 'write', frmt = wrt.frmt, 
                   path   = save.assigned.in, 
                   name   = save.name,
                   object = assigned)
    
    Tab.info$Occ.Assigned <- nrow(assigned)
    return(Tab.info)
  
}
