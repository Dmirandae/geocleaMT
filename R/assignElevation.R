#'@name assignElevation
#'
#'@title Assigning elevation data
#'
#'@description Given a geographical coordinate (decimalLongitude/ decimalLatitude)
#'the elevation data is assigned from an elevation database (Headers: decimalLongitude/
#'decimalLatitude/ elevation).
#'
#'@param data Vector of characters. Name of the input file.
#'
#'@param path.data Vector of characters. Path to the input file.
#'
#'@param rd.frmt Vector of characters. The file format to read. 
#'By default it will be read as a R object using the
#' \code{'readRDS'} argument, but it can be read as a plain text file, using the
#' \code{'readTXT'} argument. See details.
#'
#'@param elevations.db Data.Frame object. The elevations database, 
#'tree columns (decimalLatitude, decimalLongitude, elevation).
#'
#'@param round.coord Numeric vector. Decimal to round the coordinate to  
#'assign the elevation. This value must be same of the coordinate 
#'resolution of database assigned as argument on \code{elevation.db} parameter. 
#'Example: the resolution of the coordinates in the databases assigned on 
#'\code{elevation.db} is one decimal, then \code{round.coord} must be equal to 1 (round.coord=1).
#'
#'@param  wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using the
#' \code{'saveRDS'} argument, but it can be saved as plain text using the
#' \code{'saveTXT'} argument. See details.
#'
#'@param save.assigned.in Vector of characters. Path to  
#'write the output file with only the coordinates whose elevation was assigned. 
#'Output file will be called <alt.assigned>.
#'
#'@param save.unassigned.in Vector of characters. Path to 
#'write the output file with only the coordinates whose elevation was not assigned. 
#'Output file will be called <alt.unassigned>. See details.
#'
#'
#'@details  For more details about the formats to read and/or write, see the
#'\code{\link{readAndWrite}} function.
#'
#'The coordinates whose elevations were not assigned could be checked with
#'\code{\link{elevFromGg}}. This function can download data directly from 
#'Google Maps Elevation API [1]
#' 
#'The headers of the input file must follow the Darwin Core standard [2]. 
#'The user can see the guide using data('ID_DarwinCore) command.
#'
#'@return A table data.frame class with coordinates and their respective elevation. 
#'A table with coordinates whose elevations were not assigned. A table of information.
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'@note See:
#'R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{elevFromGg}}
#'@seealso \code{\link{readAndWrite}}
#'
#'@references
#'[1] The Google Maps Elevation API.  Available online at \url{https://developers.google.com/maps/documentation/elevation/intro}
#'
#'[2] Wieczorek, J. et al. 2012. Darwin core: An evolving community-developed biodiversity data standard. 
#' PloS One 7: e29715.



assignElevation <- function(data               = NULL,
                       path.data          = NULL,
                       rd.frmt            = 'readRDS',
                       elevations.db      = NULL,
                       round.coord        = 1,
                       wrt.frmt           = 'saveRDS',
                       save.assigned.in   = NULL,
                       save.unassigned.in = NULL){
  #leer tabla
  initials <- readAndWrite(action = 'read', frmt = rd.frmt,
                           path = path.data, name = data)
  if (!any(colnames(initials) == 'elevation')){
    stop('\'data\' table must to have a \'elevation\' column')
  }
  
  # insertar NA por coersion si hay factores
  initials$elevation<-as.numeric(as.character(initials$elevation))
  # Sacar los que no tenga elevaciones asignadas
  whichUnassign<-which(is.na(initials$elevation) | 
                         initials$elevation == 'unassign.Initial')
  #Separe tablas: tabla con elevaciones sin asignar
  unassign.Initial<-initials[whichUnassign,]
  #Separe tablas: tabla con elevaciones assignadas
  assigned.Initial<-initials[-whichUnassign,]
  #saque coordenadas no duplicadas desde la tabla sin elevaciones
  noDuplic<-unassign.Initial[!duplicated(unassign.Initial[,c('decimalLongitude','decimalLatitude')]) ,]
  
  cat(paste('There are',nrow(noDuplic),'coordinates not duplicated to be assigned \n',sep = ' '))
  #Asegure la clase de las variables
  elevations.db$decimalLatitude <- as.numeric(as.character(elevations.db$decimalLatitude))
  elevations.db$decimalLongitude <- as.numeric(as.character(elevations.db$decimalLongitude))
  noDuplic$decimalLatitude <- as.numeric(as.character(noDuplic$decimalLatitude))
  noDuplic$decimalLongitude <- as.numeric(as.character(noDuplic$decimalLongitude))
  unassign.Initial$decimalLatitude <- as.numeric(as.character(unassign.Initial$decimalLatitude))
  unassign.Initial$decimalLongitude <- as.numeric(as.character(unassign.Initial$decimalLongitude))
  
  
  #to.report<-seq(1,nrow(noDuplic),round(nrow(noDuplic)/5,0))
  if (!nrow(noDuplic) > 28000){
    for (i in 1:nrow(noDuplic)){
      #compare las coordenadas con las de la base de datos
      compareCoord <- which(elevations.db$decimalLatitude == 
                              round(noDuplic$decimalLatitude[i],round.coord) &
                              elevations.db$decimalLongitude == 
                              round(noDuplic$decimalLongitude[i],round.coord))
      if (!length(compareCoord)==0){
        #si tiene su par en la base de datos assigne elevacion
        elev<- elevations.db$elevation[compareCoord]
        compareCoord2 <- which(round(unassign.Initial$decimalLatitude,round.coord) ==
                                 round(noDuplic$decimalLatitude[i],round.coord) &
                                 round(unassign.Initial$decimalLongitude,round.coord) ==
                                 round(noDuplic$decimalLongitude[i],round.coord))
        # Asignar la altura
        unassign.Initial$elevation[compareCoord2]<-elev
      }
      
      #     if (i %in% to.report) {
      #       print(paste('Coordinate',i, sep = ' '))
      #     }
    }
      }else{
    for (i in 1:nrow(elevations.db)){
      #compare las coordenadas con las de la base de datos
      compareCoord <- which(round(noDuplic$decimalLatitude,round.coord) == 
                            elevations.db$decimalLatitude [i] &
                              round(noDuplic$decimalLongitude,round.coord) == 
                              elevations.db$decimalLongitude[i])
      if (!length(compareCoord)==0){
        #si tiene su par en la base de datos assigne elevacion
        elev<- elevations.db$elevation[i]
        compareCoord2 <- which(round(unassign.Initial$decimalLatitude,round.coord) ==
                                 round(noDuplic$decimalLatitude[compareCoord],round.coord) &
                                 round(unassign.Initial$decimalLongitude,round.coord) ==
                                 round(noDuplic$decimalLongitude[compareCoord],round.coord))
        # Asignar la altura
        unassign.Initial$elevation[compareCoord2]<-elev
      }
            #     if (i %in% to.report) {
            # print(paste('Coordinate',i, sep = ' '))
      #     }
    }
      }
    #Tabla de informacion
  tab.info<- as.data.frame(matrix(data = NA,nrow = 1,ncol = 3 ))
  colnames(tab.info) <- c('Occ','Elev.Assigned','Elev.Unassigned')
  tab.info$Occ <- nrow(initials)
  # Escriba las elevaciones no asignadas
  final.Unassigned<-subset(unassign.Initial, is.na(unassign.Initial$elevation) | 
                             unassign.Initial$elevation == 'unassign.Initial')
  if (!nrow(final.Unassigned) == 0) {
    readAndWrite(action = 'write', frmt = wrt.frmt,
                 path = save.unassigned.in, name = 'alt.unassigned', 
                 object = final.Unassigned)
    tab.info$Elev.Unassigned<- nrow(final.Unassigned)
    message('There are coordinates without altitudes. See: The \'alt.unassigned\' file')
  }else{
    tab.info$Elev.Unassigned <- 0
  }
  # escirba las elevaciones asignadas
  final.Assigned<-subset(unassign.Initial, !is.na(unassign.Initial$elevation) | 
                           !unassign.Initial$elevation == 'NaN')
  if (!nrow(final.Assigned) == 0) {
    All.assigned <-rbind(final.Assigned,assigned.Initial)
    readAndWrite(action = 'write', frmt = wrt.frmt,
                 path = save.assigned.in, name = 'alt.assigned', 
                 object = All.assigned)
    tab.info$Elev.Assigned <- nrow(All.assigned)
  }else{
    readAndWrite(action = 'write', frmt = wrt.frmt,
                 path = save.assigned.in, name = 'alt.assigned', 
                 object = assigned.Initial)
    tab.info$Elev.Assigned <- nrow(assigned.Initial)
  }
  return(tab.info)
}
