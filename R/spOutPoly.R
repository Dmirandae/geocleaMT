#'@name spOutPoly
#'
#'@title Obtain species outside of a polygon
#'
#'@description This function separates species with restricted 
#'distribution to a polygon or a defined area, following a series
#' of conditions.
#'
#'@param data Vector of characters. Name of the input file.
#'
#'@param rd.frmt Vector of characters. The file format to read. 
#'By default it will be read  as a  R object using 
#' \code{'readRDS'} argument, but it can be read as plain text using 
#' \code{'readTXT'} argument. See details.
#'
#'@param path Vector of characters. Path to the input file.
#'
#'@param shp.poly SpatialPolygonDataframe Class. Polygon of the referenced areas.
#' The area where the species has to be distributed.

#'@param max.per.out Numeric vector. Maximum percentege of occurrences that can 
#'be outside of polygon for the species to be considered as species with 
#'restricted distribution. See details.
#'
#'@param max.occ.out Numeric vector. Absolute maximum number of occurrences that can 
#'be outside of a polygon for the species to be considered as species with 
#'restricted distribution. See details.
#'
#'@param execute Logical vector. If \code{'TRUE'}, the process will be executed on species.
#'If \code{'FALSE'} the species will only be classified and an information table will 
#'be saved.
#'
#'@param B1  Logical vector. If \code{'TRUE'}, the condition B1T will be applied.
#'If \code{'FALSE'} the condition B1F will be applied. See details. 
#'
#'@param B2   Logical vector. If \code{'TRUE'}, the condition B2T will be applied.
#'If \code{'FALSE'} the condition B2F will be applied. See details.
#'
#'@param wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using 
#\code{'saveRDS'} argument, but it can be saved as plain text using 
#\code{'saveTXT'} argument. See details.
#' 
#'@param save.inside.in Vector of characters. Path to the output 
#'file for each  species whose distribution was considered restricted to polygon.
#'
#'
#'@param save.outside.in Vector of characters.  Path to the output 
#'file for each  species whose distribution was considered widespread.
#'
#'@details #'
#' To see the descriptions and details about conditions and parameters the user 
#' should check  R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#'The condition A, B3 and  C for species classification are executed by
#' default, and these can not be modified.
#'
#'The headers of the input file must follow the Darwin Core standard [1]. 
#'The user can see the guide using data('ID_DarwinCore) command.
#'For  more details about the formats to read and/or write, see 
#'\code{\link{readAndWrite}} function.
#'
#'@return  If execute is \code{'TRUE'} a table data.frame class with information about 
#'how was the species classified and an output file by species with occurrences. 
#'If execute is FALSE, a table data.frame class will be saved,  with information about 
#'how was the species classified.
#'
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
#' [1] Wieczorek, J. et al. 2012. Darwin core: An evolving community-developed biodiversity data standard. 
#' PloS One 7: e29715.
#' 


spOutPoly <- function(data            = NULL,
                      rd.frmt         = 'readRDS',
                      path            = NULL,
                      shp.poly        = NULL,
                      max.per.out     = 5,
                      max.occ.out     = 3,
                      execute         = T,
                      B1              = F,
                      B2              = T,
                      wrt.frmt        = 'saveRDS',
                      save.inside.in  = NULL,
                      save.outside.in = NULL) { 
  
  tab.info <- as.data.frame(matrix(NA, length(data), 8))
  colnames(tab.info) <- c('Sp', 'No.occurrences', 'No.inside', 'No.outside',
                          'Percent.out', 'Status.sp', 'Delete','CondicionApplied')
  if (execute == F) {

    for (i in 1:length(data)) {
    # lea archivo
    sp <- as.data.frame(readAndWrite(action = 'read', frmt = rd.frmt,
                                     path = path, name = data[i]))
    tab.info$Sp[i] <- data[i]
    tab.info$No.occurrences[i] <- nrow(sp)
    # Defina  cuales son las coordenadas en la matriz.
    coordinates(sp) <- sp[,c('decimalLongitude','decimalLatitude')]
    # unifique el Datum.
    proj4string(sp) <- proj4string(shp.poly)
    # estraiga los puntos dentro del poligono de trabjo.
    outside <- is.na(over(sp,(as(shp.poly, 'SpatialPolygons'))))
    if (any(outside) == T) {
      tab.info$No.outside[i] <- nrow(sp[outside, ])
      tab.info$Percent.out[i] <- as.character(round(((nrow(sp[outside, ])*100)
                                                         /nrow(sp)), 2))
      if (as.numeric(as.character(tab.info$No.outside[i])) ==
          as.numeric(as.character(tab.info$No.occurrences[i]))) {
        tab.info$No.outside[i] <- nrow(sp[outside, ])
        tab.info$No.inside[i] <- 0
        tab.info$Status.sp[i] <- 'outside'
        tab.info$Delete[i] <- 'sp'
        tab.info$CondicionApplied[i] <- 'C'
      }else{
        if (as.numeric(tab.info$Percent.out[i]) > max.per.out) {
          if (as.numeric(tab.info$No.outside[i]) <= max.occ.out) {
            if (B2 == T) {
              sp <- as.data.frame(sp[!outside, ])
              sp <- sp[, -c(length(sp) - 1, length(sp))]
              tab.info$No.outside[i] <- nrow(sp[outside, ])
              tab.info$No.inside[i] <- nrow(sp[!outside, ])
              tab.info$Status.sp[i] <- 'inside'
              tab.info$Delete[i] <- 'points'
              tab.info$CondicionApplied[i] <- 'B2T'
            } else {
              sp <- as.data.frame(sp)
              sp <- sp[,-c(length(sp) - 1,length(sp))]
              tab.info$No.outside[i] <- nrow(sp[outside, ])
              tab.info$No.inside[i] <- nrow(sp[!outside, ])
              tab.info$Status.sp[i] <- 'inside'
              tab.info$Delete[i] <- 'None'
              tab.info$CondicionApplied[i] <- 'B2F'
            }
          } else {
            sp <- as.data.frame(sp)
            sp <- sp[,-c(length(sp) - 1,length(sp))]
            tab.info$No.outside[i] <- nrow(sp[outside, ])
            tab.info$No.inside[i] <- nrow(sp[!outside, ])
            tab.info$Status.sp[i] <- 'outside'
            tab.info$Delete[i] <- 'sp'
            tab.info$CondicionApplied[i] <- 'B3'
          }
        } else {
          if (B1 == T ) {
            sp <- as.data.frame(sp[!outside, ])
            sp <- sp[, -c(length(sp) - 1, length(sp))]
            tab.info$No.outside[i] <- nrow(sp[outside, ])
            tab.info$No.inside[i] <- nrow(sp[!outside, ])
            tab.info$Status.sp[i] <- 'inside'
            tab.info$Delete[i] <- 'points'
            tab.info$CondicionApplied[i] <- 'B1T'
          } else {
            sp <- as.data.frame(sp)
            sp <- sp[,-c(length(sp) - 1,length(sp))]
            tab.info$No.outside[i] <- nrow(sp[outside, ])
            tab.info$No.inside[i] <- nrow(sp[!outside, ])
            tab.info$Status.sp[i] <- 'inside'
            tab.info$Delete[i] <- 'None'
            tab.info$CondicionApplied[i] <- 'B1F'
          }
        }
      }
    } else {
      sp <- as.data.frame(sp)
      sp <- sp[,-c(length(sp) - 1,length(sp))]
      tab.info$No.outside[i] <- nrow(sp[outside, ])
      tab.info$No.inside[i] <- nrow(sp[!outside, ])
      tab.info$Percent.out[i] <- 0
      tab.info$Status.sp[i] <- 'inside'
      tab.info$Delete[i] <- 'None'
      tab.info$CondicionApplied[i] <- 'A'
    }
    print(paste('Species',i,':',data[i],sep = ''))
    }
    path1 <- getwd()
    readAndWrite(action = 'write', frmt = wrt.frmt,
                 path = path1, name = '/Classify.Sp',
                 object = tab.info)
  }else{
  for (i in 1:length(data)) {
    #! lea archivo
    sp <- as.data.frame(readAndWrite(action = 'read', frmt = rd.frmt,
                                       path = path, name = data[i]))
    tab.info$Sp[i] <- data[i]
    tab.info$No.occurrences[i] <- nrow(sp)
    #! Defina  cuales son las coordenadas en la matriz.
    coordinates(sp) <- sp[,c('decimalLongitude','decimalLatitude')]
    #! unifique el Datum.
    proj4string(sp) <- proj4string(shp.poly)
    #! extraiga los puntos dentro del poligono de trabjo.
    outside <- is.na(over(sp,(as(shp.poly, 'SpatialPolygons'))))
    if (any(outside) == T) {
      tab.info$No.outside[i] <- nrow(sp[outside, ])
      tab.info$Percent.out[i] <- as.character(round(((nrow(sp[outside, ])*100)
                                                     /nrow(sp)), 2))
      if (as.numeric(as.character(tab.info$No.outside[i])) ==
            as.numeric(as.character(tab.info$No.occurrences[i]))) {
        tab.info$No.outside[i] <- nrow(sp[outside, ])
        tab.info$No.inside[i] <- 0
        tab.info$Status.sp[i] <- 'outside'
        tab.info$Delete[i] <- 'sp'
        tab.info$CondicionApplied[i] <- 'C'
        }else{
          if (as.numeric(tab.info$Percent.out[i]) > max.per.out) {
            if (as.numeric(tab.info$No.outside[i]) <= max.occ.out) {
              if (B2 == T) {
                sp <- as.data.frame(sp[!outside, ])
                sp <- sp[, -c(length(sp) - 1, length(sp))]
               readAndWrite(action = 'write', frmt = wrt.frmt,
                            path = save.inside.in, name = data[i],
                            object = sp)
               tab.info$No.outside[i] <- nrow(sp[outside, ])
               tab.info$No.inside[i] <- nrow(sp[!outside, ])
               tab.info$Status.sp[i] <- 'inside'
               tab.info$Delete[i] <- 'points'
               tab.info$CondicionApplied[i] <- 'B2T'
               } else {
                 sp <-as.data.frame(sp)
                 sp <- sp[,-c(length(sp) - 1,length(sp))]
                 readAndWrite(action = 'write', frmt = wrt.frmt,
                              path = save.inside.in, name = data[i],
                              object = sp)
                 tab.info$No.outside[i] <- nrow(sp[outside, ])
                 tab.info$No.inside[i] <- nrow(sp[!outside, ])
                 tab.info$Status.sp[i] <- 'inside'
                 tab.info$Delete[i] <- 'None'
                 tab.info$CondicionApplied[i] <- 'B2F'
               }
              } else {
                sp <- as.data.frame(sp)
                sp <- sp[,-c(length(sp) - 1,length(sp))]
                readAndWrite(action = 'write', frmt = wrt.frmt, 
                             path = save.outside.in, name = data[i],
                             object = sp)
                tab.info$No.outside[i] <- nrow(sp[outside, ])
                tab.info$No.inside[i] <- nrow(sp[!outside, ])
                tab.info$Status.sp[i] <- 'outside'
                tab.info$Delete[i] <- 'sp'
                tab.info$CondicionApplied[i] <- 'B3'
              }
            } else {
              if (B1 == T ) {
                sp <- as.data.frame(sp[!outside, ])
                sp <- sp[, -c(length(sp) - 1, length(sp))]
                readAndWrite(action = 'write', frmt = wrt.frmt,
                             path = save.inside.in, name = data[i],
                             object = sp)
                tab.info$No.outside[i] <- nrow(sp[outside, ])
                tab.info$No.inside[i] <- nrow(sp[!outside, ])
                tab.info$Status.sp[i] <- 'inside'
                tab.info$Delete[i] <- 'points'
                tab.info$CondicionApplied[i] <- 'B1T'
                } else {
                  sp <- as.data.frame(sp)
                  sp <- sp[,-c(length(sp) - 1,length(sp))]
                  readAndWrite(action = 'write', frmt = wrt.frmt,
                               path = save.inside.in, name = data[i],
                               object = sp)
                  tab.info$No.outside[i] <- nrow(sp[outside, ])
                  tab.info$No.inside[i] <- nrow(sp[!outside, ])
                  tab.info$Status.sp[i] <- 'inside'
                  tab.info$Delete[i] <- 'None'
                  tab.info$CondicionApplied[i] <- 'B1F'
                }
            }
        }
      } else {
        sp <- as.data.frame(sp)
        sp <- sp[,-c(length(sp) - 1,length(sp))]
        
        readAndWrite(action = 'write', frmt = wrt.frmt,
                     path = save.inside.in, name = data[i], object = sp)
        tab.info$No.outside[i] <- nrow(sp[outside, ])
        tab.info$No.inside[i] <- nrow(sp[!outside, ])
        tab.info$Percent.out[i] <- 0
        tab.info$Status.sp[i] <- 'inside'
        tab.info$Delete[i] <- 'None'
        tab.info$CondicionApplied[i] <- 'A'
      }
    print(paste('Species',i,':',data[i],sep = ''))
  }
  }
  
  return(tab.info)

}
