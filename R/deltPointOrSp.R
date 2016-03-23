#'@name delPointsOrSp
#'
#'@title  Delete some/all records of a given species
#'
#'@description The area of interest is framed as the four coordinates (North, 
#'South, West and East), if the species has some points outside of this area the user
#'will be asked to decide whether delete the point or delete the  species.
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
#'@param west Numeric vector. Western coordinate in decimal longitude format.
#'
#'@param east Numeric vector. Eastern coordinate in decimal longitude format.
#'
#'
#'@param south Numeric vector. Southern coordinate in decimal latitude format.
#'
#'
#'@param north Numeric vector. Northern coordinate in decimal latitude format.
#'
#'
#'@param plot.distrib Logical vector. If \code{'TRUE'}, the distribution of the species will 
#'be plotted in red on a world map.
#'
#'@param wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using 
#' \code{'saveRDS'} argument, but it can be saved as plain text using 
#' \code{'saveTXT'} argument. See details.
#' 
#'@param save.file Vector of characters. Path to the output 
#'file for each  species.
#'
#'@details
#'The headers of the input file must follow the Darwin Core standard [1]. 
#'The user can see the guide using data('ID_DarwinCore) command.
#'For  more details about the formats to read and/or write, see 
#'\code{\link{readAndWrite}} function.
#'
#'@return A file for each species as data frame class.
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




delPointsOrSp <- function(data         = NULL,
                          rd.frmt      = 'readRDS',
                          path         = NULL,
                          west         = -120,
                          east         = -65,
                          south        = -30,
                          north        = 30,
                          plot.distrib = T,
                          wrt.frmt     = 'saveRDS',
                          save.file    = NULL){

  tab.info <- as.data.frame(matrix(NA,length(data),5))
  colnames(tab.info) <- c('Sp','Initial.Sp','Initial.Occ','Final.Sp',
                        'Final.Occ')
  for (i in 1:length(data)) {
   
    sp.table <- readAndWrite(action = 'read', frmt = rd.frmt ,
                               path = path, name = data[i])
    tab.info$Sp[i] <- data[i]
    tab.info$Initial.Sp[i] <- length(unique(sp.table$species))
    tab.info$Initial.Occ[i] <- nrow(sp.table)
    y <- which(as.numeric(sp.table$decimalLongitude) < west | as.numeric(sp.table$decimalLongitude) > east)
    x <- which(as.numeric(sp.table$decimalLatitude) < south | as.numeric(sp.table$decimalLatitude) > north)
    total <- unique(c(y, x))
    names <- unique(sp.table$species[total])
    if (!length(total) == 0) {
     
      if (plot.distrib == T) {
     data(wrld_simpl)
     plot(wrld_simpl)
     points(x = sp.table$decimalLongitude[-total],
            y = sp.table$decimalLatitude[-total],
            col = 'blue', pch = 20)
      points(x = sp.table$decimalLongitude[total],
            y = sp.table$decimalLatitude[total],
            col = rainbow(length(names)), pch = 20)
      #legend('topleft', legend = names, title = 'Species', 
            #fill = rainbow(length(names)), cex = 0.56, bty = 'n')
    }
      for (sp in 1:length(names)) {
      message('There are points inside of the range, you can see them in red color')
      print(paste('Check the distribution:', names[sp]))
      input1 <- readline('Delete points? \n (yes: y or not: n)= ')
      if (input1 == 'y' | input1 == 'yes') {
        find.points <- which(sp.table$species[total] == as.character(names[sp]))
        total <- total[find.points]
        sp.table <- as.data.frame(sp.table[-total,])
        } else {
          input2 <- readline('Delete species? \n (yes: y or not: n)= ')
          if (input2 == 'y' | input1 == 'yes') {
            find.points <- which(sp.table$species == as.character(names[sp]))
            sp.table <- as.data.frame(sp.table[-find.points, ])
          }
        }
  y <- which(as.numeric(sp.table$decimalLongitude) < west | as.numeric(sp.table$decimalLongitude) > east)
    x <- which(as.numeric(sp.table$decimalLatitude) < south | as.numeric(sp.table$decimalLatitude) > north)
      total <- unique(c(y, x))
      dev.off()
      }
      } else {
        print(paste(data[i],': There are no points in the range',sep = ''))
        
      }
    if (!nrow(sp.table) == 0) {
    readAndWrite(action = 'write', frmt = wrt.frmt, object = sp.table,
                   path = save.file, name = data[i])
    }
    tab.info$Final.Sp[i] <- length(unique(sp.table$species))
    tab.info$Final.Occ[i] <- nrow(sp.table)
  }
  
  return(tab.info)

}
