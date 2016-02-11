#'@name pointsAtSea
#'
#'@title Separate occurrences on Earth and at sea
#'
#'@description  Delete duplicates, separating records
#'on Earth and at sea for each species.
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
#'@param wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using 
#' \code{'saveRDS'} argument, but it can be saved as plain text using 
#' \code{'saveTXT'} argument. See details.
#' 
#'@param save.AtSea.in Vector of characters. Path to the output 
#'file for each  species with the coordinates at sea.
#'
#'
#'@param save.OnEarth.in Vector of characters. Path to the output 
#'file for each  species with the coordinates on earth.
#'
#'@details For  more details about the formats to read and/or write, see 
#'\code{\link{readAndWrite}} function.
#'
#'The headers of the input file must follow the Darwin Core standard [1]. 
#'The user can see the guide using data('ID_DarwinCore) command.
#'
#'@return By each species, if this is the case, two output files 
#'data.frame class: The first one has the coordinates at sea for the species and
#' the second one has the coordinates on earth for the same species.
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


pointsAtSea <- function(data            = NULL,
                        path            = NULL,
                        rd.frmt         = 'readRDS',
                        wrt.frmt        = 'saveRDS',
                        save.OnEarth.in = NULL,
                        save.AtSea.in   = NULL) {
  
  tab.info <- as.data.frame(matrix(data = NA, nrow = length(data), ncol = 4))
  colnames(tab.info) <- c('Species', 'Total.occurrences', 'On.earth', 'At.sea')
  data(wrld_simpl)
  for (i in 1:length(data)) {
    sp <- readAndWrite(action = 'read', frmt = rd.frmt,
                         path = path, name = data[i])
    tab.info$Species[i] <- data[i]
    coord.tab <- as.data.frame(sp[which(!duplicated(sp[, c('decimalLatitude',
                                           'decimalLongitude', 'species')])), ])
    tab.info$Total.occurrences[i] <- nrow(coord.tab)
    coordinates(coord.tab) <- coord.tab[, c('decimalLongitude', 'decimalLatitude')]
    proj4string(coord.tab) <- proj4string(wrld_simpl)
    wrld_simpl <- as(wrld_simpl, 'SpatialPolygons')
    on.earth <- !is.na(over(coord.tab, wrld_simpl))
    if (any(on.earth) == T) {
      points.on.earth <- as.data.frame(coord.tab[on.earth, ])
      points.on.earth <- points.on.earth[,-c(length(points.on.earth) - 1,
                                         length(points.on.earth))]
      tab.info$On.earth[i] <- nrow(points.on.earth)
      if (length(points.on.earth) == nrow(coord.tab)) {
        tab.info$At.sea[i] <- 0
      }else{
        tab.info$At.sea[i] <- nrow(as.data.frame(coord.tab[!on.earth, ]))
      }
      readAndWrite(action = 'write', frmt = wrt.frmt, 
                         path = save.OnEarth.in, name = data[i], 
                         object = points.on.earth)
    }
    if (any(on.earth) == F) {
      points.at.sea <- as.data.frame(coord.tab[!on.earth, ])
      points.at.sea <- points.at.sea[,-c(length(points.at.sea) - 1,
                                             length(points.at.sea))]
      tab.info$At.sea[i] <- nrow(points.at.sea)
      if (length(points.at.sea) == nrow(coord.tab)) {
        tab.info$on.earth[i] <- 0
      }else{
        tab.info$At.sea[i] <- nrow(as.data.frame(coord.tab[on.earth, ]))
      }
      readAndWrite(action = 'write', frmt = wrt.frmt, 
                       path = save.AtSea.in, name = data[i], 
                       object = points.at.sea)
    }
    print(paste(i, ':', data[i], sep = ' '))
  }
  
  return(tab.info)

}
