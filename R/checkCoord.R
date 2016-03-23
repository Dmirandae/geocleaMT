#'@name checkCoord
#'@title Check coordinates
#'
#'@description Separate coordinates with a format different to decimal degrees
#'and check that the coordinates in this format are into range (latitude: -90, 90 degrees and
#' longitude: -180, 180 degrees)
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
#'@param wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using
#' \code{'saveRDS'} argument, but it can be saved as plain text using
#' \code{'saveTXT'} argument. See details.
#'
#'@param save.wrong.in Vector of characters. Path to the output
#'file for each species with coordinates in the wrong format.
#'
#'
#'@param save.right.in Vector of characters. Path to the output
#' file for each species with coordinates in the correct format.
#'
#'@details For more details about the formats to read and/or write, see
#' \code{\link{readAndWrite}} function.
#'
#' The headers of the input file must follow the Darwin Core standard [1].
#' The user can see the guide using data('ID_DarwinCore) command.
#'
#'
#'@return  For each species, if this is the case, two output files
#' data.frame class: The first one has the correct coordinates for the species and
#' the second one has the wrong coordinates for the same species.
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'@note See:
#'R-Alarcon V. and Miranda-Esquivel DR. (submitted.) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{readAndWrite}}
#'
#'@references
#'[1] Wieczorek, J. et al. 2012. Darwin core: An evolving community-developed biodiversity data standard. 
#' PloS One 7: e29715.
#'
checkCoord<-function(data          = NULL,
                     path     = NULL,
                      rd.frmt     = 'readRDS',
                      wrt.frmt    = 'saveRDS',
                      save.right.in = NULL,
                      save.wrong.in = NULL) {
  formt.lat <- c('\'', '\'', 'N', 'S')
  formt.long <- c('\'', '\'', 'E', 'W')
  cte <- NULL
  for (i in 1:length(data)) {
    sp <- readAndWrite(action = 'read', frmt = rd.frmt,
                         path = path, name = data[i])
    for (j in 1:length(formt.lat)) {
    decimal.point <- which(grep(pattern = formt.lat[j], sp$ddecimallLatitude[i]) |
                             grep(pattern = formt.long[j], x = sp$decimalLongitude[i]))
    cte <- cte+decimal.point
    }
    if (!length(cte) == 0) {
      message('Coordinates are not in decimalLatitude or  decimalLongitude format. Check coordinates with different format(s)')
      } else {
        wrong.coord.temp <- which(!(as.numeric(as.character(sp$decimalLatitude)) > -90 &
                                      as.numeric(as.character(sp$decimalLatitude)) < 90) |
                                  !(as.numeric(as.character(sp$decimalLongitude)) > -180 &
                                      as.numeric(as.character(sp$decimalLongitude)) < 180))
        if (!length(wrong.coord.temp) == 0) {
          wrong.coord <- as.data.frame(sp[wrong.coord.temp, ])
          right.coord <- as.data.frame(sp[!wrong.coord.temp, ])
          readAndWrite(action = 'write', frmt = wrt.frmt,
                         path = save.wrong.in, name = data[i],
                         object = wrong.coord)
          message(paste(data[i],':There are wrong coordinates. Check',
                        save.wrong.in,sep=''))
          if (!nrow(right.coord) == 0){
          readAndWrite(action = 'write', frmt = wrt.frmt,
                         path = save.right.in, name = data[i],
                         object = right.coord)}
          } else {
            #print(paste('Checked:', i,' ', data[i],sep=''))
            readAndWrite(action = 'write', frmt = wrt.frmt,
                           path = save.right.in, name = data[i], object = sp)
          }
      }
  }
}
