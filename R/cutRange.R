#'@name cutRange
#'
#'@title  Species on a specific elevation range
#'
#'@description Get species distributed on a specific range of altitude.
#'
#'@param data Vector of characters. Name of the input file.
#'
#'@param rd.frmt Vector of characters. File format to read. 
#'By default it will be read  as a  R object using the
#' \code{'readRDS'} argument, but it can be read as plain text using the
#' \code{'readTXT'} argument. See details.
#'
#'@param path Vector of characters. Path to the input file.
#'
#'@param range.from Numeric vector. Lower bound of the range of the distribution
#'
#'@param range.to Numeric vector. Upper bound of the range of the distribution.
#'
#'@param  wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using the
#' \code{'saveRDS'} argument, but it can be saved as plain text using the
#' \code{'saveTXT'} argument. See details.
#'
#'@param save.inside.in Vector of characters. Path to the output 
#' file with the species whose occurrences are on the range 
#' assigned in \code{range.from} to \code{range.to} parameters. Output file 
#' will be called <inside.range>.
#'
#'@param save.outside.in Vector of characters. Path to the output 
#'file with the species whose occurences are not on the range 
#'assigned in \code{range.from} and \code{range.to} parameters. Output file 
#'will be called <outside.range>.
#'
#'@details For more details about the formats to read and/or write, see 
#'\code{\link{readAndWrite}} function.
#'
#'The headers of the input file must follow the Darwin Core standard [1]. 
#'The user can see the guide using data('ID_DarwinCore) command.
#'
#'@return A table data.frame class with only the species whose occurences is on range 
#'assigned in \code{range.from} and \code{range.to} parameters, and a vector with a table with descriptive quantities.
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
cutRange <- function(data            = NULL,
                     path            = NULL,
                     rd.frmt         = 'readRDS',
                     range.from      = 0,
                     range.to        = 1000,
                     wrt.frmt        = 'saveRDS',
                     save.inside.in  = NULL,
                     save.outside.in = NULL) {
  
  data <- readAndWrite(action = 'read', frmt = rd.frmt, 
                            path = path, name = data)
  tab.info <- as.data.frame(matrix(NA, 1, 6))
  colnames(tab.info) <- c('Total.Occ','Total.Sp', 'Uniq.Sp.Inside', 
                          'Uniq.Sp.Outside','Sp.In.and.Outside','Total.In.Range')
  tab.info$Total.Sp <- length(unique(data$species))
  tab.info$Total.Occ <- nrow(data)
  data <- as.data.frame(unique(data))
  #Select species inside range
  inside.range <- subset(data, (as.numeric(as.character(data$elevation))) >
                      range.from & (as.numeric(as.character(data$elevation)))
                      < range.to)
  #Select species outside range
  outside.range <- subset(data, !(as.numeric(as.character(data$elevation)) >
                        range.from & as.numeric(as.character(data$elevation))
                        < range.to))
  # Write table with species inside the range
  readAndWrite(action = 'write', frmt = wrt.frmt, path = save.inside.in,
                 name = 'inside.range', object = inside.range)
  readAndWrite(action = 'write', frmt = wrt.frmt, path = save.outside.in,
                 name = 'outside.range', object = outside.range)
  #Count different items like Total species, total occurrences etc
  tab.info$Uniq.Sp.Inside <- length(inside.range$species[which(!unique(inside.range$species) %in%
                                                                  unique(outside.range$species))])
  tab.info$Uniq.Sp.Outside <- as.numeric(length(outside.range$species[which(!unique(outside.range$species) %in%
                                                              unique(inside.range$species))]))
  tab.info$Sp.In.and.Outside <-as.numeric(tab.info$Total.Sp) - as.numeric(tab.info$Uniq.Sp.Outside) - as.numeric(tab.info$Uniq.Sp.Inside)
  tab.info$Total.In.Range <- as.numeric(tab.info$Total.Sp) - as.numeric(tab.info$Uniq.Sp.Outside)
  
   return(tab.info)
}
