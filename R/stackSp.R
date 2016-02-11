#'@name stackSp
#'
#'@title Join species files
#'
#'@description Join all independent files into a single multi-species file.
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
#'@param save.name Vector of characters. Name of the output file. 
#'
#'@param wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using 
#' \code{'saveRDS'} argument, but it can be saved as plain text using 
#' \code{'saveTXT'} argument. See details.
#' 
#'@param save.staking.in Vector of characters. Path to the output 
#'file.
#'
#'
#'@details
#'The headers of the input file must follow the Darwin Core standard [1]. 
#'The user can see the guide using data('ID_DarwinCore) command.
#'For  more details about the formats to read and/or write, see 
#'\code{\link{readAndWrite}} function.
#'
#'@return  The output file will be saved with all species concatenated. 
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

stackSp <- function(data            = NULL,
                    rd.frmt         = 'readRDS',
                    path            = NULL,
                    save.name       = NULL,
                    save.staking.in = NULL,
                    wrt.frmt        = 'saveRDS'){
  
  stack.temp0 <- NULL
  #! lea la primera especie
  for (i in 1:length(data)) {
    if (is.null(stack.temp0)) {
      stack.temp0 <- readAndWrite(action = 'read', frmt = rd.frmt ,
                                    path = path, name = data[i])
      
      } else {
        #! lea la segunda especie
        stack.temp1 <- readAndWrite(action = 'read', frmt = rd.frmt ,
                                      path = path, name = data[i])
        #!  una por columnas la primera especie con la segunda
        stack.temp0 <- rbind.fill(stack.temp0, stack.temp1)
      }
    print(paste('Species',i,':',data[i],sep = ''))
  }
  tab.info <- as.data.frame(matrix(NA,1,2))
  colnames(tab.info) <- c('TotalSp', 'TotalOccurrences')
  tab.info$TotalSp <- length(unique(stack.temp0$species))
  tab.info$TotalOccurrences <- nrow(stack.temp0)
  readAndWrite(action = 'write', frmt = wrt.frmt , object = stack.temp0,
                 path = save.staking.in, name = save.name)
  return(tab.info)
}

