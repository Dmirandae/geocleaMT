#'@name readAndWrite
#'
#'@title Read and write tables
#'
#'@description 
#'Read or write tables in two specific formats for standardizing the 
#'cleaning process.  This function reads the input and writes the output.
#'
#'@param action Vector of characters. If \code{'read'} string, the function will 
#'read the file. If \code{'write'} string, the function will write output file.
#'
#'@param frmt Vector of characters. The format to read or write can be of 
#'two types: plain text \code{'readTXT'}/\code{'saveTXT'} or a R object
#' \code{'saveRDS'}/\code{'saveRDS'}.
#'
#'@param path Vector of characters. Path to the input file or the 
#'destination Path to the output file.See details.
#'
#'@param name Vector of characters. Name of the input file or name of file to
#'write.
#'
#'@param object  Vector of characters. Object to write. See details.
#'
#'@details It reads and/or writes a file in plain text format (txt) using the 
#'functions \code{\link{read.table}} and/or \code{\link{write.table}}. 
#'To maintain the standard manipulation the arguments for both 
#'functions \code{\link{read.table}} and \code{\link{write.table}} parameters, are the default,
#' except for the parameters: sep= 'TAB', header= TRUE, quotes= FALSE and rownames = FALSE.
#' 
#' If the argument in the action parameter is \code{'write'}, the argument in object 
#'and name parameters must be assigned. But, if the argument in 
#'the action parameter is \code{'read'}, the argument in object  parameter
#' must be omitted.
#' 
#'@return For action = \code{'read'}, load the file given for the user. 
#'For action = \code{'write'}, save an output file. Both depend on the frmt parameter.
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'
#'@note See:
#'R-Alarcon V. and Miranda-Esquivel DR. (submitted) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{saveRDS}}
#'@seealso \code{\link{write.table}}
#'
#'


readAndWrite <- function(action = NULL, 
                         frmt   = NULL, 
                         path   = NULL, 
                         name   = NULL, 
                         object = NULL){
#!  if action is read format is RDS
  if (action == 'read') {
    if (frmt == 'readRDS') {
      
      sp <- readRDS(paste(path, name, sep = ''))
      } else {
        #! else a '.TXT'
        sp <- read.table(paste(path, name, sep = ''), sep = '\t',header = T, fill=T)
      }
    return(sp)
    } else {
     #! write RDS
      if (frmt == 'saveRDS') {
        # write  a '.RDS'
        saveRDS(object,paste(path, name, sep = ''))
        }else{
          #! write  a  '.TXT'
          write.table(object,paste(path, name, sep = ''),sep = '\t', row.names = F,quote = T)
        }
    }
}
