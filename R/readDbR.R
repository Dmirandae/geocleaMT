#'@name readDbR
#'
#'@title  Read database table using R
#'
#'@description Read a database which follows the Darwin Core Standard [1]. 
#'
#'@param data Vector of characters. Name of the input file.
#'
#'@param path.data Vector of characters. Path to the input file.
#'
#'@param cut.col   Numeric vector. Columns number to read  into database. By default, the columns 
#'c(1,78,79,200,218,219)  are read. These correspond to headers of 
#'the Darwin Core standard [1]  : gbifID, decimalLongitude, decimalLatitude,
#'elevation, speciesKey and species. See details.
#'
#'@param delt.undeterm Logical vector. If it is \code{'TRUE'} return a data table 
#'with only occurrences that have taxonomic determination until species. 
#'Otherwise, it could return all occurrences read into database.
#'
#'@param save.name Vector of characters. Name of the output file. 
#'
#'@param  wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using 
#' \code{'saveRDS'} argument, but it can be saved as plain text using 
#' \code{'saveTXT'} argument. See details.
#'
#'@param save.in  Vector od characters. Path to the output file.
#'
#'
#'@details 
#'We recommend to use this function when the database have fewer than one hundred
#'thousand occurrences.
#'This function works on R platform and can be performed on any operative 
#'system (Linux, Mac OS or Windows). If the database to read has more than 
#'one hundred thousand occurrences, we recommend to use the 
#'\code{\link{readDbBash}} function. \code{\link{readDbBash}} uses 
#'the cut function from BASH programming language and can be functional 
#'on Linux or iOS operative systems, but the \code{\link{readDbBash}}
#'function always will be faster than \code{\link{readDbR}} 
#'(until four times faster).
#'
#'Databases downloaded from Global Biodiversity  Information Facility (GBIF)[2] 
#'are exported with DarwinCore headers and the column separator is TAB.
#'
#'See \code{\link{readAndWrite}} function.
#'
#'For cut.col parameter, the numbers columns to split  must be sorted sequentially. 
#'For database download from GBIF [2], the number for each header can be seem using 
#'data('ID_DarwinCore) command on console in the ID colunm.
#'
#'For more details about the formats to read and/or write, see 
#'\code{\link{readAndWrite}} function.
#'
#'@return  writing a data table as a  data.frame class and a vector a 
#'table with descriptive statistics.
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'@note See:
#'R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{readDbR}}
#'@seealso \code{\link{readAndWrite}}
#'
#'@references
#'[1] Wieczorek, J. et al. 2012. Darwin core: An evolving community-developed biodiversity data standard. 
#' PloS One 7: e29715.
#'
#'[2] Global Biodiversity Information Facility. Available online at \url{ http://www.gbif.org/}. 
#'

readDbR <- function(data          = NULL,
                   path.data     = NULL,
                   cut.col       = c(1,78,79,200,218,219), 
                   delt.undeterm = TRUE,
                   save.name     = NULL,
                   wrt.frmt      = 'saveRDS',
                   save.in       = NULL) {

  #! Cut columns from system and read as data.frame in R. The header and
  #! 'header' and 'sep' always will be TRUE and TAB respectively.

  temp.tab <- lapply(paste(path.data, data, sep = ''), fread, 
                     colClasses = 'character', data.table = F)
  if (any(cut.col == 'all')) {
    temp.tab <- temp.tab[[1]]
  }else{
    temp.tab <- temp.tab[[1]][,cut.col]
  }
  tab.info <- as.data.frame(matrix(NA,1,3))
  colnames(tab.info) <- c('Initial.Occurr','Final.Occurr','Total.sp')
  rownames(tab.info) <- 'Initial.tab'
  tab.info$Initial.Occurr <- nrow(temp.tab)
  #! If you use the species column and need delete duplicates and row without 
  #! species name, then delete.na.in.sp == T
  if (delt.undeterm == T) {
    #! si el argumento delt.undeterm =T y ust no escogio la columna de species
    #! el proceso se detendra.
    if (any(colnames(temp.tab) == 'species') == F) {
      stop(' delt.undeterm= T , if you do not select species column and argument 
           delete.na.in.sp= T, it will generate \'Warning message\'. 
           Please, be sure that delt.undeterm= F or to select species column in 
           cut.col= argument')
    }else{
      #! si hay espacio en blanco asignelos como NA
      na <- which(temp.tab$species == '')
      if (!length(na) == 0) {
        temp.tab$species[na] <- NA
      }
      #! Elimine todos las occrurrencias que no esten determinadas hasta espcie.
      temp.tab <- subset(temp.tab, !is.na(temp.tab$species))
      readAndWrite(action = 'write', frmt = wrt.frmt, path = save.in, 
                   name = save.name, object = temp.tab) 
    }
    
  } else {
    #! si las occurrencias no determinadas no son eliminadas, entonces escriba todo.
    readAndWrite(action = 'write', frmt = wrt.frmt, path = save.in, 
                 name = save.name, object = temp.tab)
  }
  tab.info$Final.Occurr <- nrow(temp.tab)
  tab.info$Total.sp <- length(unique(temp.tab$species))
  
  return(tab.info)
}
