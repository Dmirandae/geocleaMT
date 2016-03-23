#'@name readDbBash
#'
#'@title Read database table using bash commands.
#'
#'@description Read a database which follows the Darwin Core Standard [1]. 
#'
#'@param data Vector of characters. Name of the input file.
#'
#'@param path.data Vector of characters. Path to the input file.
#'
#'@param cut.col   Numeric vector. Columns number to read into database. By default, the columns 
#'c(1,78,79,200,218,219)  are read. These correspond to headers of 
#'the Darwin Core satandard [1]  : gbifID, decimalLongitude, decimalLatitude,
#'elevation, speciesKey and species. See details.
#'
#'@param delt.undeterm Logical vector. If it is \code{'TRUE'} return a data table 
#'with only occurrences that have taxonomic determination until species. 
#'Otherwise, it could return all occurrences read into database.
#'
#'@param save.name Vector of characters. Name of the output file. 
#'
#'@param  wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using the
#\code{'saveRDS'} argument, but it can be saved as plain text using the
#\code{'saveTXT'} argument. See details.
#'
#'@param save.in  Vector od characters. Path to the output file.
#'
#'@details 
#'We recommend to use this function when the database has more than one hundred
#'thousand occurrences and / or the computer has low memory.
#'\code{\link{readDbBash}} uses the cut function from BASH programming 
#'language and can be functional on Linux or Mac operative systems.
#'If this is not the case, we recomended to use the \code{\link{readDbR}}
#'which runs into the R platform and can be used on any operative 
#'system (Linux, Mac, or Windows). However, the \code{\link{readDbBash}}
#'function always will be faster than \code{\link{readDbR}} (up to four 
#'times faster).
#'
#'Databases downloaded from the Global Biodiversity  Information Facility (GBIF) [2] 
#'are exported with DarwinCore headers and the separator columns is TAB, 
#'and hence all databases read using this functions must be able TAB as 
#'separator. See \code{\link{readAndWrite}} function.
#'
#'For cut.col parameter, the numbers columns to split  must be sorted sequentially. 
#'For databases downloaded from GBIF [2], the number for each header can be seem using 
#'data('ID_DarwinCore) command on console.
#'
#'For more details about the formats to read and/or write, see 
#'\code{\link{readAndWrite}} function
#'
#'@return  writing a data table in Data.frame class and as vector return a 
#'table with descriptive quantities.
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

readDbBash <- function(data         = NULL,
                      path.data     = NULL,
                      cut.col       = c(1,78,79,200,218,219), 
                      delt.undeterm = TRUE,
                      save.name     = NULL,
                      wrt.frmt      = 'saveRDS',
                      save.in       = NULL) {
  
  initialWd<- getwd()
  if (cut.col[1] == 'all') {
    #! Argumento all indica leer todas las 255 columnas de DarwinCore2
    columns <- paste(seq(from = 1,to= 223,by = 1),collapse= ',')
    } else {
      #! Lea columnas especificas
      columns <- paste(cut.col, collapse = ',')
    }
  #! Pregunte: Cual es el sistema operativo en el que se esta ejecutando 
  #! la operacion
  system <- Sys.info()['sysname']
  if (system == 'Windows') {
    message(cat('Your OS is Windows, you must configure your bash emulator and be sure you have > R version 3.2.1  \n'))
    } else if (system == 'Linux') {
      cat('Your OS is Linux. Please, be sure you have > R version 3.2.1  \n')
      } else {
        cat('Your OS is MacOS. Please, be sure you have > R version 3.2.1 \n')
      }
  #! Cut columns from system and read as data.frame in R. The header and
  #! 'header' and 'sep' always will be TRUE and TAB respectively.
  setwd(path.data)
  temp.tab <- read.table(pipe(paste('cut -f', columns, data, sep = ' ')), 
                             header = TRUE, sep = '\t', na.strings = NA )
  tab.info <- as.data.frame(matrix(NA,1,3))
  colnames(tab.info) <- c('Initial.Occ','Final.Occ','Total.Sp')
  #rownames(tab.info) <- 'Initial.tab'
  tab.info$Initial.Occ <- nrow(temp.tab)
  setwd(initialWd)
  #! If you use the species column and need delete duplicates and row without 
  #! species name, then delete.na.in.sp == T
  if (delt.undeterm == T) {
    #! si el argumento delt.undeterm =T y ust no escogio la columna de species
    #! el proceso se detendra.
    if (any(colnames(temp.tab) == 'species') == F) {
      stop(cat(' delt.undeterm = T , if you do not select the species column
       and argument delete.na.in.sp = T, it will generate \'Warning message\'.
        Please, be sure that delt.undeterm = F or to select the species column 
        in the cut.col parameter \n'))
    }else{
      #! si hay espacion en blanco asignelos como NA
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
  
  tab.info$Final.Occ <- nrow(temp.tab)
  tab.info$Total.Sp <- length(unique(temp.tab$species))
  
  return(tab.info)

}
