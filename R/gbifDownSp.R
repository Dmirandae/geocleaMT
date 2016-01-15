#'@name gbifDownSp
#'
#'@title Download occurrences for a list of species
#'
#'@description Download occurrences from the Global Biodiversity Information Facility (GBIF) [1]
#'
#'@param sp.name Vector of characters. A species list to download. See details.
#'
#'@param taxon.key Vector of characters. A codes list, these codes are assigned by GBIF [1]
#' to each species. See details.
#'
#'@param genus Vector od Characters. A genera list to download.
#'
#'@param epithet Vector of characters. A epithet list to download.
#'
#'@param starts.in Numeric vector. Row number to start the process.
#'
#'@param wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using
#' \code{'saveRDS'} argument, but it can be saved as plain text using
#' \code{'saveTXT'} argument. See details.
#'
#'
#'@param save.download.in Character.  Vector of characters. Path to
#'the output file for each species downloaded.
#'
#'
#'@param ... All parameters that can parsed to \code{\link[rgbif]{occ_search}}
#' function from \code{rgbif} package [2] as limit, basisOfRecord, country, 
#' publishingCountry, lastInterpreted, geometry, collectionCode, institutionCode and year.
#'
#'@details Additional parameters (...) see \code{\link[rgbif]{occ_search}}
#' function from \code{rgbif} package.
#' The user can only use one parameter at a time: \code{sp.name}, \code{taxon.key} or \code{genus}
#' and/or \code{epithet}.
#'
#'For  \code{sp.name} parameter, a list of species names  could be assigned.
#'The species names must have genus and epithet, separated by a single
#'space.
#'
#'For \code{taxon.key} parameter the list have to be the ID number that assigns GBIF
#'to each taxonomical level.
#'
#'
#'The headers of the input file must follow the Darwin Core standard [3].
#'The user can see the guide using data('ID_DarwinCore) command.
#'
#'For more details about the formats to read and/or write, see
#'\code{\link{readAndWrite}} function.
#'
#'@return  Save one file by species. This file is a list of all occurrences recorded from GBIF.
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'
#'
#'@note See:
#'R-Alarcon V, Miranda-Esquivel DR.(in prep) geocleaMT: An R package to cleaning
#'geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{readAndWrite}}
#'@seealso \code{\link[rgbif]{occ_search}}
#'
#'
#'@references
#'[1]  Global Biodiversity Information Facility. Available online at \url{ http://www.gbif.org/}. 
#'
#'
#'[2] Chamberlain, S. et al. 2015. rgbif: Interface to the Global Biodiversity Information Facility `API'. R package version 0.9.0. The Comprehensive R Archive Network (CRAN). Available online at \url{http://cran.r-project.org/package=rgbif}.
#'
#'
#'[3] Wieczorek, J. et al. 2012. Darwin core: An evolving community-developed biodiversity data standard. 
#' PloS One 7: e29715.
#'
#'


gbifDownSp <- function(    sp.name           = NULL,
                            taxon.key         = NULL,
                            genus             = NULL,
                            epithet           = NULL,
                            starts.in         = 1,
                            wrt.frmt        = 'saveRDS',
                            save.download.in  = NULL,
                            ...) {

  #! if the scientific name is split in genus and epithet, join columns and search Species
   #! cat.col <- c(genus, epithet, genus.Epithet.Tab)
 #! if (!is.null(cat.col)) {
    #! col.names <- length(which(colnames(genus.Epithet.Tab) == 'genus' | colnames(genus.Epithet.Tab) == 'epithet'))
    #! if (!col.names == 2) {
      #! stop('The headers columnas should be: \'genus\', \'epithet\' ')
      #! } else {
        #! for (i in starts.in:length(genus)) {
                 #! sp.name  <- paste(as.character(genus.Epithet.Tab$genus[i]), as.character(genus.Epithet.Tab$epithet[i]), sep=' ')
               #! tab.temp <- occ_search(scientificName = as.character(sp.name), ...)
                #! data.tab <- as.data.frame(tab.temp$data)
               #!  readAndWrite('write',frmt = wrt.frmt,path = save.download.in,
                   #!           name = as.character(sp.name),object =data.tab )
                 #! print(paste('Species:',sp.name,'#',i,sep=' '))
               #! }
      #!}
        cat.col <- cbind(genus, epithet)
  if (!is.null(cat.col)) {
  colnames(cat.col)<- c('genus','epithet')
        for (i in starts.in:nrow(cat.col)) {
                 sp.name  <- paste(as.character(cat.col$genus[i]), 
                                   as.character(cat.col$epithet[i]), sep=' ')
                 tab.temp <- occ_search(scientificName = as.character(sp.name), ...)
                 data.tab <- as.data.frame(tab.temp$data)
                 readAndWrite('write',frmt = wrt.frmt,path = save.download.in,
                              name = as.character(sp.name),object =data.tab )
                 print(paste('Species:',sp.name,'#',i,sep=' '))
               }
    }else{
#! If you use Species name, you can use taxon.key
      cat.srt <- c(sp.name, taxon.key)
      if (!is.null(sp.name) & !is.null(taxon.key)) {
        stop('You can download only one option, Species or taxon.key ')
        }else{
          if (!is.null(sp.name)) {
#! Look up Species from Scientific name

            for (i in starts.in:nrow(sp.name)) {
            tab.temp <- occ_search(scientificName = as.character(sp.name$species[i]))
            data.tab <- as.data.frame(tab.temp$data)
            readAndWrite(action='write',
                         frmt = wrt.frmt,
                         path = save.download.in,
                         name = as.character(sp.name$species[i]),
                         object =data.tab )
            print(paste('Species:',sp.name$species[i],'#',i,sep=' '))

            }
            }else{
#! Look up Species from taxon.key
              for(i in starts.in:length(taxon.key)){
              tab.temp <- occ_search(taxon.key=taxon.key[i], ...)
              data.tab <- as.data.frame(tab.temp$data)
              readAndWrite('write',frmt = wrt.frmt,path = save.download.in,
                           name = as.character(taxon.key[i]),object =data.tab )
              print(paste('Species:',sp.name[i],'#',i,sep=' '))
            }
        }
        }
    }
}
