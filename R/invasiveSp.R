#'@name invasiveSp
#'
#'@title Separate invasive species
#'
#'@description  Separate species categorized as invasive for The Global
#'Invasive Species Information Network (ISSG) [1] and Island Biodiversity
#'and Invasive Species (IBIS) [2].
#'
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
#'@param save.Sp.list Logic. If \code{'TRUE'}, two files are saved, one has the
#'list of invasive species and other has the list of non-invasive species,
#'both have two columns genus and  specific epithet and it will be used as
#'input for the \code{\link{gbifDownSp}} function.
#'
#'@param starts.in starts.in Numeric vector. Row number to start the process.
#'
#'@param wrt.frmt Vector of characters. Format to save output
#'file. By default it will be written  as a  R object using
#' \code{'saveRDS'} argument, but it can be saved as plain text using
#' \code{'saveTXT'} argument. See details.
#'
#'@param save.foreign.in Vector of characters. Path to
#'write the output file with species that are categorized as invasive.
#'
#'@param save.non.foreign.in Vector of characters. Path to
#'write the output file with species that are not categorized as invasive.
#'
#'@param save.temp.in Vector of characters. Path to the temporal file
#'as backup of the process. If the process is stopped, the user could reset it
#'assigning the row number in \code{start.in} parameter and the process will start again
#'using the temporal file.
#'
#'
#'@details
#'For more details about the formats to read and/or write, see
#'\code{\link{readAndWrite}} function.
#'
#'If the argument in the \code{save.Sp.list} parameter is \code{'TRUE'},
#'the files will be saved in the path assigned in the \code{path} parameter.
#'
#'The headers of the input file must follow the Darwin Core standard [3].
#'The user can see the guide using data('ID_DarwinCore) command.
#'
#'@return A table data.frame class with a list of invasive species.
#'A table data.frame class with a list of non-invasive species.
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
#'[1] Lowe, S. et al. 2000. 100 of the world's worst invasive alien species: A selection from the global invasive species database. The Invasive Species Specialist Group (ISSG) a specialist group of the Species Survival Commission (SSC) of the World Conservation Union (IUCN).
#'
#'[2] Kells, S. S. and Worswick, C. 1997. An introduction to the IBIS database.  Melbourne Institute of Applied Economic and Social Research.
#'
#'[3] Wieczorek, J. et al. 2012. Darwin core: An evolving community-developed biodiversity data standard. 
#' PloS One 7: e29715.


invasiveSp <- function(data                = NULL,
                       rd.frmt             = 'readRDS',
                       path                = NULL,
                       starts.in           = 1,
                       save.Sp.list        = TRUE,
                       wrt.frmt            = 'saveRDS',
                       save.foreign.in     = NULL,
                       save.non.foreign.in = NULL,
                       save.temp.in        = NULL) {
  #! Si la llamada se cae, reestablesca el proceso usando starts.in
  if (!starts.in == 1) {
    foreign.sp <- as.data.frame(readAndWrite(action = 'read', frmt = 'readRDS',
                                             path = save.temp.in,
                                             name = 'foreignSp.temp'))
  }else{
    #! lea el archivo
    data <- as.data.frame(readAndWrite(action = 'read', frmt = rd.frmt,
                                       path = path, name = data))

    #! separe el nombre de la species en genero y epiteto especifico.
    split.sp <- colsplit(string = data$species, pattern = ' ',
                         names = c('genus', 'specificEpithet'))
    foreign.sp <- cbind('Foreign' = NA, split.sp, data)
    message('If the process is stopped, you can check the temporal file: \'foreignSp.temp\'')
  }
   tab.info <- as.data.frame(matrix(NA,1,3))
  colnames(tab.info) <- c('Sp', 'Foreign.Sp', 'Non.Foreign.Sp')
  tab.info$Sp <- length(unique(foreign.sp$species))
  foreign.sp.table <- as.data.frame(cbind('genus' = as.character(foreign.sp$genus[which(!duplicated(foreign.sp[,'species']))]),
                 'specificEpithet' = as.character(foreign.sp$specificEpithet[which(!duplicated(foreign.sp[,'species']))])))

  #! Call to API Foreign and ask for record of species
  #! pregunte por el registro  de la especie en la base de Ibis interna del paquete
  data('IBISlist')
  for (i in starts.in:nrow(foreign.sp.table)) {
    #! escriba el archivo temporal en caso de perdida
    readAndWrite(action = 'write',frmt = 'saveRDS', path = save.temp.in,
                 name = 'foreignSp.temp', object = foreign.sp)
    #! Esta la especie en el archivo de IBIS
    ibis <- which(paste(foreign.sp.table$genus[i],
                        foreign.sp.table$specificEpithet[i], sep = ' ') ==
                  as.character(unique(IBISlist$Species)))

    #! sino esta
    if (length(ibis) == 0) {
      #! revise en la base de datos electronica del 'Invasive Species specialist Group'
      urls <- paste('http://www.issg.org/database/species/search.asp?sts=sss&st=sss&fr=1&x=13&y=9&sn=',
                  foreign.sp.table$genus[i],'+',
                  foreign.sp.table$specificEpithet[i],'&rn=&hci=-1&ei=-1&lang=EN', sep = '')
    doc <- getURL(urls)
    issg1 <- grep(x = doc,pattern = 'No invasive species')
    issg2 <- grep(x = doc,pattern = 'incomplete information')
    #! si esta registrada como especie no invasora
    if (any(c(issg1,issg2)) == 1 ) {
      #! asigne = 0 a no invasora
      x.which <- which(paste(foreign.sp.table$genus[i],
                             foreign.sp.table$specificEpithet[i],
                             sep = ' ') == foreign.sp$species)
      foreign.sp$Foreign[x.which] <- 0
      }else{
        #! si esta registrada como especie invasora asigne = 1
        x.which <- which(paste(foreign.sp.table$genus[i],
                             foreign.sp.table$specificEpithet[i],
                             sep = ' ') == foreign.sp$species)
        foreign.sp$Foreign[x.which] <- 1
        }
    }else{
      #! si se en cuentra registrada en al base de IBIS asigne  = 1
      x.which <- which(paste(foreign.sp.table$genus[i],
                             foreign.sp.table$specificEpithet[i],
                             sep = ' ') == foreign.sp$species)
      foreign.sp$Foreign[x.which] <- 1
    }

    cat(paste(i,foreign.sp.table$genus[i],foreign.sp.table$specificEpithet[i],'- Status:',foreign.sp$Foreign[x.which][1],'\n',sep = ' '))
  }
  #! Serape las asignadas en 1 (invasoras)
  foreign <- subset(foreign.sp, foreign.sp$Foreign == 1)
  foreign <- as.data.frame(unique(foreign[, !colnames(foreign) ==
                                            c('Foreign', 'genus', 'specificEpithet')]))
  #! escribalas en un archivo
  readAndWrite(action = 'write', frmt = wrt.frmt, path = save.foreign.in,
               name = 'foreignSp', object = foreign)
  #! separe las noinvasoras
  no.foreign <- subset(foreign.sp, foreign.sp$Foreign == 0)
  no.foreign <- as.data.frame(unique(no.foreign[, !colnames(foreign) ==
                                                  c('Foreign', 'genus', 'specificEpithet')]))
  #! escribalas en un Archivo
  readAndWrite(action = 'write', frmt = wrt.frmt, path = save.non.foreign.in,
               name = 'non.foreignSp', object = no.foreign)
  #! si solo desea guardar el lista  de espcie
  if (save.Sp.list == T) {
    #! si la el arg es T eontonces escriba solo la lista de species
    input.rgbif.foreign <- as.data.frame(unique(subset(foreign.sp[,'species'],
                                                       foreign.sp$Foreign == 1)))
    colnames(input.rgbif.foreign) <- 'species'
    readAndWrite(action = 'write', frmt = wrt.frmt, path = save.foreign.in,
                 name = 'input.rgbif.foreign', object = input.rgbif.foreign)
    input.rgbif.no.foreign <- as.data.frame(unique(subset(foreign.sp[,'species'],
                                                          foreign.sp$Foreign == 0)))
    colnames(input.rgbif.no.foreign) <- 'species'
    readAndWrite(action = 'write', frmt = wrt.frmt, path = save.non.foreign.in,
                 name = 'input.rgbif.non.foreign', object = input.rgbif.no.foreign)
  }
  tab.info$Foreign.Sp <- length(unique(foreign$species))
  tab.info$Non.Foreign.Sp <- length(unique(no.foreign$species))
  return(tab.info)

}
