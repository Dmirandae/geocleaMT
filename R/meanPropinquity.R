#'@name meanPropinquity
#'
#'@title  Mean Propinquity
#'
#'@description Calculate the mean propinquity[1] and some descriptors of the distribution.
#'
#'@param coord.table data frame class. Table with coordinates (decimalLongitude,
#' decimalLatitude). If the process will be calculated by species, the table must
#' include the species column.
#'
#'@param calculatedBy Vector of characters. If the argument is \code{'species'}, 
#'the mean propinquity will be calculated for each species. If it is \code{'whole'} 
#'the propinquity will be calculated as a whole.
#'
#'
#'@param plot.dist Logical vector. If \code{'TRUE'} a density plot for the distance 
#'between occurrences will be plotted and saved, by species or as a whole. If 
#' \code{'FALSE'} there will be no plot.
#'
#'@param  plot.onMap Logical vector. If \code{'TRUE'} a map of occurences  will be 
#'plotted and saved, by species or as a whole. If \code{'FALSE'} there will be no plot.
#'
#'@param wrt.frmt Vector of characters. Format to save the output file.
#' By default it will be written as a R object using 
#' \code{'saveRDS'} argument, but it can be saved as plain text using 
#' \code{'saveTXT'} argument. See details.
#'
#'@param save.info.in Vector of characters. Path to the output 
#'file with information about Mean propinquity, median, mode, minimal spanning 
#'tree; by species or as a whole.
#' 
#'@param save.plot.in Vector of characters. Path to the output 
#'file for each plot by species or as a whole.
#'
#'
#'@details If the process uses the parameters plot.dist and plot.onMap as \code{'TRUE'}, the process will be slow.
#'The headers of the input file must follow the Darwin Core standard [2]. 
#'The user can see the guide using data('ID_DarwinCore) command.
#'For  more details about the formats to  write, see 
#'\code{\link{readAndWrite}} function.
#'
#'@return  The output file will be saved with all species concatenated. 
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'@note See:
#'R-Alarcon V. and Miranda-Esquivel DR. (submitted) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#'@seealso \code{\link{readAndWrite}}
#'
#'@references
#'[1] Rapoport. E.H. 1975 Areografia:  Estrategias  Geograficas  de  las  Especies. Mexico, Fondo de Cultura Economica
#' 
#'[2] Wieczorek, J. et al. 2012. Darwin core: An evolving community-developed biodiversity data standard. 
#' PloS One 7: e29715.




meanPropinquity <- function(coord.table  = NULL,
                            calculatedBy   = 'species',
                            wrt.frmt     = 'saveTXT',
                            save.info.in = NULL,
                            plot.dist    = FALSE,
                            plot.onMap   = FALSE,
                            save.plot.in = NULL){
  
  if (any(plot.dist, plot.onMap) == TRUE ) {
    if (is.null(save.plot.in)) {
      message('If any plot argument is TRUE, please : Assign the \'save.plot.in\' argument ')
    }
  }
  
  coord.table$decimalLongitude <- as.numeric(as.character(coord.table$decimalLongitude))
  coord.table$decimalLatitude <- as.numeric(as.character(coord.table$decimalLatitude))
    
  if (!calculatedBy == 'species') {
    uniqueAll <- coord.table[!duplicated(
      coord.table[, c('decimalLongitude', 'decimalLatitude')]), ]
    coordinates(uniqueAll) <- uniqueAll[, c('decimalLongitude', 'decimalLatitude')]
    distanPoints <- dist(coordinates(uniqueAll))
    mst <- spantree(d = distanPoints)
    meanDist <- mean(mst$dist,na.rm = T)
    medianDist <- median(mst$dist,na.rm = T)
    sdDist <- sd(mst$dist)
    minDist <- mst$dist[which.min(mst$dist)]
    maxDist <- mst$dist[which.max(mst$dist)]
    skewnessDist <- skewness(x = mst$dist, method = 'bickel', na.rm = T)
    modeDist <- mlv(mst$dist,method = 'mfv', na.rm = T)
    modeSkewness <- modeDist$skewness
    infoTable <- as.data.frame(matrix(data = NA,nrow = 1,ncol = 9))
    colnames(infoTable) <- c('Occ','MeanPropinquity','Median','SD','MinDist',
                             'MaxDist','Skewness','Mode','ModeSkewness')
    infoTable$Occ <- nrow(coord.table)
    infoTable$MeanPropinquity <- meanDist 
    infoTable$Median <- medianDist
    infoTable$SD <- sdDist
    infoTable$MinDist <- minDist
    infoTable$MaxDist <- maxDist
    infoTable$Skewness <- skewnessDist[1]
    infoTable$Mode <- modeDist$M
    infoTable$ModeSkewness <- modeSkewness
    result <- list('meanPropinquity' = meanDist,
                   'medianDist' = medianDist,
                   'sdDist' = sdDist,
                   'minDist' = minDist,
                   'maxDist' = maxDist,
                   'skewnessDist' = skewnessDist[1],
                   'mode' = modeDist$M,
                   'modeSkewness' = modeSkewness,
                   'minimalSpaningTree' = mst)
    readAndWrite(action = 'write', frmt = wrt.frmt, path = save.info.in,
                 name = 'InfoPropinquity_Whole', object = infoTable)
    if (plot.dist == TRUE) {
      jpeg(paste(save.plot.in, 'plotDistWhole.jpeg', sep = ''), res = 300,
           width = 25 ,height = 15 ,units = 'cm')
      plotDist <- density(mst$dist)
      plot(plotDist, main = 'Distance between Occurrences',
           xlab = 'Distance (decimal degrees)', type = 'p',cex = 0.5)
      polygon(plotDist, border = 'grey30')
      abline(v = meanDist, col = 'red')
      abline(v = medianDist, col = 'yellow4')
      abline(v = modeDist$M, col = 'blue1')
      dev.off()
      }
    if (plot.onMap == TRUE) {
      coordinates(coord.table) <- coord.table[, c('decimalLongitude', 
                                                  'decimalLatitude')]
      data('wrld_simpl')
      jpeg(paste(save.plot.in, 'plotOnMapWhole.jpeg', sep = ''), res = 300,
           width = 25 ,height = 15 ,units = 'cm')
      plot(wrld_simpl,main = 'Distribution Map')
      plot(coord.table,add = T, pch = '*', col = 'red' )
      dev.off()
    }
    return(infoTable)
    
    
    
  } else {
  #by species
    spNames <- as.character(unique(coord.table$species))
   
    infoTable <- as.data.frame(matrix(data = NA,nrow = length(spNames),ncol = 10))
    colnames(infoTable) <- c('Sp','Occ','MeanPropinquity','Median','SD','MinDist',
                             'MaxDist','Skewness','Mode','ModeSkewness')
    for (i in 1:length(spNames)) {
     
      spOccurrences <- coord.table[which(coord.table$species == spNames[i]),]
      uniqueAll <- spOccurrences[!duplicated(spOccurrences[, c('decimalLongitude', 'decimalLatitude')]), ]
      coordinates(uniqueAll) <- uniqueAll[, c('decimalLongitude', 'decimalLatitude')]
      distanPoints <- dist(coordinates(uniqueAll))
      mst <- spantree(d = distanPoints)
      meanDist <- mean(mst$dist,na.rm = T)
      medianDist <- median(mst$dist,na.rm = T)
      sdDist <- sd(mst$dist)
      minDist <- mst$dist[which.min(mst$dist)]
      maxDist <- mst$dist[which.max(mst$dist)]
      skewnessDist <- skewness(x = mst$dist, method = 'bickel', na.rm = T)
      modeDist <- mlv(mst$dist,method = 'mfv', na.rm = T)
      modeSkewness <- modeDist$skewness
     
      infoTable$Sp[i] <- as.character(spNames[i])
      infoTable$Occ[i] <- nrow(spOccurrences)
      infoTable$MeanPropinquity[i] <- meanDist 
      infoTable$Median[i] <- medianDist
      infoTable$SD[i] <- sdDist
      infoTable$MinDist[i] <- minDist
      infoTable$MaxDist[i] <- maxDist
      infoTable$Skewness[i] <- skewnessDist[1]
      infoTable$Mode[i] <- modeDist$M
      infoTable$ModeSkewness[i] <- modeSkewness
    
    if (plot.dist == TRUE) {
      
      jpeg(paste(save.plot.in,'plotDist_',spNames[i],'.jpeg',sep = ''), res = 300,
           width = 25 ,height = 15 ,units = 'cm')
      plotDist <- density(mst$dist)
      plot(plotDist, main = paste('Distance between Occurrences:',spNames[i],sep = ' '),
           xlab = 'Distance (decimal degrees)', type = 'p',cex = 0.5)
      polygon(plotDist, border = 'grey30')
      abline(v = meanDist, col = 'red')
      abline(v = medianDist, col = 'yellow4')
      abline(v = modeDist$M, col = 'blue1')
      dev.off()
    }
    if (plot.onMap == TRUE) {
      coordinates(spOccurrences) <- spOccurrences[, c('decimalLongitude', 
                                                  'decimalLatitude')]
      data('wrld_simpl')
      jpeg(paste(save.plot.in, 'plotOnMap_',spNames[i],'.jpeg', sep = ''), 
           res = 300, width = 25 ,height = 15 ,units = 'cm')
      plot(wrld_simpl,main = paste('Distribution Map',spNames[i],sep = ' '))
      plot(spOccurrences,add = T, pch = '*', col = 'red' )
      dev.off()
      
    }
      cat(paste('Saving information:',as.character(spNames[i], sep= ' '),'\n',sep=''))
      }
    readAndWrite(action = 'write', frmt = wrt.frmt, path = save.info.in,
                 name = 'InfoPropinquity_species', object = infoTable)
    print(paste('See: Information table in: ',save.info.in,'InfoPropinquity',
                sep = ''))
    cat('DENSITY PLOT LEGEND :::: \n 
          RED: Mean Propinquity \n 
          YELLOW: Median  Propinquity \n 
          BLUE: Mode  Propinquity')
    return(infoTable)
  }
}
