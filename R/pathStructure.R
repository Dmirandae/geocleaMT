#'@name pathStructure
#'
#'@title  Folder structure
#'
#'@description Create the file structure for the protocol proposed 
#' R-Alarcon & Miranda-Esquivel (submitted) [1].
#'
#'@param path.dir Vector of characters. Path to the folders' structure for all data/results.
#'
#'@param group Vector of characters. Name or names of group; create a 
#'subdirectory for each group into each main directory. See details.
#'
#'@details A group is a high taxonomic level such as Amphibia, Reptilia, Mammalia, etc.
#'
#'
#'@return Created folders into the assigned path.
#'
#'@author R-Alarcon Viviana and Miranda-Esquivel Daniel R.
#'
#'
#'@note See:
#'[1] R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.
#'
#' @references 
#'[1] R-Alarcon V. and Miranda-Esquivel DR.(submitted) geocleaMT: An R package to
#'cleaning geographical data from electronic biodatabases.


pathStructure <- function(path.dir = NULL, 
                       group    = NULL){
 if (is.null(group)) {
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'readDbR/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'readDbBash/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'assignElevation/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'assignElevation/alt.unassig', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'assignElevation/alt.assig', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'elevFromGg/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'elevFromGg/alt.unassig', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'elevFromGg/alt.assig', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'cutRange/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'cutRange/inside', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'cutRange/outside', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'invasiveSp/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'invasiveSp/foreign', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'invasiveSp/non.foreign', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'gbifDownSp/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'splitGeoref/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'splitGeoref/georref', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'splitGeoref/not.georref', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'splitGeoref/min.Occurren', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'checkCoord/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'checkCoord/wrogn.coord', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'checkCoord/right.coord', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'pointAtSea/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'pointAtSea/at.sea', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'pointAtSea/on.earth', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_America/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_America/outside', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_America/inside', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_Choco/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_Choco/outside', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_Choco/inside', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'usefulSp/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'usefulSp/useful', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'usefulSp/useless', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'stackingSp/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'delPointOrSp/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'delPointOrSp/delete', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'delPointOrSp/undelete', sep = ''))
   #!dir.create(showWarnings = FALSE, path = paste(path.dir, 'plot/', sep = ''))
   #!dir.create(showWarnings = FALSE, path = paste(path.dir, 'martitracks/', sep = ''))
   #!dir.create(showWarnings = FALSE, path = paste(path.dir, 'ndm/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'tables/', sep = ''))
   dir.create(showWarnings = FALSE, path = paste(path.dir, 'meanPropinquity/', sep = ''))
   }else{
     for (i in group) {
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'readDbR/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'readDbR/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'readDbBash/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'readDbBash/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'assignElevation/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'assignElevation/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'assignElevation/', i, '/alt.unassig', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'assignElevation/', i, '/alt.assig',   sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'elevFromGg/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'elevFromGg/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'elevFromGg/', i, '/alt.unassig', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'elevFromGg/', i, '/alt.assig', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'cutRange/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'cutRange/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'cutRange/', i, '/inside', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'cutRange/', i, '/outside', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'invasiveSp/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'invasiveSp/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'invasiveSp/', i, '/foreign', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'invasiveSp/', i, '/non.foreign', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'gbifDownSp/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'gbifDownSp/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'splitGeoref/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'splitGeoref/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'splitGeoref/', i, '/georref',  sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'splitGeoref/', i, '/not.georref',  sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'splitGeoref/', i, '/min.Occurren', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'checkCoord/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'checkCoord/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'checkCoord/', i, '/wrogn.coord',  sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'checkCoord/', i, '/right.coord',  sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'pointAtSea/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'pointAtSea/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'pointAtSea/', i, '/at.sea',  sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'pointAtSea/', i, '/on.earth',  sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_America/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_America/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_America/', i, '/outside',   sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_America/', i, '/inside',  sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_Choco/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_Choco/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_Choco/', i, '/outside',  sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'spOutPoly_Choco/', i, '/inside',  sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'usefulSp/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'usefulSp/', i, sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'usefulSp/', i, '/useful', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'usefulSp/', i, '/useless', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'stackingSp/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'delPointOrSp/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'delPointOrSp/', i, sep = ''))
       #!dir.create(showWarnings = FALSE, path = paste(path.dir, 'plot/', sep = ''))
       #!dir.create(showWarnings = FALSE, path = paste(path.dir, 'martitracks/', sep = ''))
       #!dir.create(showWarnings = FALSE, path = paste(path.dir, 'ndm/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'tables/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'meanPropinquity/', sep = ''))
       dir.create(showWarnings = FALSE, path = paste(path.dir, 'meanPropinquity/', i, sep = ''))
    }
  }
}
