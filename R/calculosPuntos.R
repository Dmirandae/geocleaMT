library(dplyr)
library(rgeos)
library(sp)

datosParamo <- (read.csv("evidenciaTotal.dat",sep=" ",skip=1,header=F))

#head(datosParamo)

#tail(datosParamo)

datosParamo$V1 <- as.factor(datosParamo$V1)

#resumen <- datosParamo %>%   group_by(V1) %>% summarise(conteoOcurrencias = length(V1))

#print(resumen)


listadoEspecies  <- levels(datosParamo$V1)

                                        #
#datos  <- datosParamo[datosParamo$V1 == listadoEspecies[2],]
#datos  <- datos[,-1]
#datosSp  <- datos

#length(datos)

cosasSp  <-  function(sp, base=datosParamo){

    datosSp  <- base[base$V1 == sp,-1]

    meanX  <- mean(datosSp[,1])

    meanY  <- mean(datosSp[,2])

    numPuntos  <- length(datosSp[,1])

    if (numPuntos > 3){
    p <- Polygon(datosSp)
    ps  <-  Polygons(list(p),1)
    sps  <-  SpatialPolygons(list(ps))

    realCentroid  <-     gCentroid  (sps)

    x  <-  realCentroid$x
    y  <-  realCentroid$y

}else{x  <-  y  <-  NA}

   cat(sp,"=\t",mean(dist(datosSp)),numPuntos,"**",x,y,"*",meanX,meanY,"\n")


#    cat(sp,"\t",mean(dist(datosSp)),numPuntos,"**",meanX,meanY,"--","\n")

 #   print(realCentroid)

    ##return(datosSp)
}



#listadoEspecies


listadoSpA  <- lapply(listadoEspecies,cosasSp,base=datosParamo)

#cat(listadoSpA)

#summary(resumen$conteoOcurrencias)

#qplot(resumen$conteoOcurrencias)
