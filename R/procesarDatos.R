## procesoardatos

archivos  <-  dir(pattern = "csv")

datosSp  <-  read.csv(archivos[1], sep=";",header = T)

head(datosSp)

tail(datosSp)

names(datosSp)

cor(datosSp$XCen, datosSp$Xmean , use= "pairwise.complete.obs")

cor(datosSp$YCen, datosSp$Ymean , use= "pairwise.complete.obs")

newDatos  <-  datosSp[(!is.na(datosSp$YCen)),]

qplot(newDatos$YCen,newDatos$XCen)

qplot(newDatos$Ymean,newDatos$Xmean)

qplot(datosSp$Ymean,datosSp$Xmean)


                                        #dev.off()

maxMeanX  <-  max(datosSp$Xmean)
minMeanX  <-  min(datosSp$Xmean)

maxMeanY  <-  max(datosSp$Ymean)
minMeanY  <-  min(datosSp$Ymean)

maxMeanX
minMeanX

maxMeanY
minMeanY

qplot(datosSp$Xmean)

qplot(datosSp$Ymean)


extraerVecindarioXY  <-  function( xReferencia = 5.0,
                                  yReferencia = -70.0,
                                  radio = 1.0,
                                  datos = datosSp ){

reducedDatos  <-  datos[(datos$Xmean <= xReferencia + radio & datos$Xmean >= xReferencia - radio) &
                            (datos$Ymean <= yReferencia + radio & datos$Ymean >= yReferencia - radio),]

reducedDatos

return(reducedDatos)
}


extraerVecindarioXY(radio = 3)

#head(reducedDatosSp)
#tail(reducedDatosSp)
