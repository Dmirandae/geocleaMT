library(dplyr)


datosParamo <- (read.csv("evidenciaTotal.dat",sep=" ",skip=1,header=F))

head(datosParamo)

tail(datosParamo)

datosParamo$V1 <- as.factor(datosParamo$V1)

resumen <- datosParamo %>%   group_by(V1) %>%
           summarise(conteoOcurrencias = length(V1))


listadoEspecies  <- levels(datosParamo$V1)

datos  <- datosParamo[datosParamo$V1 == listadoEspecies[1],]

datos[,-1]

cosasSp  <-  function(sp, base=datosParamo){

    datosSp  <- mean(dist(base[base$V1 == sp,-1]))

    cat(sp,"\t",datosSp,"\n")

    ##return(datosSp)
}


lapply(listadoEspecies,cosasSp,base=datosParamo)

summary(resumen$conteoOcurrencias)

#qplot(resumen$conteoOcurrencias)

