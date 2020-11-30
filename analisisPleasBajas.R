cat("\014")
rm(list=ls())
graphics.off()
rm(list=ls())
library('xlsx')
library('reshape','ggplot2')

#########################################

raiz    <- 'D:/analisisMarea/PISCO 18-19'

lista.archs <- list.files(path=raiz,recursive = TRUE,full.names = TRUE,pattern = '*.xls')
lista.archs <- as.list(lista.archs)

##########################

leer <- TRUE
if (leer){
  datos <- lapply(lista.archs,function(x){
  tabla <- read.xlsx(file = x,header = FALSE,sheetIndex = 1,as.data.frame = TRUE)
  
  indcNa <- which(is.na(tabla[1,]))
  
  if (length(indcNa)>0){
    tabla <- tabla[,-indcNa]
  }
  
  names(tabla) <- replicate(expr = c(1,2,3),3)
  
  t1 <- tabla[,1:3]
  t2 <- tabla[,4:6]
  t3 <- tabla[,7:9]
  
  as.Date(t3$`1`[1],format='%d/%m/%Y')
  
  tabla2 <- rbind(tabla[,1:3],
                  tabla[,4:6],
                  tabla[,7:9])
    hora <- as.numeric(format(tabla2[,2],'%H'))
  minuto <- as.numeric(format(tabla2[,2],'%M'))
  tiempo <- as.POSIXct(tabla2[,1],origin='1900-01-01')+hora*3600+minuto*60
  data.frame(T = tiempo,H=tabla2[,3])
  
})
         datos2 <- do.call(what = rbind,args = datos)
  names(datos2) <- c('tiempo','nivel')
  save(file = paste0(raiz,'/datos.RData'),list = c('datos','datos2'))
}else{
  load(paste0(raiz,'/datos.RData'))
}

###############

datosOrdenados <- datos2[order(datos2$tiempo),]

indcNA <- which(is.na(datos2$tiempo))

if(length(indcNA)>0){
datos2 <- datos2[-indcNA,]
}

indcNA <- which(is.na(datos2$nivel))

if(length(indcNA)>0){
  datos2 <- datos2[-indcNA,]}
rm('indcNA')
##################
datos2 <- datos2[order(datos2$tiempo),]

indc <- which(datos2$tiempo<'2018-01-01')
if(length(indc)>0){
datos2 <- datos2[-indc,]}
####################
# indc <- seq(1,90000)
# windows(width=1200,height=750)
# plot(datos2$tiempo[indc],datos2$nivel[indc])
# 
# rm('indc')
# 
#########################

datos3 <- split(datos2,cut.POSIXt(x = datos2$tiempo,breaks = 'day'))

##################################

windows(width=1200,height=700)
plot(datos3[[1]]$tiempo,datos3[[1]]$nivel)
###########
nDias <- length(datos3)
pleasYbajaMares <- vector(mode='list',length=length(datos3))

for(rr in 1:nDias){

tiempo <- as.numeric(datos3[[rr]]$tiempo)
if (length(tiempo)>0){
  
tabla <- data.frame(T=tiempo,H=datos3[[rr]]$nivel)

modelo <- mgcv::gam(H~s(T,k=15),data=tabla)

segundaD <- diff(modelo$fitted.values,differences = 2)

concavidadMenos <- which(segundaD < 0)
   separarMenos <- which(diff(concavidadMenos)!=1)
  concavidadMas <- which(segundaD > 0)
     separarMas <- which(diff(concavidadMas)!=1)

# windows(width=1200,height=650)
# par(mfrow=c(2,1))
# plot(tiempo[concavidadMenos],
#      modelo$fitted.values[concavidadMenos])
# plot(tiempo[concavidadMas],
#      modelo$fitted.values[concavidadMas])


cuantosPuntosMenos <- length(separarMenos )
  cuantosPuntosMas <- length(separarMas )

  if (cuantosPuntosMenos>0){
listaIndice <- vector(mode = 'list',length = cuantosPuntosMenos+1)
  
for(ii in 1:(cuantosPuntosMenos+1)){
  if (ii==1){
      listaIndice[[ii]] <- concavidadMenos[1:separarMenos[1]]
  }else if(ii<(cuantosPuntosMenos+1)){
    listaIndice[[ii]] <- concavidadMenos[(1+separarMenos[ii-1]):(separarMenos[ii])]
  }else{
    listaIndice[[ii]] <- concavidadMenos[(1+separarMenos[ii-1]):length(concavidadMenos)]
  }
}

nNiveles <- cuantosPuntosMenos+1
# windows(width=1200,height=650)
# par(mfrow=c(nNiveles,1))
for (ii in 1:nNiveles){
  maximo <- which.max( modelo$fitted.values[listaIndice[[ii]]] )
  if ( maximo != 1 & maximo != length(listaIndice[[ii]])){
  Tmax <- tiempo[listaIndice[[ii]][maximo]]
  Hmax <- modelo$fitted.values[listaIndice[[ii]]][maximo]
  if (!exists('X')){
    X <- data.frame(T=Tmax,H=Hmax)
  }else{
    X <- rbind(X,data.frame(T=Tmax,H=Hmax))
  }
  }
  
# plot(
#      tiempo[listaIndice[[ii]]],
#      modelo$fitted.values[listaIndice[[ii]]])
#      points(Tmax,Hmax,col='red')
     }
  }


  if (cuantosPuntosMas>0){
listaIndice <- vector(mode = 'list',length = cuantosPuntosMas+1)

for(ii in 1:(cuantosPuntosMas+1)){
  if (ii==1){
    listaIndice[[ii]] <- concavidadMas[1:separarMas[1]]
  }else if(ii<(cuantosPuntosMas+1)){
    listaIndice[[ii]] <- concavidadMas[(1+separarMas[ii-1]):separarMas[ii]]
  }else{
    listaIndice[[ii]] <- concavidadMas[(1+separarMas[ii-1]):length(concavidadMas)]
  }
}


# windows(width=1200,height=650)
# par(mfrow=c(cuantosPuntosMas+1,1))
for (ii in 1:(cuantosPuntosMas+1)){
  minimo <- which.min( modelo$fitted.values[listaIndice[[ii]]] )
  if ( minimo != 1 & minimo != length(listaIndice[[ii]])){
  Tmin <- tiempo[listaIndice[[ii]][minimo]]
  Hmin <- modelo$fitted.values[listaIndice[[ii]]][minimo]
  if (!exists('X')){
    Y <- data.frame(T=Tmin,H=Hmin)
  }else{
    Y <- rbind(X,data.frame(T=Tmin,H=Hmin))
  }
  
  # plot(tiempo[listaIndice[[ii]]],
  #      modelo$fitted.values[listaIndice[[ii]]])
  #  points(Tmin,Hmin,col='red')
  }
}

}


pleasYbajaMares[[rr]] <- rbind(X,Y)}

}


pleasYbajaMares <- do.call(what = rbind,args = pleasYbajaMares)

############################



pleasYbajaMares <- pleasYbajaMares[order(pleasYbajaMares$T),]



pleasYbajaMares$T <- as.POSIXct(pleasYbajaMares$T,origin='1970-01-01')

repetidos <- which(duplicated(pleasYbajaMares$T))

pleasYbajaMares <- pleasYbajaMares[-repetidos,]

write.table(x=pleasYbajaMares,file=paste0(raiz,'/pleasYbajamares.csv'),row.names = FALSE)

#################
windows(width=1200,height=700)
plot(pleasYbajaMares$T,pleasYbajaMares$H,type = 'o')