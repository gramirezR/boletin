interpolar_componente <- function(componente,malla){
  library('sp')
   names(malla) <- c('lon','lat')
     lapply(componente,function(x){ 
              y <- as.data.frame(stack(as.data.frame(x))$values)
       names(y) <- 'comp'
       indcNa <- is.na(y$comp)
               dd <- data.frame(lon=malla$lon[-indcNa],lat=malla$lat[-indcNa],comp=y$comp[-indcNa])
              # 
               modelo <- mgcv::gam(data=dd,comp~te(x=lon,y=lat,k=c(10,10)))
              # 
               interp <- data.frame(lon=malla$lon,lat=malla$lat,
                            valor=stack(predict(modelo,newdata=malla))$values)
  })}