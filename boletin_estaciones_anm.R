boletin_estaciones_anm <- function(lista,puntos,fecha.anterior,fecha.actual){

  narchs <- length(lista)
  
  ncin <- open.nc(lista[1])
  lon <- var.get.nc(ncin,'longitude')
  lat <- var.get.nc(ncin,'latitude')
  # print.nc(ncin)
  close.nc(ncin)
 distacia.balta.lon <- abs( lon - puntos$balta[1]  )
 distacia.balta.lat <- abs( lat - puntos$balta[2]  )
   
 indc.lon.Balta <- which.min(distacia.balta.lon)
 indc.lat.Balta <- which.min(distacia.balta.lat)
 
 distacia.talara.lon <- abs( lon - puntos$talara[1]  )
 distacia.talara.lat <- abs( lat - puntos$talara[2]  )
 
 indc.lon.talara <- which.min(distacia.talara.lon)
 indc.lat.talara <- which.min(distacia.talara.lat)
 
 distacia.chimbo.lon <- abs( lon - puntos$chimbote[1]  )
 distacia.chimbo.lat <- abs( lat - puntos$chimbote[2]  )
 
 indc.lon.chimbo <- which.min(distacia.chimbo.lon)
 indc.lat.chimbo <- which.min(distacia.chimbo.lat)
 
 
 
  nivel.Balta <- matrix(nrow=narchs,ncol=1)
 nivel.Talara <- nivel.Balta
 nivel.Chimbo <- nivel.Balta
       tiempo <- nivel.Balta
 
 for (ii in 1:narchs){
   ncin <- open.nc(lista[ii])
 nivel.Balta[ii,1] <- 0.01*var.get.nc(ncin,variable = 'sla'
                           ,start=c(indc.lon.Balta,indc.lat.Balta,1)
                           ,count=c(1,1,1))
 
 nivel.Talara[ii,1] <- 0.01*var.get.nc(ncin,variable = 'sla'
                                      ,start=c(indc.lon.talara,indc.lat.talara,1)
                                      ,count=c(1,1,1))
 
 nivel.Chimbo[ii,1] <- 0.01*var.get.nc(ncin,variable = 'sla'
                                      ,start=c(indc.lon.chimbo,indc.lat.chimbo,1)
                                      ,count=c(1,1,1))
 tiempo[ii,1] <- var.get.nc(ncin,variable = 'time')
 close.nc(ncin)
 }
 
       tiempo <- rbind(tiempo,tiempo,tiempo)
          anm <- rbind(nivel.Balta,nivel.Talara,nivel.Chimbo)
          est <- rbind( as.matrix(replicate(narchs,'Galapagos')   ,nrow=narchs,ncol=1),
                        as.matrix(replicate(narchs,'Talara')  ,nrow=narchs,ncol=1),
                        as.matrix(replicate(narchs,'Chimbote'),nrow=narchs,ncol=1))
 balta.lon <- lon[indc.lon.Balta]-360
 balta.lat <- lat[indc.lat.Balta]
 
  grafica <- data.frame(    T = as.POSIXct(tiempo*86400,origin='1950-01-01'), 
                          anm = anm,
                          EST = as.factor(est))   
  guardar <- list(grafica,balta.lon,balta.lat)
save(file = 'datosNivel.Rdata',guardar)   

 pp <- ggplot(data=grafica,aes(x=T,y=anm,colour=EST))
 pp <- pp + scale_x_datetime(breaks = 'month')
 pp <- pp + geom_path(size=1.5)
 pp <- pp + scale_color_manual(name='ESTACIÓN',values=c('Galapagos'='red','Talara'='blue','Chimbote'='green'))
 pp <- pp + theme_bw()
 pp <- pp + labs(x= 'Fecha',y='Anomalía nivel del mar (cm)',
                 title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                              'Dpto. de Oceanografía - Div. Oceanografía'),
                 subtitle=paste0('Nivel del mar'),
                 caption=paste0('Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)',
                                ' Climatología: 1993-2012'))
 pp <- pp + theme( axis.title.x = element_text( size=24,hjust=0.5  ),
                   axis.title.y = element_text( size=24,hjust=0.5  ),
                   axis.text = element_text(size=20),
                   title=element_text(size=26),
                   plot.subtitle=element_text(size=24),
                   plot.caption = element_text(size = 20,hjust = 0),
                   legend.text = element_text(size = 20,hjust = 0),
                   legend.title = element_text(size = 21,hjust = 0)
                   )
 
 return(pp)
 
 }