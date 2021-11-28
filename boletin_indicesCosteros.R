indicesCosteros <- function(TSM,ATSM,TSM34,ATSM34, salidaRDS, imprimir){


load('E:/programasR/boletin/climatologia_1982-2018.RDat')# -> climatologiaTSM(x,y,z)
  
################### INDICE DE ATSM COSTERO ###################
  T <- as.numeric(levels(factor(ATSM$data$tiempo)))
  
  ATSM_por_tiempo <- split(ATSM$data,
                   cut(ATSM$data$tiempo,breaks = as.numeric(levels(factor(ATSM$data$tiempo)))))
  
  TSM_por_tiempo <- split(TSM$data,
                          cut(TSM$data$tiempo,breaks = as.numeric(levels(factor(TSM$data$tiempo)))))
  
  nSeccionesATSM <- length(ATSM_por_tiempo)
  nSeccionesTSM <- length(TSM_por_tiempo)
  
  ATSM_por_tiempo_34 <- split(ATSM34$data,
                           cut(ATSM34$data$tiempo,breaks = as.numeric(levels(factor(ATSM34$data$tiempo)))))
  
  TSM_por_tiempo_34 <- split(TSM34$data,
                          cut(TSM34$data$tiempo,breaks = as.numeric(levels(factor(TSM34$data$tiempo)))))
  
  
  promediosNorte <- mapply(function(x,y){
    zonaNorte <- c(-8.24,-3)
    indc <- which( x$lat >= zonaNorte[1] & x$lat<=zonaNorte[2]  )
    indc2 <- which( climatologiaSST$y >= zonaNorte[1] & climatologiaSST$y<=zonaNorte[2] &
                    climatologiaSST$x >= 270 & climatologiaSST$x<=289.875)
    desvStd <- sd(climatologiaSST$z[indc2], na.rm=TRUE)
    mean(x$atsm[indc]/desvStd, na.rm=TRUE)
  },x= ATSM_por_tiempo,y=TSM_por_tiempo)
  
  promediosCentro <- mapply(function(x,y){
    zonaCentro <- c(-15.4,-8.24)
    indc <- which( x$lat >= zonaCentro[1] & x$lat<=zonaCentro[2]  )
    indc2 <- which( climatologiaSST$y >= zonaCentro[1] & climatologiaSST$y<=zonaCentro[2] &
                    climatologiaSST$x >= 270 & climatologiaSST$x<=289.875  )
    desvStd <- sd(climatologiaSST$z[indc2], na.rm=TRUE)
    mean(x$atsm[indc]/desvStd, na.rm=TRUE)
  },x= ATSM_por_tiempo,y=TSM_por_tiempo)
  
  promediosSur <- mapply(function(x,y){
    zonaSur <- c(-18.35,-15.4)  
    indc <- which( x$lat >= zonaSur[1] & x$lat<=zonaSur[2]  )
    indc2 <- which( climatologiaSST$y >= zonaSur[1] & climatologiaSST$y <=zonaSur[2]  &
                    climatologiaSST$x >= 270 & climatologiaSST$x<=289.875 )
    desvStd <- sd(climatologiaSST$z[indc2], na.rm=TRUE)
    mean(x$atsm[indc]/desvStd,na.rm=TRUE)
  },x= ATSM_por_tiempo,y=TSM_por_tiempo)
  
  promedio34 <- mapply(function(x,y){
    zona34 <- c(-5,5)
    indc <- which( x$lat >= zona34[1] & x$lat<=zona34[2]  )
    indc2 <- which( climatologiaSST$y >= zona34[1] & climatologiaSST$y <=zona34[2] &
                      climatologiaSST$x >= 185 & climatologiaSST$x<=210 )    
    desvStd <- sd(climatologiaSST$z[indc2], na.rm=TRUE)
    mean(x$atsm[indc],na.rm=TRUE)/desvStd
  },x= ATSM_por_tiempo_34,y=TSM_por_tiempo_34)
  
  promedioA <- mapply(function(x,y){
    zona34 <- c(-5,5)
    indc <- which( x$lat >= zona34[1] & x$lat<=zona34[2]  )
    indc2 <- which( climatologiaSST$y >= zona34[1] & climatologiaSST$y <=zona34[2] &
                      climatologiaSST$x >= 185 & climatologiaSST$x<=210 )
    mean(x$atsm[indc], na.rm=TRUE)
  },x= ATSM_por_tiempo_34,y=TSM_por_tiempo_34)
   
  promedio <- data.frame(T = T[-1], valor=promedioA )
  
  saveRDS( object = promedio, file = salidaRDS  )
  
  #############################################################
  
 # promedios <- do.call(what = rbind,args = promediosNorte)
  T <-T[-1]
  T <- c( T, T, T, T)
  z <- c( promediosNorte, promediosCentro, promediosSur, promedio34 )
 zona <- factor(c( rep('norte', length(promediosNorte)),
                   rep('centro', length(promediosCentro)),
                   rep('sur', length(promediosSur)),
                   rep('zona3.4', length(promedio34))),
                levels=c('norte','centro','sur','zona3.4'))
  
  promedios <- data.frame(T=T,indice = z, zona = zona )
  # promedios <- data.frame(T=T,indice = promediosCentro )
  # promedios <- data.frame(T=T,indice = promediosNorte )
  # 
  rownames(promedios) <- NULL
  
  promedios$T <- as.POSIXct(promedios$T,origin='1981-01-01')
  if(imprimir){
  png(filename = paste0(boletin.dir,'indices_atsm_50millas_',fecha.actual,'.png'),
      width=1600,height=850)
  pp <- ggplot(data=promedios,aes(x=T,y=indice, group=zona))
  pp <- pp + geom_line(aes(col=zona),size=2,)
  pp <- pp + geom_hline(yintercept=0, linetype="dotted", 
                        color = "black", size=2)
  pp <- pp + scale_x_datetime(date_breaks = 'month',date_labels = '%b-%Y' )
  pp <- pp + scale_y_continuous(limits = c(-7,7), breaks =  seq(-5,7,by=2))
  pp <- pp + labs(x='Fecha',y='Z',
                  title='TSM estandarizada dentro de las 50 millas',
                  caption='Fuente de datos: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)\nClimatología: 1981-2009')
  pp <- pp + theme(axis.text.x = element_text( size = 24,color='black',angle=90, hjust = 0.5 ),
                   axis.text.y = element_text( size = 34,color='black' ),
                   axis.title.x = element_text( size = 40 ),
                   axis.title.y = element_text( size = 40 ),
                   title = element_text( size = 36 ),
                   plot.subtitle = element_text(size = 28),
                   plot.caption = element_text( size = 28,hjust = 0),
                   legend.text = element_text(size=18),
                   legend.key.size = unit(2, "cm"))
  
  plot(pp)
  dev.off()
  
  save(file=paste0(boletin.dir,'indice01.RData'),list = 'promedios')}
  # ########################################################################
  # 
  # 
  # promedios <- data.frame(T=T[-1],indice = promediosCentro )
  # rownames(promedios) <- NULL
  # promedios$T <- as.POSIXct(promedios$T,origin='1981-01-01')
  # 
  # png(filename = paste0(boletin.dir,'indice_Centro_atsm_50millas_',fecha.actual,'.png'),
  #     width=1600,height=850)
  # pp <- ggplot(data=promedios,aes(x=T,y=indice))
  # pp <- pp + geom_line(col='blue',size=2)
  # pp <- pp + scale_x_datetime(date_breaks = 'month',date_labels = '%b-%Y' )
  # pp <- pp + scale_y_continuous(limits = c(-3,3) )
  # pp <- pp + labs(x='Fecha',y='?ndice',
  #                 title='?ndice costero de la TSM dentro de las 50 millas',
  #                 subtitle = 'Zona Centro (Salaverry a San Juan de Marcona)',
  #                 caption='Fuente de datos: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)\nClimatolog?a: 1981-2009')
  # pp <- pp + theme(axis.text.x = element_text( size = 24,color='black',angle=90 ),
  #                  axis.text.y = element_text( size = 34,color='black' ),
  #                  axis.title.x = element_text( size = 40 ),
  #                  axis.title.y = element_text( size = 40 ),
  #                  title = element_text( size = 36 ),
  #                  plot.subtitle = element_text(size = 28),
  #                  plot.caption = element_text( size = 28,hjust = 0))
  # 
  # plot(pp)
  # dev.off()
  # 
  # save(file=paste0(boletin.dir,'indice01.RData'),list = 'promedios')
  # ##############################################
  # 
  # promedios <- data.frame(T=T[-1],indice = promediosSur )
  # rownames(promedios) <- NULL
  # promedios$T <- as.POSIXct(promedios$T,origin='1981-01-01')
  # 
  # png(filename = paste0(boletin.dir,'indice_Sur_atsm_50millas_',fecha.actual,'.png'),
  #     width=1600,height=850)
  # pp <- ggplot(data=promedios,aes(x=T,y=indice))
  # pp <- pp + geom_line(col='blue',size=2)
  # pp <- pp + scale_x_datetime(date_breaks = 'month',date_labels = '%b-%Y' )
  # pp <- pp + scale_y_continuous(limits = c(-3,3) )
  # pp <- pp + labs(x='Fecha',y='?ndice',
  #                 title='?ndice Costero de la TSM dentro de las 50 millas',
  #                 subtitle = 'Zona Sur (San Juan de Marcona a Santa Rosa)',
  #                 caption='Fuente de datos: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)\nClimatolog?a: 1981-2009')
  # pp <- pp + theme(axis.text.x = element_text( size = 24,color='black',angle=90 ),
  #                  axis.text.y = element_text( size = 34,color='black' ),
  #                  axis.title.x = element_text( size = 40 ),
  #                  axis.title.y = element_text( size = 40 ),
  #                  title = element_text( size = 36 ),
  #                  plot.subtitle = element_text(size = 28),
  #                  plot.caption = element_text( size = 28,hjust = 0))
  # 
  # plot(pp)
  # dev.off()
  # 
  # save(file=paste0(boletin.dir,'indice01.RData'),list = 'promedios')
  # 
  # 
  
}