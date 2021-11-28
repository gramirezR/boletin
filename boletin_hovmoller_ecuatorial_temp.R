hovmoller_ecuatorial_temp <- function(lista.archivos,variable,region.lon,region.lat, suavizar){
  narchs <- length(lista.archivos)
  require('metR')
  #### DELIMITAR LA ZONA
  
  ncin <- open.nc(lista.archivos[1])
  
  lat <- var.get.nc(ncin,'lat')
  lon <- var.get.nc(ncin,'lon') 
  
  close.nc(ncin)
  
  indc.lat <- which((lat >= region.lat[1]) & (lat <= region.lat[2]) )
  indc.lon <- which((lon >= region.lon[1]) & (lon <= region.lon[2]) )
  
  #######
  
  lon <- lon[indc.lon]
  
  n.lon <- diff( range(indc.lon)  ) + 1
  
  n.lat <- diff( range(indc.lat)  ) + 1
  
  #ind_tie <- seq(from=1, to=narchs, by=7)
  
  n.tie <- length(lista.archivos)
  
  temp <- array( dim=c(n.tie,length(lon)) )
  
  tie <- array( dim=n.tie )
  
  for (ii in 1:n.tie){
    ncin <- open.nc(lista.archivos[ii])
    
    tie[ii] <- var.get.nc(ncin,'time')/86400
    
    M <- var.get.nc(ncin,variable)
    
    close.nc(ncin) 
    
    ys <- rowMeans(M[indc.lon,indc.lat],na.rm=TRUE)
    
    inds <- !is.na(ys)
    
    temp[ii,]  <- (0.01*ys)
    
    rm('ys')
  }
  
  ###### ARMADO DEL DATA.FRAME PARA GGPLOT#####
  
  malla <- expand.grid(T=tie,lon=lon)
  
  Z <- stack( as.data.frame(temp) )
  
  
  hovmoller <- data.frame(   T  = malla$T,
                             lon = malla$lon,
                             anm = Z$values)
  
  # suave <- loess(anm~lon+T,data=hovmoller,span=0.018)
  
  suave <- mgcv::gam(anm~ te(T,lon,k=c(20,20)),data = hovmoller)
  pred <- predict(suave,newdata = as.data.frame(malla))
  
  # Z <- stack(as.data.frame(pred))
  Z <- as.matrix( pred )
  hovmoller <- data.frame(   T = malla$T,
                             lon = malla$lon,
                             anm = Z)
  
  
  
  paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
  
  niveles <- niveles <- seq(-6, 6, by=0.5)
  
  indcT <- grep( '[0-9]*-[0-9]*-01', as.Date(tie,origin='1981-01-01')  )
  
  marcas_y <- tie[indcT]  
  
  #tie[indcT
  
  etiquetas_y <- months(as.Date(marcas_y, origin='1981-01-01'), abbreviate =TRUE)
  
  etiquetas <- seq(from=region.lon[1], to=region.lon[2], by=20)
  
  etktas <- unlist(lapply( -(etiquetas - 360) ,
                           function(x){ 
                             if (x > 180){
                               paste0(as.vector( as.character(360-x)),'E') 
                             }else if( x< 180) {
                               paste0( x ,'W')
                             }else{
                               as.character(x)
                             }
                           }) )
  

  pp <- ggplot(data=hovmoller,aes(x=T,y=lon,fill=anm)) 
  pp <- pp + geom_raster(aes(fill = anm),interpolate=FALSE,show.legend = TRUE  )
  pp <- pp + scale_fill_gradientn(colours = paleta_color,breaks = niveles,limits =range(niveles))
  
  pp <- pp + stat_contour(data=hovmoller, aes(x=T,y=lon,z=anm),
                          breaks = niveles,col='black' ,inherit.aes=FALSE )
 
  pp <- pp + geom_text_contour(data=hovmoller, aes(x=T,y=lon,z=anm),
                               stroke = 0.15,skip=0,min.size =10,size=14,rotate = FALSE,
                               breaks = niveles)
  
   # pp <- pp + geom_dl(data=hovmoller,aes(x=lon,y=T,z=anm,label=..level..),
   #                   breaks = niveles,col='black',
   #                   method=list('bottom.pieces', cex=2.0), stat="contour")
   # 
  pp <- pp + labs(       y = 'Longitud',
                         x = 'Fecha 2019-2020',
                         title = 'DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \nDpto. de Oceanografía - Div. Oceanografía',
                         subtitle = expression(paste( 'Anomalía de la Temperatura Superficial del Mar ',
                                                      2*degree~N~a~2*degree~S)) ,
                         caption = paste0('Fuente: COPERNICUS MARINE ENVIRONMENT\n             MONITORING SERVICE (CMEMSv3.0)',
                                          '\nClimatología: 1993-2012'))
  pp <- pp + theme_bw()
  pp <- pp + scale_x_continuous( breaks = marcas_y, 
                                  label = etiquetas_y,
                                 expand = c(0.01,0.01)
                                  )
  pp <- pp + scale_y_reverse(expand = c(0,0),
                             breaks = etiquetas,
                             labels =  etktas,
                             limits = rev(range(etiquetas)))
  
  pp <- pp + theme( axis.text.y = element_text( size = 34,color='black' ),
                    axis.text.x = element_text( size = 34,color='black',angle=90 ),
                    axis.title.y = element_text( size = 40 ),
                    axis.title.x = element_text( size = 40 ),
                    title = element_text( size = 32 ),
                    plot.caption = element_text( size = 28,hjust = 0))
  
  pp <- pp + guides( fill = guide_colorbar(  barheight = unit(25, "cm"),barwidth = unit(1.5,'cm'),
                                             label.theme = element_text(size=30),
                                             title = 'ATSM\n?C',
                                             title.theme = element_text(size=28)))
  
  return(pp)
}