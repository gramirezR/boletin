### PROCESAR DATOS DE VIENTO ASCAT #####
cat('\014') 
graphics.off()
rm(list=ls())

library('RNetCDF')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
library('ggquiver')
library('RCurl')
####

# carpeta <- 'C:/Users/gramirez/ANM/boletin/viento/2018/09/'
fecha.actual <- lubridate::today();

anio <- lubridate::year(fecha.actual) ;
mes <- formatC(lubridate::month(fecha.actual),digits=1,flag=0) ;
dia <- lubridate::day(fecha.actual) ;
mes.pal <- format(as.Date(paste(anio,mes,dia,sep='-'),format='%Y-%m-%d'),format='%B')

carpeta <- paste0('C:/Users/gramirez/ANM/boletin/viento/',anio,'/',mes,'/')
figuras <- paste0('C:/Users/gramirez/ANM/boletin/viento/',anio,'/figuras/')
if (!dir.exists(carpeta)){
  dir.create(carpeta,recursive = TRUE)
}


ftp.dir <- paste0('ftp://ftp.ssmi.com/windsat/bmaps_v07.0.1/y',anio,'/m',mes,'/')


 # ftp.dir <- 'ftp://ftp.ssmi.com/windsat/bmaps_v07.0.1/y2018/m11/'
 # escala <- c(0.15,0.2,0.2,0.3, 0.01,0.1,0.2,1.5)

 dim.var <- 8
 mag.ind <- 7
 dir.ind <- 8  
 escala <- c(0.15,0.2,0.2,0.3, 0.01,0.1,0.2,1.5)
 #    [sst,windLF,windMF,vapor,cloud,rain,windAW,wdir]
 #    sst  is surface water temperature at depth of about 1 mm in deg C
 #    windLF is 10 meter surface wind in m/s made using 10.7 GHz channel and above
 #    windMF is 10 meter surface wind in m/s made using 18.7 GHz channel and above
 #   vapor is atmospheric water vapor in millimeters
 #    cloud is liquid cloud water in millimeters
 #   rain  is rain rate in millimeters/hour
 #   windAW is 10 meter surface wind for all weather conditions made using 3 algorithms
 #   wdir is wind direction oceanographic convention, blowing North = 0 in degrees
 lista <- getURL(ftp.dir 
                      ,username='gramirez@dhn.mil.pe',
                       password ='gramirez@dhn.mil.pe'
                      ,dirlistonly = TRUE,verbose=TRUE)
      
  # lista <- unlist(str_extract_all(unlist(strsplit(lista,'\r\n')),'.+(v02.1_3day.gz)'))
 lista <- unlist(str_extract_all(unlist(strsplit(lista,'\r\n')),'.+(d3d.gz)'))
 
  lapply( lista,  
          function(x) {
             prefijo <- str_extract(x,pattern='.*[^.gz]')
                arch <- paste0(carpeta, prefijo )
            arch.sal <-  paste0(carpeta,prefijo)
            if (!file.exists(arch.sal)){
               f <-  CFILE(paste0(carpeta,x), mode="wb") 
            curlPerform( url = paste0(ftp.dir, x), 
                    username = 'gramirez@dhn.mil.pe',
                    password = 'gramirez@dhn.mil.pe',
                   writedata = f@ref  ) 
            close(f)
            R.utils::gunzip(paste0(carpeta,x),
                            destname=arch.sal,
                            skip  =FALSE,
                            remove=TRUE)}
            })
  
####### LEER PROMEDIOS #######
  
  limite.lon <- c(-90, -70)
  limite.lat <- c(-20, 2)
     dim.lat <- 720 
     dim.lon <- 1440

  xlat <- 0.25*seq(1:dim.lat)-90.125
  xlon <- 0.25*seq(1:dim.lon)-0.125
  
  narchs <- length(lista)
  
   indcx <- which(xlon>(limite.lon[1]+360) & xlon<(limite.lon[2]+360))
  
   indcy <- which(xlat>limite.lat[1]&xlat<limite.lat[2])
  
  matriz <- expand.grid(lon = xlon[indcx],
                        lat = xlat[indcy])
  ########################################
  
  # arch <- paste0(carpeta,str_extract(lista[narchs],pattern='.*[^.gz]'))
  # archId <- file(arch,'rb')
  # datos <- readBin(archId,what = 'int',size=1,
  #                  signed = FALSE,
  #                  endian = 'little',
  #                  n = dim.lon*dim.lat*dim.var)
  # close(archId)
  # 
  # 
  # datos <- array(datos,
  #                dim = c(dim.lon,dim.lat,dim.var))   
  # 
  # fields::image.plot(drop(datos[indcx,indcy,mag.ind]*escala[mag.ind]))
  # 
 ############ 
for (ii in narchs:narchs) {
noHePasado <- TRUE
      arch <- paste0(carpeta,str_extract(lista[ii],pattern='.*[^.gz]'))
    archId <- file(arch,'rb')
     datos <- readBin(archId,what = 'int',size=1,
                           signed = FALSE,
                           endian = 'little',
                                n = dim.lon*dim.lat*dim.var)
    close(archId)
    
         datos <- array(datos,
                        dim = c(dim.lon,dim.lat,dim.var))
    datos[,,mag.ind] <- datos[,,mag.ind]*escala[mag.ind]
    datos[,,dir.ind] <- datos[,,dir.ind]*escala[dir.ind]
    
     if (noHePasado){
       noHePasado <- !noHePasado
      magnitud <- stack(data.frame(datos[indcx,indcy,mag.ind]))
     direccion <- stack(data.frame(datos[indcx,indcy,dir.ind]))
          indc <- which(magnitud$values > 50 | magnitud$values < 0)
          if (length(indc)>0){
      magnitud$values[indc] <- NA
     direccion$values[indc] <- NA}
     }else {
              A <- stack(data.frame(datos[indcx,indcy,mag.ind]))
              B <- stack(data.frame(datos[indcx,indcy,dir.ind]))
           indc <- (A$values) > 50 | (magnitud$values < 0)
          indc[ is.na(indc) ] <- TRUE
        A$values[indc] <- NA
        B$values[indc] <- NA
        
        direccion$values[!indc] <- rowMeans(cbind(B$values[!indc],
                                                  direccion$values[!indc]),na.rm=TRUE)
         magnitud$values[!indc] <- rowMeans(cbind(A$values[!indc],
                                                  magnitud$values[!indc]),na.rm=TRUE)
     }
}

     direc2 <- direccion$values

     indc <- which(direccion$values>360)
     if (length(indc)>0){
     direccion$values[indc] <- NA}

     indc <- which(direc2>=0 &  direc2<=90  )
     direccion$values[indc] <-  90 - direccion$values[indc]

     indc <- which(direc2>90 & direc2<=180)
     direccion$values[indc] <- 270 + direccion$values[indc]

     indc <- which(direc2>180 & direc2<=270)
     direccion$values[indc] <- 450 - direccion$values[indc]

     indc <- which(direc2>270 & direc2<=360)
     direccion$values[indc] <- 450 - direccion$values[indc]
     rm(direc2)

      velocidad <- data.frame(mag=magnitud$values,
                              dir=direccion$values,
                              lon=matriz$lon,
                              lat=matriz$lat   )
      
      indc <- is.na(velocidad$mag)
      
      # indc <- velocidad$mag > 50
      # 
      # velocidad$mag[indc] <- NA
      # 
       suave <- mgcv::gam(data=velocidad,mag~te(lon,lat,k=c(16,16)))
       Z <- as.matrix(predict(suave,newdata = as.data.frame(matriz)))
       Z[indc] <- NA
      velocidad$mag <- Z
      rm('Z')
###=======================================================      
      suave <- mgcv::gam(data=velocidad,dir~te(lon,lat,k=c(16,16)))
      Z <- as.matrix(predict(suave,newdata = as.data.frame(matriz)))
      Z[indc] <- NA
      velocidad$dir <- Z
      rm('Z')
      # 
      # velocidad$mag[indc] <- NA
  
      u <- velocidad$mag*cos(pi*velocidad$dir/180)
      
      v <- velocidad$mag*sin(pi*velocidad$dir/180)
      
      indcU <- seq(from=1,to=length(u),length.out = 550)
      
      campo <- data.frame(lon = velocidad$lon[indcU],
                          lat = velocidad$lat[indcU],
                            u = u[indcU],
                            v = v[indcU])     
      
     marcas.x <- seq(from=limite.lon[1],to=limite.lon[2],by=5)   
  etiquetas.x <- unlist(lapply( as.list(marcas.x),function(x)
                                              {if ( x > -180 )
                                                 { paste0(-x,'W')}
                                                else if(x < -180)
                                                  { paste0(-x,'E') } else
                                                    { 180 }} ))
  
     marcas.y <- seq(from=limite.lat[1],to=limite.lat[2],by=2)
  etiquetas.y <- unlist( lapply( as.list(marcas.y),function(x)
                                                    { if (x>0){
                                                      paste0(x,'N')
                                                    }else if(x<0){
                                                      paste0(-x,'S')
                                                    }else { '0' }  })  )

  ###### LINEA DE COSTA-----------------
  load('costa_Peru_202.RDat') 
  
  
###############
 if (exists('pp')){
  rm(pp)
 }
  niveles <- seq(from=0,to=12,by=2)
  
  paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
  
 png(file=paste0(figuras,'mapa_viento_', mes.pal,  '02.png'),width=1000,height=1200)
 pp <- ggplot(data=velocidad,aes(x=lon,y=lat,fill=mag))
 pp <- pp + geom_raster(interpolate=TRUE)
 pp <- pp + scale_fill_gradientn(colours = paleta_color,
                                  breaks = niveles,
                                  labels = as.character(niveles)
                                 )
 pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),color = 'black', fill = 'grey80',inherit.aes=FALSE  )
 
 for (kk in 1:length(fronteras)){
   f <- as.data.frame(fronteras[[kk]])
   pp <- pp + geom_point(data=f,aes( x=X1,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
 }
 
  pp <- pp + geom_quiver(data=campo,
                         aes(x=lon,y=lat,u=u,v=v),
                         vecsize = 5,
                            size = 0.7,
                         inherit.aes=FALSE)
  pp <- pp + scale_x_continuous(limits=360+range(limite.lon),
                                expand=c(0,0),
                                breaks = 360+marcas.x,
                                labels = etiquetas.x
                                )
  pp <- pp + scale_y_continuous(limits=range(limite.lat),
                                expand=c(0,0),
                                breaks=marcas.y,
                                labels=etiquetas.y)
  pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('Campo de viento, \n ', dia ,' de ',mes.pal,' del ',anio ),
                caption='Fuente: WINDSAT, Remote Sensing Systems, NASA, www.remss.com.
Elaboración: Dirección de Hidrografía y Navegación,
Departamento de Oceanografía,
División de Oceanografía Física')
  pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                    axis.title.y = element_text( size=28,hjust=0.5  ),
                       axis.text = element_text(size=20),
                    title=element_text(size=32),
                    plot.caption = element_text(size = 18,hjust = 0))
  pp <- pp + guides( fill = guide_colorbar(  barheight = unit(20, "cm"),
                                              barwidth = unit(2,'cm'),
                                           label.theme = element_text(size=24),
                                                 title = 'Magnitud\nm/s',
                                           title.theme = element_text(size=24)
                                           ))

 plot(pp)
dev.off() 
 
 
 
