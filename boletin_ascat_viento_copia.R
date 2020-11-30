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

carpeta <- 'C:/Users/gramirez/ANM/boletin/viento/2018/09/'

if (!dir.exists(carpeta)){
  dir.create(carpeta,recursive = TRUE)
}

ftp.dir <- 'ftp://ftp.ssmi.com/ascat/metopa/bmaps_v02.1/y2018/m09/'

 lista <- getURL('ftp://ftp.ssmi.com/ascat/metopa/bmaps_v02.1/y2018/m09/'
                      ,username='gramirez@dhn.mil.pe',
                       password ='gramirez@dhn.mil.pe'
                      ,dirlistonly = TRUE,verbose=TRUE)
      
  lista <- unlist(str_extract_all(unlist(strsplit(lista,'\r\n')),'.+(v02.1.gz)'))

  lapply( lista,  
          function(x) {
            arch <- paste0(carpeta,x)
            arch.sal = paste0(carpeta,str_extract(x,pattern='.*[^.gz]'))
            if (!file.exists(x)){
               f <-  CFILE(arch, mode="wb") 
            curlPerform( url = paste0(ftp.dir, x), 
                    username = 'gramirez@dhn.mil.pe',
                    password = 'gramirez@dhn.mil.pe',
                   writedata = f@ref  ) 
            close(f)
            R.utils::gunzip(arch,
                            destname=arch.sal,
                            skip=TRUE)}
            })
  

limite.lon = c(-90, -70)
limite.lat = c(-20, 2)
  dim.lat <- 720 
 dim.lon  <- 1440
  dim.var <- 4

  xlat <- 0.25*seq(1:dim.lat)-90.125
  xlon <- 0.25*seq(1:dim.lon)-0.125
  
  escala <- c(.2,1.5,1.0,0.02)
####### LEER PROMEDIOS #######
  narchs <- length(lista)
  
  #####
  
  indcx <- which(xlon>limite.lon[1]+360 & xlon<limite.lon[2]+360)
  
  indcy <- which(xlat<2&xlat>-20)
  
  indcx.v <- indcx[seq(from=1,to=length(indcx),by=5)]
  indcy.v <- indcy[seq(from=1,to=length(indcy),by=5)]
  
  matriz <- expand.grid(lon = xlon[indcx],
                        lat = xlat[indcy])
  
  matriz.v <- expand.grid(lon = xlon[indcx.v],
                          lat = xlat[indcy.v])
 ####### 
 for (ii in 1:narchs) {
    
    arch <- paste0(carpeta,str_extract(lista[ii],pattern='.*[^.gz]'))
    archId <- file(arch,'rb')
    datos <- readBin(archId,what='int',size=1,
                     signed=FALSE,endian='little',n=dim.lon*dim.lat*dim.var)
    close(archId)
    
     datos <- array(datos,dim = c(dim.lon,dim.lat,dim.var))
    break
    datos[,,1] <- datos[,,1]*escala[1]
    datos[,,2] <- datos[,,2]*escala[2]
    
    
    indc <- datos[,,1]>=50
    
    datos[,,2][indc] <- NA
    datos[,,1][indc] <- NA
    # magnitud <- stack(data.frame(unname(datos[indcx,indcy,1])))
    M <-  stack(data.frame(unname(datos[indcx,indcy,1])))
    
    # magnitud.v <- stack(data.frame(unname(datos[indcx.v,indcy.v,1])))
    M.v <- stack(data.frame(unname(datos[indcx.v,indcy.v,1])))
    
    M$lon <- matriz$lon
    M$lat <- matriz$lat
    
    M.v$lon <- matriz.v$lon
    M.v$lat <- matriz.v$lat
    
    # direccion <- stack(data.frame(unname(datos[indcx,indcy,2]  )))
    # direccion.v <- stack(data.frame(unname(datos[indcx.v,indcy.v,2]  )))
    
      D <- stack(data.frame(unname(datos[indcx,indcy,2]  )))
    D.v <- stack(data.frame(unname(datos[indcx.v,indcy.v,2]  )))
    
    D$lon <- matriz$lon
    D$lat <- matriz$lat
    
    D.v$lon <- matriz.v$lon
    D.v$lat <- matriz.v$lat
    
    direc <- D.v$values
    direc2 <- direc
    
    indc <- which(direc2>=0 &  direc2<=90  )
    direc[indc] <- 90 - direc[indc]
    
    indc <- which(direc2>90 & direc2<=180)
    direc[indc] <- 270 + direc[indc]
    
    indc <- which(direc2>180 & direc2<=270)
    direc[indc] <- 450 - direc[indc] 
    
    indc <- which(direc2>270 & direc2<=360)
    direc[indc] <- 450 - direc[indc]
    
    M.v$U <- M.v$values*cos(pi*direc/180)
    M.v$V <- M.v$values*sin(pi*direc/180)
    
    if (ii==1){
         magnitud <- M
       magnitud.v <- M.v
        direccion <- D
      direccion.v <- D.v
    }else{
         magnitud$values <- magnitud$values    + M$values

       magnitud.v$U <- magnitud.v$U  + M.v$U
       magnitud.v$V <- magnitud.v$V  + M.v$V
       
        direccion$values <- direccion$values   + D$values
      direccion.v$values <- direccion.v$values + D.v$values
      
    }
    
    
  }
 
     magnitud$values <- magnitud$values    / narchs
       magnitud.v$U  <- magnitud.v$U  / narchs
        magnitud.v$V <- magnitud.v$V  / narchs
       direccion$values <- direccion$values   / narchs
  direccion.v$values <- direccion.v$values / narchs
 
 
 ###### LINEA DE COSTA-----------------
 if (!rgeosStatus()) gpclibPermit()
 gshhs.f.b <- "C:/Users/gramirez/programasR/gshhg-bin-2.3.7/gshhs_f.b"
 shore <- getRgshhsMap(gshhs.f.b, xlim = limite.lon, ylim = limite.lat)
 shore <- fortify(shore)
 
 shore$long <- 360+shore$long
 ##### FRONTERAS
 
 gshhs.gronteras <- 'C:/Users/gramirez/programasR/gshhg-bin-2.3.7/wdb_borders_f.b'
 
 fronteras <- getRgshhsMap(gshhs.gronteras,xlim = limite.lon, ylim = limite.lat)
 
 fronteras <- lapply(slot(fronteras, "lines"), function(x) lapply(slot(x, "Lines"),
                                                                  function(y) slot(y, "coords"))) 
 nfronteras <- length(fronteras)
 
 # 
 # ### MAPA  -------
 # 
 # mapa$lon <- mapa$lon-360
 # shore$long <- shore$long-360
 
  #######
 rm('pp')
 paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')
 windows(width=2000,height=2400)
 pp <- ggplot(data=magnitud,aes(x=lon,y=lat,fill=values))
 pp <- pp + geom_raster(interpolate=TRUE)
 pp <- pp + scale_fill_gradientn(colours= paleta_color,limits =c(0,10))
 pp <- pp + geom_polygon( data=shore,aes(x=long,y=lat,group=group),color = 'black', fill = 'grey80',inherit.aes=FALSE  )
 for (kk in 1:length(fronteras)){
   f <- as.data.frame(fronteras[[kk]])
   pp <- pp + geom_point(data=f,aes( x=X1+360,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
 }

  pp <- pp + geom_quiver(data=magnitud.v,
                         aes(x=lon,y=lat,u=U,v=V),
                         vecsize=0.7,
                         inherit.aes=FALSE)
  pp <- pp + scale_x_continuous(limits=360+range(limite.lon),
                                expand=c(0,0)
                                )
  pp <- pp + scale_y_continuous(limits=range(limite.lat),
                                expand=c(0,0))
  pp <- pp + labs(x='Longitud',y='Latitud',
                title=paste0('Campo de viento a 10m '),
                caption='Fuente: Remote Sensing Systems ASCAT C-2015 
                \n Daily Ocean Vector Winds on 0.25 deg grid, Version 02.1
                \n Elaboración: Dirección de Hidrografía y Navegación
                \n División de Oceanografía')
  pp <- pp + theme( axis.title.x = element_text( size=12,hjust=0.5  ),
                    axis.title.y = element_text( size=12,hjust=0.5  ),
                    title=element_text(size=16),
                    plot.caption = element_text(size = 8,hjust = 0))
  pp <- pp + guides( fill = guide_colorbar(  barheight = unit(11, "cm"),
                                              barwidth = unit(1,'cm'),
                                           label.theme = element_text(size=12),
                                                 title = 'Magnitud\nm/s',
                                           title.theme = element_text(size=12)))

 plot(pp)
 
 
 
 
 