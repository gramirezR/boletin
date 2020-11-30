cat("\014") 
graphics.off()
rm(list=ls())

# library('RNetCDF')
setwd('D:/programasR/boletin/')
library('stringr')
library('tidyverse')
library('scales')
library('directlabels')
library('maptools')
library('reshape2')
require('RNetCDF')
require('metR')
gshhs.dir <- 'D:/programasR/gshhg-bin-2.3.7/'
carpetaDatos <- 'D:/NOAA/SST/'
carpetaCP <- 'D:/NOAA/SST/componentesP/'
load('costa_Peru_205.RDat')

lim.lon <- c(250, 290)-360
lim.lat <- c(-20, 5)

meses <- lubridate::month(ISOdate(2019,1:12,1),label=TRUE,abbr=FALSE)

anios <- 1982:2018

fechas <- expand.grid(meses,anios)

archivos <- paste0(carpetaDatos,fechas$Var1,fechas$Var2,'.RData')

load(archivos[1])

filas <- length(anomaliaT$z)
columnas <- length(archivos)

ASST <- matrix(nrow=filas,ncol = columnas)

for (ii in 1:columnas){
  load(archivos[ii])
  ASST[,ii] <- -anomaliaT$z
}

indcNA <- which(is.na(ASST[,1]))

ASST <- ASST[-indcNA,]

lon <- anomaliaT$x[-indcNA]
lat <- anomaliaT$y[-indcNA]

s <- svd(ASST)


paleta_color <- cptcity::cpt('ncl_amwg_blueyellowred')

#######
niveles <- seq(from=-0.026,to=0.026,by=0.002)
for (pcN in 1:4){
campo <- data.frame(x=lon,y=lat,z=s$u[,pcN ])

     subtitulo.grafico <- paste0('Componente principal ',pcN)
     png(width=1200,height=800,filename = paste0(carpetaCP,'FEO',pcN,'.png'))
     pp <- ggplot(data=campo,aes(x=x,y=y,z=z,fill=z))
     pp <- pp+geom_raster(aes(fill = z),interpolate=TRUE,show.legend = TRUE  )
     pp <- pp + scale_fill_gradientn( colours = paleta_color,
                                      limits = range(niveles))
     pp <- pp + geom_contour(linetype = 1 ,
                             col = 'black' ,
                             breaks = niveles)
     # pp <- pp + geom_text_contour(stroke = 0.15,skip=0,
     #                              min.size =10,size=10,rotate = FALSE,
     #                              check_overlap=TRUE,breaks = niveles)
     pp <- pp + geom_polygon( data=shore,aes(x=long-360,y=lat,group=group),color = 'black', fill = 'grey80',inherit.aes=FALSE  )
     for (kk in 1:length(fronteras)){
       f <- as.data.frame(fronteras[[kk]])
       pp <- pp + geom_point(data=f,aes( x=X1-360,y=X2 ),col='grey30',size=0.05,inherit.aes=FALSE)
     }
     pp <- pp + labs(x='Longitud',y='Latitud',
                     title=paste0('DIRECCIÓN DE HIDROGRAFÍA Y NAVEGACIÓN \n',
                                  'Dpto. de Oceanografía - Div. Oceanografía'),
                     subtitle = subtitulo.grafico,
                     caption = 'Fuente: ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2.highres/')
     pp <- pp + theme_bw(   )
     pp <- pp + scale_x_continuous(limits = lim.lon,
                                   expand = c(0.01,0),
                                   breaks= seq(from=lim.lon[1],to=lim.lon[2],by=5))
     pp <- pp + scale_y_continuous(limits = lim.lat,
                                   expand = c(0.01,0))
     pp <- pp + guides( fill = guide_colorbar(  barheight = unit(14, "cm"),
                                                barwidth = unit(0.85,'cm'),
                                                label.theme = element_text(size=20),
                                                title = 'TSM °C',
                                                title.theme = element_text(size=22)))
     pp <- pp + theme( axis.title.x = element_text( size=28,hjust=0.5  ),
                       axis.title.y = element_text( size=28,hjust=0.5  ),
                       axis.text = element_text(size=20),
                       title=element_text(size=26),
                       plot.subtitle=element_text(size=24),
                       plot.caption = element_text(size = 18))
     plot(pp)
     dev.off()

     rm(list='pp')
     tiempo <- seq(from=as.Date('1982-01-01'),to=as.Date('2018-12-01'),by='month')
     
     cptmp <- s$v[,pcN ]
     
     # cptmp <- pracma::movavg(cptmp,n = 3,type = 'r')
     
      mxCP <- max(cptmp)
      cptmp <- cptmp/mxCP
     
      indc <- as.list(cptmp>0)
      
      signo <- lapply(X = indc,FUN = function(x){
        if (x) {
          'positivo'
        }else{
            'negativo'
          }
      }) 
     signo <- unlist(signo)
     
  
        
        
        modelo <- smooth.spline(x=tiempo,y=cptmp) 
        
        T <- seq(from=tiempo[1],to=last(tiempo),length.out = 1000)
        
        resultado <- predict(modelo,newdata = T)
        
        CP <- data.frame(x=T,y=resultado)
        
     niveles <- seq(-0.05,0.05,by=0.0025)
     subtitulo.grafico <- paste0('componentePrincipal ',pcN)
     png(width=1200,height=800,filename = paste0(carpetaCP,'PC',pcN,'.png')) 
     pp <- ggplot(data=CP,aes(x=x,y=y))
     #pp <- pp + geom_area(data=subset(CP,y>=0))
     pp <- pp + geom_line()
     pp <- pp + xlab('Fecha')
     pp <- pp + theme_bw(   )
     pp <- pp + scale_x_date(date_breaks ='year',labels = waiver())
     pp <- pp + theme(  axis.text.x = element_text(angle=90,vjust = 0.5,size=16),
                        axis.title.x = element_text(size=20))
     plot(pp)
     dev.off()
}
