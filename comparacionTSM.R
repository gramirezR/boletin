cat('\014')
rm(list=ls())
library('ggplot2')


arch1 <- 'D:/boletin/datos/temp/2017/figuras/indice01.RData'
arch2 <- 'D:/boletin/datos/temp/2020/figuras/indice02.RData'

load(arch1)
promedios2016 <- promedios


load(arch2)
promedios2019 <- promedios
rm(promedios)
########################################

T1 <- promedios2016$T - promedios2016$T[1]
T2 <- promedios2019$T - promedios2019$T[1]
semanas <- seq(from=promedios2016$T[1],to=data.table::last(promedios2016$T),by='week')
etqt <- format(semanas,'%b-%d')
marcas <- as.numeric(semanas - promedios2016$T[1])
########################

promedios2016$x <- as.numeric(T1)
promedios2019$x <- as.numeric(T2)

names(promedios2016) <- c('x','atsm','T')
names(promedios2019) <- c('x','atsm','T')

promedios2016$periodo <- '2016'
promedios2019$periodo <- '2019'

promedios <- rbind(promedios2016,promedios2019)

#######################
if(exists('pp')) rm('pp')
windows(width=1200,height=700)
pp <- ggplot( data=promedios , aes(x=T,y=atsm,colour=periodo) )
pp <- pp + geom_line(size=2)
pp <- pp + scale_x_continuous(breaks = marcas,labels = etqt)
pp <- pp + scale_y_continuous(limits = c(-1,3) )
pp <- pp + scale_colour_manual(labels = c('2016-2017','2019-2020'),values = c('blue','red'))
pp <- pp + labs(x='Fecha',y='ATSM (°C)',
                title='Promedio de la ATSM dentro de las 50 millas',
                caption='Fuente: COPERNICUS MARINE ENVIRONMENT MONITORING SERVICE (CMEMS v3.0)
                Climatología: 1981-2009
                Eaboración: Departamento de oceanografía')
pp <- pp + theme(axis.text.x = element_text( size = 24,color='black',angle=90 ),
                 axis.text.y = element_text( size = 24,color='black' ),
                 axis.title.x = element_text( size = 26 ),
                 axis.title.y = element_text( size = 26 ),
                 title = element_text( size = 20 ),
                 plot.subtitle = element_text(size = 20),
                 plot.caption = element_text( size = 16,hjust = 0))
plot(pp)






