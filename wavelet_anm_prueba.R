cat('\014')
rm(list=ls())
library(WaveletComp)
archivo <- 'E:/programasR/hov_anm.RDS'

hovmoller <- readRDS(archivo)

##################################

matriz <- hovmoller[[1]]

########
windows()
raster::contour(t(matriz))

############

serie <- data.frame(x = matriz[,1])

serie_wavelet <- analyze.wavelet(my.data = serie,
                                 loess.span = 1,
                                 dt = 1, dj = 1/250,
                                 lowerPeriod = 10,
                                 upperPeriod = 120,
                                 make.pval = TRUE, n.sim=10)

##################
windows()
wt.image(serie_wavelet, color.key = 'quantile',
         n.levels = 250)

#################
# windows()
resultado <- reconstruct(serie_wavelet, plot.waves = FALSE)






