cat('\014')
rm(list=ls())
library('ggplot2')
# library('rgeos')
library('maptools')
library('reshape2')
library(rgdal)
graphics.off()
setwd('E:/programasR/')
gshhs.dir <- 'E:/programasR/gshhg-bin-2.3.7/'
salida.arch <- 'E:/programasR/boletin/costa_Sur.RData'

##########

limites.lon <- c(-90, -70)+360
limites.lat <- c(-19, -14.6)

gpclibPermit()

# rgeosStatus()

shore <- getRgshhsMap( paste0(gshhs.dir,'gshhs_f.b') ,
                         xlim = limites.lon-360,
                         ylim = limites.lat,
                    avoidGEOS = TRUE,shift=TRUE)

plot(shore)

shore <- fortify(shore)


save(list='shore',file=salida.arch)

# costa <- as.data.frame(shore@polygons[[1]]@Polygons[[2]])
# names(costa) <- c('lon','lat')
# shore <- fortify(shore)
#####################

# indc <- which(shore$group=='3.1')

# plot( shore[indc,c('long','lat')] )

############
# shore1$long <- shore1$long + 360
# shore2 <- getRgshhsMap( paste0(gshhs.dir,'gshhs_f.b') ,
#                         xlim = limites.lon,
#                         ylim = limites.lat)
# shore2 <- fortify(shore2)
# 
# shore <- rbind(shore1,shore2)
# 
# rm(shore2)
# shore <- shore1
# rm(shore1)
# shore$long <- 360 + shore$long
############
windows()
pp <- ggplot(data=shore,aes(x=long,y=lat,facets=group))
pp <- pp + geom_point()
plot(pp)
###### FRONTERAS #######

# fronteras1 <- Rgshhs( fn=paste0(gshhs.dir,'wdb_borders_f.b') ,
#                             xlim = limites.lon, ylim = limites.lat, level=1)
# 
# fronteras1 <- lapply(slot(fronteras1, "lines"), function(x) lapply(slot(x, "Lines"),
# function(y) slot(y, "coords")))
# 
# 
# 
# fronteras2 <- getRgshhsMap( paste0(gshhs.dir,'wdb_borders_f.b') ,
#                             xlim = c(limites.lon[1]-360,175-360), ylim = limites.lat)
# #
# fronteras2 <- lapply(slot(fronteras2, "lines"), function(x) lapply(slot(x, "Lines"),
#                           function(y) slot(y, "coords")))
# 
# 
# fr.lon <- lapply(fronteras1,function (x) {
#   x[[1]][,1]+360 })
# #
# for (ii in 1:length(fronteras1)){
#   fronteras1[[ii]][[1]][,1] <- fr.lon[[ii]]
# }
# rm('fr.lon')
# 
# fr.lon <- lapply(fronteras2,function (x) {
#   x[[1]][,1]+360 })
# #
# for (ii in 1:length(fronteras2)){
#   fronteras2[[ii]][[1]][,1] <- fr.lon[[ii]]
# }
# rm('fr.lon')
# 
# fronteras <- fronteras1
# 
# rm('fronteras1')
# rm('fronteras2')
# 
# nfronteras <- length(fronteras)
#indc <- shore$long<0

# pp <- ggplot(data=shore,aes(x=long,y=lat,facets=group))
# pp <- pp + geom_point()
# plot(pp)
# shore <- costa
# save(list='shore',file=salida.arch)
