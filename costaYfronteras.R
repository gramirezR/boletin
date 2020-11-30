cat('\014')
rm(list=ls())
library('ggplot2')
library('maptools')
library('reshape2')

setwd('D:/programasR/')
  gshhs.dir <- 'D:/programasR/gshhs/'
salida.arch <- 'D:/marcoMerma/costa_Callao_full.RData'

##########

limites.lon <- c(-90, -75)+360
limites.lat <- c(-12.5, -11.5)

gpclibPermit()

shore <- getRgshhsMap( paste0(gshhs.dir,'gshhs_f.b') ,
                        xlim = limites.lon-360,
                        ylim = limites.lat,avoidGEOS=TRUE,shift=TRUE)

plot(shore)

#shore1 <- fortify(shore1)
# shore2 <- getRgshhsMap( paste0(gshhs.dir,'gshhs_f.b') ,
#                         xlim = c(-180,limites.lon[2]-360),
#                         ylim = limites.lat)
# shore2 <- fortify(shore2)
# shore <- rbind(shore1,shore2)

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

fronteras1 <- getRgshhsMap( paste0(gshhs.dir,'wdb_borders_f.b') ,
                            xlim = c(-179.9,limites.lon[2]-360), ylim = limites.lat)

fronteras1 <- lapply(slot(fronteras1, "lines"), function(x) lapply(slot(x, "Lines"),
function(y) slot(y, "coords")))



fronteras2 <- getRgshhsMap( paste0(gshhs.dir,'wdb_borders_f.b') ,
                            xlim = c(limites.lon[1]-360,175-360), ylim = limites.lat)
#
fronteras2 <- lapply(slot(fronteras2, "lines"), function(x) lapply(slot(x, "Lines"),
                          function(y) slot(y, "coords")))


fr.lon <- lapply(fronteras1,function (x) {
  x[[1]][,1]+360 })
#
for (ii in 1:length(fronteras1)){
  fronteras1[[ii]][[1]][,1] <- fr.lon[[ii]]
}
rm('fr.lon')

fr.lon <- lapply(fronteras2,function (x) {
  x[[1]][,1]+360 })
#
for (ii in 1:length(fronteras2)){
  fronteras2[[ii]][[1]][,1] <- fr.lon[[ii]]
}
rm('fr.lon')

fronteras <- fronteras1

rm('fronteras1')
rm('fronteras2')

nfronteras <- length(fronteras)
#indc <- shore$long<0

# pp <- ggplot(data=shore,aes(x=long,y=lat,facets=group))
# pp <- pp + geom_point()
# plot(pp)

save(list=c('shore','fronteras'),file=salida.arch)
