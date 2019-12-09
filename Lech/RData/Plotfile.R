###
install.packages("scales")

require(raster)
require(rgdal)
library(scales)
#♠

tAll <- readOGR("./Lech/GPS-Punkte/Mit Attributen/Tam/TamAllDGMDist.shp")
tAll@data$Vcol <- as.character(tAll@data$Vcol)
str(tAll@data)



pT2 <- read.csv("./Lech/RData/ProfileT2.csv")
pT3 <- read.csv("./Lech/RData/ProfileT3.csv")
pT4 <- read.csv("./Lech/RData/ProfileT4.csv")
pT5 <- read.csv("./Lech/RData/ProfileT5.csv")
str(pT2)


plot(tAll@data$Dist, tAll@data$DGM, xlim = c(125, 350), typ = "n", ylim = c(880, 884), ylab = "Höhe ü. NN. [m]", xlab = "Distanz zur Flussmitte")
lines(pT2$dist, pT2$Z)
lines(pT3$dist, pT3$Z)
lines(pT4$dist, pT4$Z)
lines(pT5$dist, pT5$Z)

points(tAll@data$Dist[tAll@data$Trans == 2], tAll@data$DGM[tAll@data$Trans == 2], pch = 23, cex = (tAll@data$PNT_LIVE/50+.5), col = alpha(tAll@data$Vcol, .8), lwd =2)
points(tAll@data$Dist[tAll@data$Trans == 3], tAll@data$DGM[tAll@data$Trans == 3], pch = 23, cex = (tAll@data$PNT_LIVE/50+.5), col = tAll@data$Vcol, lwd =2)
points(tAll@data$Dist[tAll@data$Trans == 4], tAll@data$DGM[tAll@data$Trans == 4], pch = 23, cex = (tAll@data$PNT_LIVE/50+.5), col = tAll@data$Vcol, lwd =2)
points(tAll@data$Dist[tAll@data$Trans == 5], tAll@data$DGM[tAll@data$Trans == 5], pch = 23, cex = (tAll@data$PNT_LIVE/50+.5), col = alpha(tAll@data$Vcol, .7), lwd =2)
