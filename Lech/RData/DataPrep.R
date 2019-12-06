### Test

#install.packages("geosphere")

require(raster)
require(rgdal)
require(geosphere)

# Daten einlesen ####

### GPS-Punkte
# t2 <- readOGR("/home/denis/Dokumente/GitHub-Repos/Tamarix/Tamarix/Lech/GPS-Punkte/Mit Attributen/Tam/T2_Tam.shp")
# t3 <- readOGR("/home/denis/Dokumente/GitHub-Repos/Tamarix/Tamarix/Lech/GPS-Punkte/Mit Attributen/Tam/T3_Tam.shp")
# t4 <- readOGR("/home/denis/Dokumente/GitHub-Repos/Tamarix/Tamarix/Lech/GPS-Punkte/Mit Attributen/Tam/T4_Tam.shp")
# t5 <- readOGR("/home/denis/Dokumente/GitHub-Repos/Tamarix/Tamarix/Lech/GPS-Punkte/Mit Attributen/Tam/T5_Tam.shp")


tAll<- readOGR("./Lech/GPS-Punkte/Mit Attributen/Tam/Tam_All_DGM.shp")

head(tAll, 5)
summary(tAll)
str(tAll)

for (i in 2:22) {
  tAll@data[, i] <- as.character(tAll@data[, i])
}

for (j in c(3, 5:22)) {
  tAll@data[, j] <- as.numeric(tAll@data[, j])
}

### Lech-Mittellinie
lech <- readOGR("./Lech/shapes/Lech_Mittellinie.shp")

### Kontrolle
plot(lech)
points(tAll)

# Distanz der Punkte zur Lech-Mittellinie ####
tAll_Dist <- cbind(tAll@data, Dist = dist2Line(as.data.frame(tAll)[c(24:25)], lech)[,1])

tAll@data <- tAll_Dist

writeOGR(tAll, layer = "TamAll", dsn = "./Lech/GPS-Punkte/Mit Attributen/Tam/TamAllDGMDist.shp", driver = "ESRI Shapefile")


head(tAll@data)
