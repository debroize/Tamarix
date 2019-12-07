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


# Add transect ####

t2 <- cbind(read.csv("./Lech/RData/T2.csv", stringsAsFactors = F)[, 1], c(2))
t3 <- cbind(read.csv("./Lech/RData/T3.csv", stringsAsFactors = F)[, 1], c(3))
t4 <- cbind(read.csv("./Lech/RData/T4.csv", stringsAsFactors = F)[, 1], c(4))
t5 <- cbind(read.csv("./Lech/RData/T5.csv", stringsAsFactors = F)[, 1], c(5))

head(t2)

tges <- rbind(t2, t3, t4, t5)

tsort <- numeric()

for (k in 1:length(tAll@data$NAME)) {
 tsort[k] <- tges[tges[, 1] == tAll@data$NAME[k], 2] 
}


tAll@data <- cbind(tAll@data, Trans = tsort)

colV <- data.frame(2:6, c("dodgerblue2", "green", "gold2", "brown", "gray50"))
colnames(colV) <- c("Vit", "col")
str(colV)

coly <- character()

for (l in 1:276) {
  coly[l] <- as.character(colV$col[colV$Vit == tAll@data$PNT_VIT[l]])
}

tAll@data <- cbind(tAll@data, Vcol = as.character(coly))
tAll@data$Vcol <- as.character(tAll@data$Vcol)
#tAll@data$Vcol <- as.character(coly)

str(tAll@data)

writeOGR(tAll, layer = "TamAll", dsn = "./Lech/GPS-Punkte/Mit Attributen/Tam/TamAllDGMDist.shp", driver = "ESRI Shapefile")


head(tAll@data)
