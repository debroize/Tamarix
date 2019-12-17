
setwd("C:/Users/Enz/Documents/Studium/Forschungsprojekt Lech/Tamarix")

library(raster)
library(rgdal)
library(plyr)

list.files()


t <- read.csv("Lech/RData/tges.csv", stringsAsFactors = FALSE)
tAll <- readOGR("Lech/GPS-Punkte/Mit Attributen/Tam/TamAllDGMDist.shp")

summary(tAll@data$Dist)
head(t)

vit_col <- c("dodgerblue2", "olivedrab3", "gold2", "brown", "gray50")


a <- data.frame()
a <- tAll$PNT_VIT[tAll$Dist <= "175"   ]
a <- count(a)

b <- tAll$PNT_VIT[tAll$Dist < 200 & tAll$Dist > 175]
b <- count(b)

c <- tAll$PNT_VIT[tAll$Dist <225 & tAll$Dist > 200]
c <- count(c)

d <- tAll$PNT_VIT[tAll$Dist > "225"]
d <- count(d)

dist_vit <- data.frame()

for (i in 2:6) {
  dist_vit[i-1, 1] <- a$freq[b$x == i]
  dist_vit[i-1, 2] <- b$freq[b$x == i] 
  dist_vit[i-1, 3] <- c$freq[b$x == i]
  dist_vit[i-1, 4] <- d$freq[b$x == i]  
  
}

d <- as.matrix(dist_vit)

colnames(d) <- c("<175", "175 - 200", "200 - 225", ">225")
x11()
o <- barplot(d, ylim = c(0,40), main = "Vitalitätsstadium nach Entfernung zum Fluss", 
             ylab = "Anzahl Tamarisken", xlab = "Entfernung zum Fluss in [m]",col = vit_col, 
             beside = TRUE, cex.lab = 1.4, cex.main = 1.4)
legend(0,40,legend= c("Juvenil", "Jung Adult", "Adult", "Senil", "Tot"), fill = vit_col, cex = 1.4)

#text(o, 30,labels= paste("N =" ,b$counts[2:6] ))

png("Vitalitätsstadium nach Entfernung zum Fluss.png")
dev.off()
