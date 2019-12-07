#### Schöne plots Skript######

#Wd setzen

setwd("C:/Users/Enz/Documents/Studium/Forschungsprojekt Lech/Tamarix")

library(raster)
library(rgdal)

list.files()


t <- read.csv("Lech/RData/tges.csv", stringsAsFactors = FALSE)

head(t)


vit_col <- c("cyan", "green", "yellow", "brown", "gray50")
sub_col <- c("")

hist(t[, "Pnt.Vit"], breaks = 1:6, xlab = "Vitalitätsstufe", ylab = "Anzahl",
     main = "Tamarisken Anzahl nach Vitalität", col = vit_col)
png("Hist Vitalität.png")


############
##Barplots##
############

## nach Vit 
substrate <- t[, c("Sub.1", "Sub.2","Sub.3", "Sub.4", "Pnt.Vit")]


sub_mean_vit <- data.frame()

for (i in 2:6) {
  sub_vit1 <- substrate$Sub.1[substrate$Pnt.Vit == i]
  sub_vit2 <- substrate$Sub.2[substrate$Pnt.Vit == i]
  sub_vit3 <- substrate$Sub.3[substrate$Pnt.Vit == i]
  sub_vit4 <- substrate$Sub.4[substrate$Pnt.Vit == i]
  
  sub_mean_vit[1,i-1] <- mean(sub_vit1)
  sub_mean_vit[2,i-1] <- mean(sub_vit2)
  sub_mean_vit[3,i-1] <- mean(sub_vit3)
  sub_mean_vit[4,i-1] <- mean(sub_vit4)
  
}

s <- as.matrix(sub_mean_vit)

barplot(s, main = "Substrat Anteile nach Vitalität", ylab = "Anteil in [%]", 
        xlab = "Vitalitatsstufe",  legend = c("Ton bis Feinsand", "Sand", "Kies", "Steine"),
        density = c(100,75,50, 0))
png("Substratanteile im Mittel nach Vitaluität.png")

###############
###Boxplots####
## nach dist

tAll <- readOGR("Lech/GPS-Punkte/Mit Attributen/Tam/TamAllDGMDist.shp")

class(tAll)

head(tAll)

boxplot(tAll@data[,"Dist"] ~ tAll@data[,"PNT_VIT"], 
        main = "Entfernung vom Hauptstrom nach Vitalität", xlab = "Vitalitätsstufe", 
        ylab = "Entfernung in Meter" )

