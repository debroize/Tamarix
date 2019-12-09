#### Schöne plots Skript######

#Wd setzen

setwd("C:/Users/Enz/Documents/Studium/Forschungsprojekt Lech/Tamarix")

library(raster)
library(rgdal)

list.files()


t <- read.csv("Lech/RData/tges.csv", stringsAsFactors = FALSE)

head(t)

x11()
vit_col <- c("dodgerblue2", "green", "gold2", "brown", "gray50")

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

colnames(s) <- c("Juvenil", "Jung Adult", "Adult", "Senil", "Tot")

barplot(s, main = "Substrat Anteile nach Vitalität", ylab = "Anteil in [%]", 
        xlab = "Vitalitatsstufe", density = c(100,75,50, 0),xlim = c(0,7.5))
legend(6.2,100,legend = c("Ton bis Feinsand", "Sand", "Kies", "Steine"), 
       density = c(100,75,50, 0))

png("Substratanteile im Mittel nach Vitaluität.png")



#################
##veg dens



veg_dens <- t[, c("VegDens.1", "VegDens.2","VegDens.3", "VegDens.4", "Pnt.Vit")]


dens_mean_vit <- data.frame()

for (i in 2:6) {
  dens_vit1 <- veg_dens$VegDens.1[veg_dens$Pnt.Vit == i]
  dens_vit2 <- veg_dens$VegDens.2[veg_dens$Pnt.Vit == i]
  dens_vit3 <- veg_dens$VegDens.3[veg_dens$Pnt.Vit == i]
  dens_vit4 <- veg_dens$VegDens.4[veg_dens$Pnt.Vit == i]
  
  dens_mean_vit[1,i-1] <- mean(dens_vit1)
  dens_mean_vit[2,i-1] <- mean(dens_vit2)
  dens_mean_vit[3,i-1] <- mean(dens_vit3)
  dens_mean_vit[4,i-1] <- mean(dens_vit4)
  
}

d <- as.matrix(dens_mean_vit)

colnames(d) <- c("Juvenil", "Jung Adult", "Adult", "Senil", "Tot")

barplot(d, main = "Vegetationsdeckung nach Vitalität", ylab = "Anteil in [%]", 
        xlab = "Vitalitatsstufe", density = c(100,75,50, 0))
legend(0,100,legend= c("Moos", "Kraut", "Busch", "Baum"), density = c(100,75,50, 0)  )

png("Vegetationsdeckungen im Mittel nach Vitaluität.png")


#####################
########### veg type

vtype <- t[, c("Veg.Type", "Pnt.Vit")]

vtype_sum_vit <- data.frame()
unique(vtype$Veg.Type)


for (i in 2:6) {
  
  vtype_vit1 <- vtype$Pnt.Vit[vtype$Veg.Type == "Pionier"]
  vtype_vit2 <- vtype$Pnt.Vit[vtype$Veg.Type == "Weidengebuesch"]
  vtype_vit3 <- vtype$Pnt.Vit[vtype$Veg.Type == "Erlen-Weidengebuesch"]
  vtype_vit4 <- vtype$Pnt.Vit[vtype$Veg.Type == "Grauerlengebuesch"]
  vtype_vit5 <- vtype$Pnt.Vit[vtype$Veg.Type == "Erlenwald"]
  vtype_vit6 <- vtype$Pnt.Vit[vtype$Veg.Type == "Kiefern-Erlen-Weidengebuesch"]
  vtype_vit7 <- vtype$Pnt.Vit[vtype$Veg.Type == "Erlen-Kiefergebuesch"]
  vtype_vit8 <- vtype$Pnt.Vit[vtype$Veg.Type == "Kiefergebuesch"]
  vtype_vit9 <- vtype$Pnt.Vit[vtype$Veg.Type == "Kiefernwald"]
  
  
  
  vtype_sum_vit[i-1,1] <- sum(vtype_vit1 == i)
  vtype_sum_vit[i-1,2] <- sum(vtype_vit2 == i) 
  vtype_sum_vit[i-1,3] <- sum(vtype_vit3 == i)  
  vtype_sum_vit[i-1,4] <- sum(vtype_vit4 == i)
  vtype_sum_vit[i-1,5] <- sum(vtype_vit5 == i)
  vtype_sum_vit[i-1,6] <- sum(vtype_vit6 == i)
  vtype_sum_vit[i-1,7] <- sum(vtype_vit7 == i)
  vtype_sum_vit[i-1,8] <- sum(vtype_vit8 == i)
  vtype_sum_vit[i-1,9] <- sum(vtype_vit9 == i)
  }


v <- as.matrix(vtype_sum_vit)
colnames(v) <- c("Pionier", "Grauerlengeb.", "Weidengeb.", "Erlen-Weidengeb.", "Kiefergeb.", 
                 "Kiefern-Erlen-Weidengebu.", "Kiefernwald", "Erlen-Kiefergeb.", "Erlenwald")   
barplot(v, main = "Vitalitätstufe nach Vegetationstyp", ylab = "Anzahl Tamarisken", 
       col = vit_col)
legend(8, 80,legend = c("Juvenil", "Jung Adult", "Adult", "Senil", "Tot"), fill = vit_col)

  png("Vitalitätsstufen nach Vegetationstyp.png")


###############
###Boxplots####
## nach dist

tAll <- readOGR("Lech/GPS-Punkte/Mit Attributen/Tam/TamAllDGMDist.shp")

class(tAll)

head(tAll)


boxplot(tAll@data[,"Dist"] ~ tAll@data[,"PNT_VIT"], 
        main = "Entfernung vom Hauptstrom nach Vitalität", xlab = "Vitalitätsstufe", 
        ylab = "Entfernung in Meter" )



boxplot(tAll@data[,"DGM"] ~ tAll@data[,"PNT_VIT"], 
        main = "Entfernung vom Hauptstrom nach Vitalität", xlab = "Vitalitätsstufe", 
        ylab = "Entfernung in Meter" )


