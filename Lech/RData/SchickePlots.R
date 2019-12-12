#### Schöne plots Skript######

#Wd setzen

#setwd("C:/Users/Enz/Documents/Studium/Forschungsprojekt Lech/Tamarix")


library(raster)
library(rgdal)

list.files()


t <- read.csv("./Lech/RData/tges.csv", stringsAsFactors = FALSE)

head(t)

x11()
vit_col <- c("dodgerblue2", "olivedrab3", "gold2", "brown", "gray50")

j <- t$Pnt.Vit
class(j)

b <- hist(j, breaks = (0.5:6.5),ylim = c(0,120), xlab = "Vitalitätsstadium", 
          ylab = "Anzahl Tamarisken", main = "Tamarisken Anzahl nach Vitalität", 
          col = vit_col, xlim = c(1.5,6.5))
text(b$mids, b$counts, labels=b$counts, adj = c(0.5,-0.5))

png("Hist Vitalität.png")

## selber plot mit ggplot2 #####
cs <- c("dodgerblue2", "olivedrab3", "gold2", "brown", "gray50")
t$Vitalitässtadium <- factor(t$Pnt.Vit, levels= c(2:6), labels= c("Juvenil", "Jung Adult", "Adult", "Senil", "Tot"))
head(t)

ggplot(t, aes(Vitalitässtadium)) + 
  ggtitle("Tamariskenanzahl nach Vitalität") + 
  scale_y_continuous(limits = c(0, 110)) +
  ylab("Anzahl Tamarisken") +
  geom_histogram(stat= "count", fill= cs, col= "black") + 
  geom_text(stat='count', aes(label=..count..), vjust=-1, size= 8) +
  theme_classic() %+replace%
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"))

  ###############

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
x11()
z <- barplot(s, main = "Substrat Anteile nach Vitalität", ylab = "Anteil in [%]", 
        xlab = "Vitalitatsstadium", density = c(100,75,50, 0),xlim = c(0,7.5))

legend(6.2,100,legend = c("Ton bis Feinsand", "Sand", "Kies", "Steine"), 
       density = c(100,75,50, 0))
#text(z, 98,labels= paste("N =" ,b$counts[2:6] ))


png("Substratanteile im Mittel nach Vitaluität.png")

##### Density plot nach Distanz###

#a <- tAll@data["Dist"]
#a <- as.matrix(t((a)))
#a <- sort(a)

#x11()
#plot(density(a))
#barplot(a)
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
x11()
o <- barplot(d, ylim = c(0,60), main = "Vegetationsdeckung nach Vitalität", ylab = "Anteil in [%]", 
        xlab = "Vitalitatsstadium", density = c(100,75,50, 0), beside = TRUE)
legend(0,60,legend= c("Moos", "Kraut", "Busch", "Baum"), density = c(100,75,50, 0)  )

#text(o, 50,labels= paste("N =" ,b$counts[2:6] ))

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

ta <- read.csv("C:/Users/Enz/Documents/Studium/Lech/Tamarix/Lech/RData/tges.csv", 
               stringsAsFactors = FALSE)


v <- as.matrix(vtype_sum_vit)
colnames(v) <- c("Pionier", "Weidengeb.", "Erlen-Weidengeb.", "Grauerlengeb.","Erlenwald",
                 "Kiefer-Erlen-Weidengeb.", "Kiefergeb.", "Erlen-Kiefergeb.", "Kiefernwald")   
k <- c()

for (p in 1:9) {
  
k[p] <- (sum(ta$X.Veg.Type.Faktor.[ta$X.Veg.Type.Faktor. == p]))/p
}

x11()
h <- barplot(v, main = "Vitalitätstadium nach Vegetationstyp", ylab = "Anzahl Tamarisken", 
       col = vit_col)
legend(8, 80,legend = c("Juvenil", "Jung Adult", "Adult", "Senil", "Tot"), fill = vit_col, 
       cex = 1.25)

text(h, 40,labels= paste("N =" , k), cex = 1.25)

png("Vitalitätsstadium nach Vegetationstyp.png")


###############
###Boxplots####
## nach dist

tAll <- readOGR("Lech/GPS-Punkte/Mit Attributen/Tam/TamAllDGMDist.shp")

class(tAll)

head(tAll)
x11()

boxplot(tAll@data[,"Dist"] ~ tAll@data[,"PNT_VIT"], 
        main = "Entfernung vom Hauptstrom nach Vitalität", xlab = "Vitalitätsstufe", 
        ylab = "Entfernung in Meter" )



boxplot(tAll@data[,"DGM"] ~ tAll@data[,"PNT_VIT"], 
        main = "Entfernung vom Hauptstrom nach Vitalität", xlab = "Vitalitätsstufe", 
        ylab = "Entfernung in Meter" )


