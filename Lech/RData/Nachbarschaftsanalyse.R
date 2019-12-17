# Nachbarschaftsanalyse ####

require(rgdal)

### GPS-Punkte mit DGM und Dist
tAll <- readOGR("./Lech/GPS-Punkte/Mit Attributen/Tam/TamAllDGMDist.shp")

## Datentypen anpassen
for (i in 2:22) {
  tAll@data[, i] <- as.character(tAll@data[, i])
}
for (j in c(3, 5:22)) {
  tAll@data[, j] <- as.numeric(tAll@data[, j])
}

## Datensatz kürzen
tSub <- tAll
tSub@data <- tAll@data[c('NAME', 'VEG_TYPE', 'PNT_VIT', 'MORPHDYN', 'DGM', 'Dist', 'Trans')]



## Distanzmatrix einlesen 
dismat <- readOGR("./Lech/shapes/Distanzmatrix_Klassisch.shp")
df_dismat <- dismat@data[-1]


## Nähestens 5 Punkte filtern 
nearest5 <- data.frame()
for(i in 1:nrow(df_dismat)){
  nearest5[i, 1:5] <- sort(df_dismat[i,])[2:6]
  nearest5[i, 6:10] <- as.numeric(substr(colnames(sort(df_dismat[i,])[2:6]), start = 2, stop = 5))
}

### Spalten und Reihen beschriften
colnames(nearest5) <- c('Dnr1', 'Dnr2', 'Dnr3', 'Dnr4', 'Dnr5', 'Pnr1', 'Pnr2', 'Pnr3', 'Pnr4', 'Pnr5')
rownames(nearest5) <- dismat@data[[1]]


## Punktnummer durch VitTyp ersetzt
for(i in tSub@data$NAME){
  nearest5[,6:10][nearest5[,6:10] == i] <- tSub@data$PNT_VIT[tSub@data$NAME == i]
}


# Nächste Nachbar Analyse, Mittlere-Distanzen #####
## NN-DataFrame mit Datensatz verbinden
tSub@data <- cbind(tSub@data, nearest5)

tSub@data$mean3 <- rowMeans(tSub@data[c('Dnr1', 'Dnr2', 'Dnr3')])
tSub@data$mean5 <- rowMeans(tSub@data[c('Dnr1', 'Dnr2', 'Dnr3', 'Dnr4', 'Dnr5')])


## ANOVA Bedingungen für Nachbarschaften
boxplot(tSub@data$mean3 ~ tSub@data$PNT_VIT, ylim= c(0, 15))
boxplot(tSub@data$mean5 ~ tSub@data$PNT_VIT, ylim= c(0, 15), xlab = "Vitalität", ylab= "Distanz 5 nächste Pflanzen")

### Test auf Normalverteilung
shapiro.test(tSub@data$mean3) # nicht normalverteilt
shapiro.test(tSub@data$mean5) # nicht normalverteilt
### Test auf Homogenität der Varianzen
fligner.test(tSub@data$mean3 ~ tSub@data$PNT_VIT) # Varianzen sind verschieden
fligner.test(tSub@data$mean3 ~ tSub@data$PNT_VIT) # Varianzen sind verschieden
# Keine ANOVA zulässig

## T-Tests
t.test(tSub@data$mean3[tSub@data$PNT_VIT == 5], tSub@data$mean3[tSub@data$Pnt.Vit == 6])
## keie Unterschiede der Mittelwerte




# Nächste Nachbar Analyse, benachbarte VitTypen #####
nbVitTab <- data.frame(matrix(NA, nrow = 5, ncol = 5), row.names = c(2:6))
colnames(nbVitTab) <- c(2:6)
for(ownVitTyp in 2:6){
  for(nbrVitTyp in 2:6){
    
    nbVitTab[ownVitTyp-1, nbrVitTyp-1] <- 
      sum(tSub@data[tSub@data$PNT_VIT == ownVitTyp,][c('Pnr1', 'Pnr2', 'Pnr3', 'Pnr4', 'Pnr5')] == nbrVitTyp )
    nbVitTab[ownVitTyp-1, nbrVitTyp-1] <- 
      nbVitTab[ownVitTyp-1, nbrVitTyp-1]/
      (nrow(tSub@data[tSub@data$PNT_VIT == ownVitTyp,]) * nrow(tSub@data[tSub@data$PNT_VIT == nbrVitTyp,]))
  }
}
nbVitTab

## normalisieren
nbVitTab[1,1]
nrow(tSub@data[tSub@data$PNT_VIT == ownVitTyp,])


summary(nbVitTab)
imgVitTab <- raster(as.matrix(nbVitTab))
plot(imgVitTab, main= "Nachbarschaftsbeziehungen")

colnames(nbVitTab) <- c("Juvenil", "Jung-Adult", "Adult", "Senil", "Tot")
rownames(nbVitTab) <- c("Juvenil", "Jung-Adult", "Adult", "Senil", "Tot")
nbVitTab2 <- nbVitTab*10
corrplot(as.matrix(as.data.frame(nbVitTab2)), method = "color", cl.lim=c(min(nbVitTab2),max(nbVitTab2)), col=colorRampPalette(c("darkblue","white","white","white", "lightblue","blue","blue", "darkblue"))(300), cl.pos = "n", tl.col = "black", tl.srt = 45, addgrid.col = "black")
?corrplot
corrplot(as.matrix(as.data.frame(nbVitTab2)), method = "color", cl.lim=c(min(nbVitTab2),max(nbVitTab2)), col=colorRampPalette(c("white", "red", "pink","yellow","lightgreen","green", "darkgreen"))(200), cl.pos = "n", tl.col = "black", tl.srt = 45)

min(nbVitTab)
max(nbVitTab)










# Nachbarschaftsanalyse nach Distanz ####
require(dplyr)

countTam <- function(df, distance){
  
  cnt = NULL
  for(i in 1:276){ 
  cnt[i] <- sum(df[i,] < distance)
  
  }
 return(cnt)
}

## Spalten erstellen die Anzahl an Tamarisken in Bestimmten Umkreis wiedergibt
countTam(df_dismat, 13)
#df_dismat$SumDist5 <- countTam(df_dismat, 5)
#df_dismat$SumDist10 <- countTam(df_dismat, 10)
#df_dismat$SumDist20 <- countTam(df_dismat, 20)
#df_dismat$SumDist35 <- countTam(df_dismat, 35)
#df_dismat$SumDist50 <- countTam(df_dismat, 50)
#df_dismat$SumDist100 <- countTam(df_dismat, 100)
#df_dismat$SumDist150 <- countTam(df_dismat, 150)
#df_dismat$SumDist200 <- countTam(df_dismat, 200)

ncol(df_dismat)
Dist <- c(1:20, 35, 50, 100)
length(Dist)

for(i in c(Dist)){
  
  df_dismat[, (ncol(df_dismat) + 1)] <- countTam(df_dismat, Dist)

 colnames(df_dismat)[ncol(df_dismat)] <- paste0("SumDist", i)
   
}
colnames(df_dismat)
distCounts <- df_dismat[, -c(1:(ncol(df_dismat) - length(Dist)))]
#distCounts <- df_dismat[c("SumDist5", "SumDist10", "SumDist20", "SumDist35", "SumDist50", "SumDist100", "SumDist150", "SumDist200")]
colnames(distCounts)


## Datensatz zusammenführen
tSub@data <- cbind(tSub@data, distCounts)

## Für einzelne Vitalitätstypen Mitteln
DistTab <- data.frame(matrix(NA, nrow = 5, ncol = 23), row.names = c(2:6))
Dist <- c(1:20, 35, 50, 100)
colnames(DistTab) <- Dist

for(VitTyp in 2:6){
  for(Distanz in 8:30){

  DistTab[VitTyp-1, Distanz - 7] <- mean(tSub@data[tSub@data$PNT_VIT == VitTyp, ][, Distanz])
  
  }
  
}


## Als Raster Plotten
require(raster)
summary(DistTab)
ra.DistTab <- raster(as.matrix(DistTab))
plot(ra.DistTab, main= "Nachbarschaftsbeziehungen")



df_distTab <- as.data.frame(DistTab)


## "Variogramme" plotten
#Dist <- c(5, 10, 20, 35, 50, 100, 150, 200)
DistTab_fin <- cbind(Dist, t(DistTab))
colnames(DistTab_fin) <- c("Dist", "Juvenil", "Jung_Adult", "Adult", "Senil", "Tot")
DistTab_fin <- as.data.frame(DistTab_fin)
pl <- ggplot(DistTab_fin, aes(x= Dist)) + 
  geom_point(aes(y= Juvenil), col= "blue") + geom_line(aes(y= Juvenil),col="blue") +
  geom_point(aes(y= Jung_Adult), col= "orange") + geom_line(aes(y= Jung_Adult),col="orange") +
  geom_point(aes(y= Adult), col= "yellow") + geom_line(aes(y= Adult),col="yellow") + 
  geom_point(aes(y= Senil), col= "brown") + geom_line(aes(y= Senil),col="brown") +
  geom_point(aes(y= Tot), col= "gray") + geom_line(aes(y= Tot),col="gray")  
  
require(plotly) 
ggplotly(pl)

