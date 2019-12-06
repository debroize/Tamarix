### Test

#install.packages("geosphere")

require(raster)
require(rgdal)
require(geosphere)

# Daten einlesen ####

### GPS-Punkte mit DGM und Dist
tAll <- readOGR("./Lech/GPS-Punkte/Mit Attributen/Tam/TamAllDGMDist.shp")

## Datentypen anpassen
for (i in 2:22) {
  tAll@data[, i] <- as.character(tAll@data[, i])
}
for (j in c(3, 5:22)) {
  tAll@data[, j] <- as.numeric(tAll@data[, j])
}


# Datensatz auf Vitalitätstypen aufteilen ####
tSub <- tAll
tSub@data <- tAll@data[c('NAME', 'VEG_TYPE', 'PNT_VIT', 'MORPHDYN', 'DGM', 'Dist')]

tVit2 <- tSub@data[tSub@data$PNT_VIT == 2, ]
tVit3 <- tSub@data[tSub@data$PNT_VIT == 3, ]
tVit4 <- tSub@data[tSub@data$PNT_VIT == 4, ]
tVit5 <- tSub@data[tSub@data$PNT_VIT == 5, ]
tVit6 <- tSub@data[tSub@data$PNT_VIT == 6, ]


summary(tVit2)
str(tSub@data)

# Analyse ob sich die Diatanzen der VitTypen unterscheiden ####
## Bedingungen Prüfen
### Histogramme
par(mfrow= c(2,3))
hist(tSub@data$Dist, main = "", xlab = "Alle", col = "darkblue", xlim = c(140, 340), breaks = seq(140, 340, by= 20))
hist(tVit2$Dist, main = "Flussabstand", xlab = "Juvenil", col = "blue", xlim = c(140, 340), breaks = seq(140, 340, by= 20))
hist(tVit3$Dist, main = "", xlab = "Jung Adult", col = "blue", xlim = c(140, 340), breaks = seq(140, 340, by= 20))
hist(tVit4$Dist, main = "", xlab = "Adult", col = "blue", xlim = c(140, 340), breaks = seq(140, 340, by= 20))
hist(tVit5$Dist, main = "", xlab = "Senil", col = "blue", xlim = c(140, 340), breaks = seq(140, 340, by= 20))
hist(tVit6$Dist, main = "", xlab = "Tot", col = "blue", xlim = c(140, 340), breaks = seq(140, 340, by= 20))
### Test auf Normalverteilung
shapiro.test(tSub@data$Dist) # nicht normalverteilt
### Test auf Homogenität der Varianzen
fligner.test(tSub@data$Dist ~ tSub@data$Pnt.Vit) # Varianzen sind verschieden

# varsDist <- c(var(tVit2$Dist), var(tVit3$Dist), var(tVit4$Dist), var(tVit5$Dist), var(tVit6$Dist))




# T-Tests auf mittlere Flussdistanz ####

t.test(tVit5$Dist, tVit6$Dist)
## signifikante Unterschiede in der Flussdistanz-Verteilung zwischen:
# juvenil (2) und adult (4)
# juvenil (2) und senil (5)
# jung adult (3) und senil (5)
# senil (5) und tot (6)




# Analyse ob sich die Höhe der VitTypen unterscheiden ####
## Bedingungen Prüfen
### Histogramme
par(mfrow= c(2,3))
hist(tSub@data$DGM, main = "", xlab = "Alle", col = "brown", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
hist(tVit2$DGM, main = "DGM", xlab = "Juvenil", col = "orange", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
hist(tVit3$DGM, main = "", xlab = "Jung Adult", col = "orange", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
hist(tVit4$DGM, main = "", xlab = "Adult", col = "orange", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
hist(tVit5$DGM, main = "", xlab = "Senil", col = "orange", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
hist(tVit6$DGM, main = "", xlab = "Tot", col = "orange", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
### Test auf Normalverteilung
shapiro.test(tSub@data$DGM) # nicht normalverteilt
### Test auf Homogenität der Varianzen
fligner.test(tSub@data$DGM ~ tSub@data$Pnt.Vit) # Varianzen sind verschieden

# varsDGM <- c(var(tVit2$DGM), var(tVit3$DGM), var(tVit4$DGM), var(tVit5$DGM), var(tVit6$DGM))


## ANOVA
aov.data <- aov(tSub@data$DGM ~ tSub@data$Pnt.Vit) # signifikant => mindestens ein VitTyp unterscheidet sich von den anderen

TukeyHSD(aov.data)
## signifikante Unterschiede in der Höhen-Verteilung zwischen:
# juvenil (2) und tot (6)
# jung adult (3) und senil (5)
# jung adult (3) und tot (6)

t.test(tVit2$DGM, tVit5$DGM)
## signifikante Unterschiede in der Höhen-Verteilung zwischen:
# juvenil (2) und senil (5)
# juvenil (2) und tot (6)
# jung adult (3) und adult (4)
# jung adult (3) und senil (5)
# jung adult (3) und tot (6)
# adult (4) und tot (6)

boxplot(tSub@data$DGM ~ tSub@data$Pnt.Vit)
boxplot(tSub@data$Dist ~ tSub@data$Pnt.Vit)





# Distanzmatrix einlesen ####
dismat <- readOGR("./Lech/shapes/Distanzmatrix_Klassisch.shp")

df_dismat <- dismat@data[-1]




## Nähestens 5 Punkte filtern ####

nearest5 <- data.frame()
for(i in 1:nrow(df_dismat)){
  nearest5[i, 1:5] <- sort(df_dismat[i,])[2:6]
  nearest5[i, 6:10] <- as.numeric(substr(colnames(sort(df_dismat[i,])[2:6]), start = 2, stop = 5))
}

### Spalten und Reihen beschriften
colnames(nearest5) <- c('Dnr1', 'Dnr2', 'Dnr3', 'Dnr4', 'Dnr5', 'Pnr1', 'Pnr2', 'Pnr3', 'Pnr4', 'Pnr5')
rownames(nearest5) <- dismat@data[[1]]

head(nearest5, 5)

## Punktnummer durch VitTyp ersetzt
for(i in tSub@data$name){
  nearest5[,6:10][nearest5[,6:10] == i] <- tSub@data$Pnt.Vit[tSub@data$name == i]
  
}




# Nächste Nachbar Analyse, Mittlere-Distanzen #####
## NN-DataFrame mit Datensatz verbinden
tSub@data <- cbind(tSub@data, nearest5)
head(tSub@data, 4)

tSub@data$mean3 <- rowMeans(tSub@data[c('Dnr1', 'Dnr2', 'Dnr3')])
tSub@data$mean5 <- rowMeans(tSub@data[c('Dnr1', 'Dnr2', 'Dnr3', 'Dnr4', 'Dnr5')])


## ANOVA2
boxplot(tSub@data$mean3 ~ tSub@data$Pnt.Vit, ylim= c(0, 15))
boxplot(tSub@data$mean5 ~ tSub@data$Pnt.Vit, ylim= c(0, 15))

### Test auf Normalverteilung
shapiro.test(tSub@data$mean3) # nicht normalverteilt
shapiro.test(tSub@data$mean5) # nicht normalverteilt
### Test auf Homogenität der Varianzen
fligner.test(tSub@data$mean3 ~ tSub@data$Pnt.Vit) # Varianzen sind verschieden
fligner.test(tSub@data$mean3 ~ tSub@data$Pnt.Vit) # Varianzen sind verschieden
# Keine ANOVA zulässig

## T-Tests
t.test(tSub@data$mean3[tSub@data$Pnt.Vit == 5], tSub@data$mean3[tSub@data$Pnt.Vit == 6])
## keie Unterschiede der Mittelwerte


# Nächste Nachbar Analyse, benachbarte VitTypen #####
nbVitTab <- data.frame(matrix(NA, nrow = 5, ncol = 5), row.names = c(2:6))
colnames(nbVitTab) <- c(2:6)
for(ownVitTyp in 2:6){
  for(nbrVitTyp in 2:6){
    
    nbVitTab[ownVitTyp-1, nbrVitTyp-1] <- 
      sum(tSub@data[tSub@data$Pnt.Vit == ownVitTyp,][c('Pnr1', 'Pnr2', 'Pnr3', 'Pnr4', 'Pnr5')] == nbrVitTyp )
    nbVitTab[ownVitTyp-1, nbrVitTyp-1] <- 
      nbVitTab[ownVitTyp-1, nbrVitTyp-1]/
      (nrow(tSub@data[tSub@data$Pnt.Vit == ownVitTyp,]) * nrow(tSub@data[tSub@data$Pnt.Vit == nbrVitTyp,]))
  }
}
nbVitTab

## normalisieren
nbVitTab[1,1]
nrow(tSub@data[tSub@data$Pnt.Vit == ownVitTyp,])


summary(nbVitTab)
imgVitTab <- raster(as.matrix(nbVitTab))
plot(imgVitTab)
