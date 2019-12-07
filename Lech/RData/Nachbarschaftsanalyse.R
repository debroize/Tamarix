# Nachbarschaftsanalyse ####

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
boxplot(tSub@data$mean5 ~ tSub@data$PNT_VIT, ylim= c(0, 15))

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
plot(imgVitTab)
