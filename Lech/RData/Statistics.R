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

# Höhen anpassen (Simulierung des Grundwasserabstands)
# Tamarisken aus  Transekt 4 werden um 0.1m angehoben
# Tamarisken aus Transekt 5 werden um 0.3m angehoben

tAll@data$DGMmod[tAll@data$Trans == 2] <- tAll@data$DGM[tAll@data$Trans == 2]
tAll@data$DGMmod[tAll@data$Trans == 3] <- tAll@data$DGM[tAll@data$Trans == 3]
tAll@data$DGMmod[tAll@data$Trans == 4] <- tAll@data$DGM[tAll@data$Trans == 4] + 0.1
tAll@data$DGMmod[tAll@data$Trans == 5] <- tAll@data$DGM[tAll@data$Trans == 5] + 0.3

# Datensatz auf Vitalitätstypen aufteilen ####
tSub <- tAll
tSub@data <- tAll@data[c('NAME', 'VEG_TYPE', 'PNT_VIT', 'MORPHDYN', 'DGM', 'DGMmod', 'Dist', 'Trans')]

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

# Boxplot
boxplot(tSub@data$Dist ~ tSub@data$PNT_VIT, xlab = "Vitalität", main = "Flussabstand")

### Test auf Normalverteilung
shapiro.test(tSub@data$Dist) # nicht normalverteilt
### Test auf Homogenität der Varianzen
fligner.test(tSub@data$Dist ~ tSub@data$PNT_VIT) # Varianzen sind verschieden

# varsDist <- c(var(tVit2$Dist), var(tVit3$Dist), var(tVit4$Dist), var(tVit5$Dist), var(tVit6$Dist))




################### T-Test und ANOVA dürfen eigentlich nicht verwendet werden weil die Bedingungen für Normalverteilung und Varianzengleichheit nicht erfüllt sind
# T-Tests auf mittlere Flussdistanz ####
t.test(tVit5$Dist, tVit6$Dist)
## signifikante Unterschiede in der Flussdistanz-Verteilung zwischen:
# juvenil (2) und adult (4)
# juvenil (2) und senil (5)
# jung adult (3) und senil (5)
# senil (5) und tot (6)
################################ T-Test und ANOVA dürfen eigentlich nicht verwendet werden




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

# Boxplot
boxplot(tSub@data$DGM ~ tSub@data$PNT_VIT, xlab = "Vitalität", main = "Höhe")

### Test auf Normalverteilung
shapiro.test(tSub@data$DGM) # nicht normalverteilt
### Test auf Homogenität der Varianzen
fligner.test(tSub@data$DGM ~ tSub@data$PNT_VIT) # Varianzen sind verschieden

# varsDGM <- c(var(tVit2$DGM), var(tVit3$DGM), var(tVit4$DGM), var(tVit5$DGM), var(tVit6$DGM))




################### T-Test und ANOVA dürfen eigentlich nicht verwendet werden weil die Bedingungen für Normalverteilung und Varianzengleichheit nicht erfüllt sind
## ANOVA
aov.data <- aov(tSub@data$DGM ~ tSub@data$PNT_VIT) # signifikant => mindestens ein VitTyp unterscheidet sich von den anderen

TukeyHSD(aov.data)
## signifikante Unterschiede in der Höhen-Verteilung zwischen:
# juvenil (2) und tot (6)
# jung adult (3) und senil (5)
# jung adult (3) und tot (6)

# T-Tests auf mittlere Höhe ####
t.test(tVit6$DGM, tVit4$DGM)
## signifikante Unterschiede in der Höhen-Verteilung zwischen:
# juvenil (2) und senil (5)
# juvenil (2) und tot (6)
# jung adult (3) und adult (4)
# jung adult (3) und senil (5)
# jung adult (3) und tot (6)
# adult (4) und tot (6)
################################ T-Test und ANOVA dürfen eigentlich nicht verwendet werden





# 2te Analyse ob sich die Höhe der VitTypen unterscheiden, mit angepassten Höhen ####

### Histogramme
par(mfrow= c(2,3))
hist(tSub@data$DGMmod, main = "", xlab = "Alle", col = "brown", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
hist(tVit2$DGMmod, main = "DGM-Modifiziert", xlab = "Juvenil", col = "orange", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
hist(tVit3$DGMmod, main = "", xlab = "Jung Adult", col = "orange", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
hist(tVit4$DGMmod, main = "", xlab = "Adult", col = "orange", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
hist(tVit5$DGMmod, main = "", xlab = "Senil", col = "orange", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))
hist(tVit6$DGMmod, main = "", xlab = "Tot", col = "orange", xlim = c(880, 884), breaks = seq(880, 884, by= 0.5))

# Boxplot
boxplot(tSub@data$DGMmod ~ tSub@data$PNT_VIT, xlab = "Vitalität", main = "Höhe")

### Test auf Normalverteilung
shapiro.test(tSub@data$DGMmod) # nicht normalverteilt
### Test auf Homogenität der Varianzen
fligner.test(tSub@data$DGMmod ~ tSub@data$PNT_VIT) # Varianzen sind verschieden
# keine Unterschiede zu den nicht modifizierten Daten



# Vergleich der Boxplots von realer und modifizierter Höhe
par(mfrow= c(1,2))
boxplot(tSub@data$DGM ~ tSub@data$PNT_VIT, xlab = "Vitalität", ylab = "Höhe", ylim = c(880.5, 884))
boxplot(tSub@data$DGMmod ~ tSub@data$PNT_VIT, xlab = "Vitalität", ylab = "Höhe",  ylim = c(880.5, 884))

################### T-Test und ANOVA dürfen eigentlich nicht verwendet werden weil die Bedingungen für Normalverteilung und Varianzengleichheit nicht erfüllt sind
# T-Tests auf mittlere Höhe ####
t.test(tVit6$DGMmod, tVit5$DGMmod)
## signifikante Unterschiede in der Höhen-Verteilung zwischen:
# juvenil (2) und senil (5)
# juvenil (2) und tot (6)
# jung adult (3) und adult (4)
# jung adult (3) und senil (5)
# jung adult (3) und tot (6)
# adult (4) und tot (6)
## keine Unterschiede in der signifikanz bei Verwendung der modifizierten Daten

## ANOVA
aov.data <- aov(tSub@data$DGMmod ~ factor(tSub@data$PNT_VIT)) # signifikant => mindestens ein VitTyp unterscheidet sich von den anderen
summary(aov.data)

TukeyHSD(aov.data)
## signifikante Unterschiede in der Höhen-Verteilung zwischen:
# juvenil (2) und tot (6)
# jung adult (3) und senil (5)
# jung adult (3) und tot (6)
# keine unterschiede im Tuckey Test im Vergleich zu den nicht modifizierten Daten
################################ T-Test und ANOVA dürfen eigentlich nicht verwendet werden

# Kruskal-Wallis Test (Pendant zu ANOVA wenn keine Varianzengleichheit und Normalverteilung vorliegen)
KWorig <- kruskal.test(tSub@data$DGM ~ factor(tSub@data$PNT_VIT))
KWmod <- kruskal.test(tSub@data$DGMmod ~ factor(tSub@data$PNT_VIT))
KWorig  
KWmod 
# Der Kruskal-Wallis Test ist signifikant, mindestens eine Gruppe unterscheidet sich daher von den anderen durch einen anderen Mittelwert

# Paarweiser Wilcox Test um die relationen zwischen den Gruppen zu ermitteln
WTorig <- pairwise.wilcox.test(tSub@data$DGM,factor(tSub@data$PNT_VIT),p.adj='bonferroni',exact=F)
WTmod <- pairwise.wilcox.test(tSub@data$DGMmod,factor(tSub@data$PNT_VIT),p.adj='bonferroni',exact=F)
WTorig
WTmod
# zwischen den realen und modifizierten daten lässt sich feststellen, dass die Ergebnisse der modifizierten Höhe signifikanter sind
# Im Vergleich zu ANOVA und T-Test kann gezeigt werden, dass mehr Gruppenpaare signifikante Unterschiede Zeigen (adult und tot jetzt signifikant verschieden)
## signifikante Unterschiede in der Höhen-Verteilung zwischen:
# juvenil (2) und tot (6)
# jung adult (3) und senil (5)
# jung adult (3) und tot (6)
# adult (4) und tot (6)



# Kruskal-Walis und Wilcox Test für Flussdistanz
# Kruskal-Wallis Test (Pendant zu ANOVA wenn keine Varianzengleichheit und Normalverteilung vorliegen)
KWdist <- kruskal.test(tSub@data$Dist ~ factor(tSub@data$PNT_VIT))
KWdist
# Der Kruskal-Wallis Test ist signifikant, mindestens eine Gruppe unterscheidet sich daher von den anderen durch einen anderen Mittelwert

# Paarweiser Wilcox Test um die relationen zwischen den Gruppen zu ermitteln
WTdist <- pairwise.wilcox.test(tSub@data$DGM,factor(tSub@data$PNT_VIT),p.adj='bonferroni',exact=F)
WTdist
# Im Vergleich zu ANOVA und T-Test kann gezeigt werden, dass andere Gruppenpaare signifikante Unterschiede Zeigen (adult und tot jetzt signifikant verschieden)
## signifikante Unterschiede in der Flussdistanz-Verteilung zwischen:
# juvenil (2) und tot (6)
# jung adult (3) und senil (5)
# jung adult (3) und tot (6)
# adult (4) und tot (6)
  par(mfrow= c(1,1))
boxplot(tSub@data$Dist ~ tSub@data$PNT_VIT, xlab = "Vitalität", ylab = "Flussabstand")















# Statistiken für VegVit und Kiefernhöhe, Standorttyp und Morphodynamik
tSub2 <- tAll
tSub2@data <- tAll@data[c('NAME', 'VEG_TYPE', 'PNT_VIT', 'AGE_PIN', 'MORPHDYN', 'Trans')]

par(mfrow= c(1,2))
boxplot(tSub2@data$AGE_PIN ~ tSub2@data$PNT_VIT, xlab = "Vitalität", ylab = "Standortalter", ylim = c(2,20))
VegType_ordered <- factor(tSub2@data$VEG_TYPE, ordered = TRUE, 
                          levels = c("Pionier", "Weidengebüsch", "Erlen-Weidengebüsch", "Grauerlengebüsch", "Kiefern-Erlen-Weidengebüsch", "Erlen-Kiefergebüsch", "Erlenwald", "Kiefergebüsch", "Kiefernwald"))
boxplot(tSub2@data$PNT_VIT ~ as.numeric(VegType_ordered), xlab = "Vitalität", ylab = "Standorttyp")


par(mfrow= c(1,1))
boxplot(tSub2@data$MORPHDYN ~ tSub2@data$PNT_VIT, xlab = "Vitalität", ylab = "Morphodynamik")



# Kruskal-Walis und Wilcox Test für Standortalter
# Kruskal-Wallis Test (Pendant zu ANOVA wenn keine Varianzengleichheit und Normalverteilung vorliegen)
KWalt <- kruskal.test(tSub2@data$AGE_PIN ~ factor(tSub2@data$PNT_VIT))
KWalt
# Der Kruskal-Wallis Test ist signifikant, mindestens eine Gruppe unterscheidet sich daher von den anderen durch einen anderen Mittelwert

# Paarweiser Wilcox Test um die relationen zwischen den Gruppen zu ermitteln
WTdist <- pairwise.wilcox.test(tSub2@data$AGE_PIN,factor(tSub2@data$PNT_VIT),p.adj='bonferroni',exact=F)
WTdist
## signifikante Unterschiede der Vitalitätstypen-Verteilungs auf die Standorte zwischen:
# juvenil (2) und adult (4)
# juvenil (2) und senil (5)
# juvenil (2) und tot (6)
# jung adult (3) und senil (5)






