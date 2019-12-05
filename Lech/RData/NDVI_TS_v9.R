############################################################
### NDVI trends in Southern Beech forests in New Zealand ###
############################################################
# Reserching the NDVI data provided by Landsat 5, 7 and 8 to find
# potential trends.
# by Janos Siegle

library("zoo")
library("zyp")
library("vegan")

setwd("C:/Users/Janos/Desktop/BA")


####################################################################
# NDVI Landsat 5
####################################################################

######## Data Preparation ########


##Loading modified LS5 Data from Teja
LS5 <- read.csv("nz_timeseries_janos_LS5.csv", sep = ";")

##NDVI Calculation
LS5[, 16] <- as.numeric((LS5$B4 - LS5$B3) / (LS5$B4 + LS5$B3))

##remove unconfident values (NDVI > 1 & NDVI < 0.6)
LS5 <- LS5[!LS5[, 16] > 1, ]
LS5 <- LS5[!LS5[, 16] < 0.6, ]

##Filter for DDV and 0 in sr_cloud_qa
LS5 <- LS5[LS5[, 8] %in% c(0, 1), ]

##Filter for sr_atmos_opacity
LS5 <- LS5[!LS5[, 7] > 100, ] 

##Transform Dates
LS5[, 6] <- substr(LS5[, 6], 1, 10)


####################################################################
# NDVI Landsat 7
####################################################################

######## Data Preparation ########

##Loading modified LS7 Data from Teja
LS7 <- read.csv("nz_timeseries_janos_LS7.csv", sep = ";")

##NDVI Calculation
LS7[, 16] <- as.numeric((LS7$B4 - LS7$B3) / (LS7$B4 + LS7$B3))

##Filter for DDV and 0 in sr_cloud_qa
LS7 <- LS7[LS7[, 8] %in% c(0, 1), ]

##Filter for sr_atmos_opacity
LS7 <- LS7[LS7[, 7] < 100, ]

##remove unconfident values (NDVI > 1 & NDVI < 0.6)
LS7 <- LS7[!LS7[, 16] > 1, ]
LS7 <- LS7[!LS7[, 16] < 0.6, ]

##Transform Dates
LS7[, 6] <- substr(LS7[, 6], 1, 10)


####################################################################
# NDVI Landsat 8
####################################################################

######## Data Preparation ########

##Loading modified LS8 Data from Teja
LS8 <- read.csv("nz_timeseries_janos_LS8.csv", sep = ";")

##NDVI Calculation and apply Correction Coeficent (Roy et al.,2016)
LS8[, 18] <- as.numeric((LS8$B5 - LS8$B4) / (LS8$B5 + LS8$B4))
LS8[, 19] <- as.numeric(0.9589 * LS8[, 18] + 0.0029)

##Filter for clear in pixel_qa
LS8 <- LS8[LS8[, 7] %in% 322, ]

##remove unconfident values (NDVI > 1 & NDVI < 0.6)
LS8 <- LS8[!LS8[, 19] > 1, ]
LS8 <- LS8[!LS8[, 19] < 0.6, ]

##Transform Dates
LS8[, 6] <- substr(LS8[, 6], 1, 10)


####################################################################
# Combined Landsat 5, 7 & 8 for each Point
####################################################################

# Define array and the seasons
season <- c("spring", "summer", "autumn", "winter")
spring <- c("09", "10", "11")
summer <- c("12", "01", "02")
autumn <- c("03", "04", "05")
winter <- c("06", "07", "08")
arr <- array(NA, dim = c(20, 4, 59))
dimnames(arr) <- list(1999:2018, season, 1:59)
NDVI_s <- arr

# Loop to divide NDVIs by years, seasons and points
for (i in 1:59) {
  split <- LS[LS$ID == i, ]
  for (j in 1999:2018) {
    NDVI_s[dimnames(NDVI_s)[[1]] == j, "spring", i] <- 
     mean(split[substr(split$date, 1, 7)%in% paste0(j, "-", spring), 3])
    NDVI_s[dimnames(NDVI_s)[[1]] == j, "summer", i] <- 
     mean(split[substr(split$date, 1, 7)%in% paste0(j, "-", summer), 3])
    NDVI_s[dimnames(NDVI_s)[[1]] == j, "autumn", i] <- 
     mean(split[substr(split$date, 1, 7)%in% paste0(j, "-", autumn), 3])
    NDVI_s[dimnames(NDVI_s)[[1]] == j, "winter", i] <- 
     mean(split[substr(split$date, 1, 7)%in% paste0(j, "-", winter), 3])
  }
}


####################################################################
# Analysing
####################################################################

####### Calculating Mann-Kendall trend statistaics and Theil-Sen
        slope according to Yue et al. (2002) #######

# Summer
summer_N <- matrix(NA, 20, 59)
rownames(summer_N) <- 1999:2018
colnames(summer_N) <- 1:59

for (i in 1:59) summer_N[, i] <- NDVI_s[, 2, i]

yue_summer <- matrix(NA, 59, 11)
rownames(yue_summer) <- 1:59
colnames(yue_summer) <- c("lbound","trend","trendp","ubound","tau","sig",
                          "nruns","autocor","valid_frac","linear","intercept")

for (i in 1:59) yue_summer[i, ] <- zyp.yuepilon(summer_N[, i])

####### Plot NDVI ########

# Read elevation for metadata
meta <- read.csv("nz_sample_meta_janos.csv")
elev <- meta[, 6]
names(elev) <- meta[, 1]
hist(elev, breaks = 10)

## Defining Groups
g1 <- meta[meta[, 6] > 870, 1] #higher than 870 m
g2 <- meta[meta[, 6] < 870, 1] #lower than 870 m

## Form Median and Quantiles for each Group
medi_g1 <- matrix(NA, 20, 4)
colnames(medi_g1) <- c("Year", "Q25", "Median", "Q75")
medi_g1[, 1] <- 1999:2018

for (i in 1:20) {
    medi_g1[i, 3] <- median(summer_N[i, g1], na.rm = T)
    medi_g1[i, 2] <- quantile(summer_N[i, g1], probs = c(0.25), na.rm = T)
    medi_g1[i, 4] <- quantile(summer_N[i, g1], probs = c(0.75), na.rm = T)
}

medi_g2 <- matrix(NA, 20, 4)
colnames(medi_g2) <- c("Year", "Q25", "Median", "Q75")
medi_g2[, 1] <- 1999:2018

for (i in 1:20) {
    medi_g2[i, 3] <- median(summer_N[i, g2], na.rm = T)
    medi_g2[i, 2] <- quantile(summer_N[i, g2], probs = c(0.25), na.rm = T)
    medi_g2[i, 4] <- quantile(summer_N[i, g2], probs = c(0.75), na.rm = T)
}

# Plot
par(mfrow = c(1, 2))

plot(1999:2018, summer_N[, 2], xlim = c(1999, 2018), ylim = c(0.6, 1),
     xlab = "", ylab = "NDVI", main = "Group 1 (>870m)", type = "n")
polygon(c(1999:2018, 2018:1999), c(medi_g1[, 4], rev(medi_g1[, 2])),
        col = "lightgrey", border = NA)
for (i in g1) points(1999:2018, summer_N[, i])
lines(1999:2018, medi_g1[, 3], col = "red", lwd = 2)
lines(1999:2018, medi_g1[, 2], col = "darkblue", lwd = 2)
lines(1999:2018, medi_g1[, 4], col = "blue", lwd = 2)
legend(2011, 1, c("Q75", "Median", "Q25"), col = c("blue", "red", "darkblue"),
       bty = "n", lwd = 2)

plot(1999:2018, summer_N[, 1], xlim = c(1999, 2018), ylim = c(0.6, 1),
     xlab = "", ylab = "NDVI", main = "Group 2 (<870m)", type = "n")
polygon(c(1999:2018, 2018:1999), c(medi_g2[, 4], rev(medi_g2[, 2])),
        col = "lightgrey", border = NA)
for (i in g2) points(1999:2018, summer_N[, i])
lines(1999:2018, medi_g2[, 3], col = "red", lwd = 2)
lines(1999:2018, medi_g2[, 2], col = "darkblue", lwd = 2)
lines(1999:2018, medi_g2[, 4], col = "blue", lwd = 2)
legend(2011, 1, c("Q75", "Median", "Q25"), col = c("blue", "red", "darkblue"),
       bty = "n", lwd = 2)


## w\o grouping
medi <- matrix(NA, 20, 4)
colnames(medi) <- c("Year", "Q25", "Median", "Q75")
medi[, 1] <- 1999:2018

for (i in 1:20) {
    medi[i, 3] <- median(summer_N[i, g3], na.rm = T)
    medi[i, 2] <- quantile(summer_N[i, g3], probs = c(0.25), na.rm = T)
    medi[i, 4] <- quantile(summer_N[i, g3], probs = c(0.75), na.rm = T)
}

plot(1999:2018, summer_N[, 1], xlim = c(1999, 2018), ylim = c(0.6, 1),
     xlab = "", ylab = "NDVI", main = "Landsat 5, 7 & 8 Data", type = "n")
polygon(c(1999:2018, 2018:1999), c(medi[, 4], rev(medi[, 2])),
        col = "lightgrey", border = NA)
for (i in 1:59) points(1999:2018, summer_N[, i])
lines(1999:2018, medi[, 3], col = "red", lwd = 2)
lines(1999:2018, medi[, 2], col = "darkblue", lwd = 2)
lines(1999:2018, medi[, 4], col = "blue", lwd = 2)
legend(2011, 1, c("Q75", "Median", "Q25"), col = c("blue", "red", "darkblue"),
       bty = "n", lwd = 2)



## Influence of exposition and slope
exp <- read.csv("Exposition.csv", sep = ";")
hist(exp[, 4]) #exposition
hist(exp[, 5]) #slope
exp_col <- rep(NA, 59)

for (i in 1:59) {
if (exp[i, 4] > 135 & exp[i, 4] < 225 & exp[i, 5] > 10) exp_col[i] <- "blue"
else exp_col[i] <- "red"
}

plot(yue_summer[, 2] ~ elev, col = exp_col)
plot(lm(yue_summer[, 2] ~ elev), col = exp_col)

teststat <- mrpp(yue_summer[, 2], exp_col)


## searching for a threshold
th <- matrix(NA, 31, 3)
th[, 1] <- seq(750, 1050, 10)

for (i in seq(750, 1050, 10)) {
    index <- elev >= i
    th[th[, 1] == i, 2] <- mrpp(yue_summer[, 2], index)[[7]]
    th[th[, 1] == i, 3] <- mrpp(yue_summer[, 2], index)[[8]]
}

par(mfrow = c(1, 2))
plot(th[, 2]~th[, 1])
plot(th[, 3]~th[, 1])

index <- elev > 870
perm <- mrpp(yue_summer[, 2], index)


####### Plotting Exemples w\ Linear Regression #######
## Plot 24 & 44
jpeg("ExPlot_24_44.jpeg", quality = 100, width = 1200, height = 600)
par(mfrow = c(1, 2))

plot(summer_N[, 24]~rownames(summer_N), lwd = 2, ylim = c(0.66, 0.93),
     ylab = "NDVI", xlab = "Jahr", main = "Stichprobe 24", cex = 1.3,
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
abline(lm(summer_N[, 24]~as.numeric(rownames(summer_N))), lwd = 2)

plot(summer_N[, 44]~rownames(summer_N), lwd = 2, ylim = c(0.66, 0.93),
     ylab = "NDVI", xlab = "Jahr", main = "Stichprobe 44", cex = 1.3,
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
abline(lm(summer_N[, 44]~as.numeric(rownames(summer_N))), lwd = 2)

dev.off()

## Plot 28 & 19
jpeg("ExPlot_28_19.jpeg", quality = 100, width = 1200, height = 600)
par(mfrow = c(1, 2))

plot(summer_N[, 28]~rownames(summer_N), lwd = 2, ylim = c(0.66, 0.93),
     ylab = "NDVI", xlab = "Jahr", main = "Stichprobe 28", cex = 1.3,
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
abline(lm(summer_N[, 28]~as.numeric(rownames(summer_N))), lwd = 2)
abline(lm(summer_N[1:12, 28]~as.numeric(rownames(summer_N[1:12, ]))),
       col = "darkgrey", lwd = 2)
legend(1999, 0.7, c("Lineare Regression bis 2010",
                    "Lineare Regression bis 2018"),
       col = c("darkgrey", "black"), bty = "n", lwd = 2, cex = 1.5)

plot(summer_N[, 19]~rownames(summer_N), lwd = 2, ylim = c(0.66, 0.93),
     ylab = "NDVI", xlab = "Jahr", main = "Stichprobe 19", cex = 1.3,
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
abline(lm(summer_N[, 19]~as.numeric(rownames(summer_N))), lwd = 2)

dev.off()












plot(elev[-c(19,28)], yue_summer[-c(19,28), 2], pch = 20, col = "chocolate4",
     xlab = "elevation", ylab = "Theil-Sen-Slope", cex = 1.2)
plot(lm(yue_summer[-c(19,28), 2]~elev[-c(19,28)]), col = "chocolate4", lwd = 2)
points(elev[-28], yue_spring[-28, 2], pch = 20, col = "aquamarine4", cex = 1.2)
abline(lm(yue_spring[-c(19,28), 2]~elev[-c(19,28)]), col = "aquamarine4", lwd = 2)
legend("topright", pch = 20, c("summer", "spring"), col = c("chocolate4", "aquamarine4"), bty = "n")








