
install.packages("plyr")
library("plyr")

tAll <- readOGR("D:/Uni/Lech/Tamarix/Lech/GPS-Punkte/Mit Attributen/Tam/TamAllDGMDist.shp")
str(tAll)

df <- tAll@data[c("VEG_TYPE", "PNT_VIT")]

m <- data.frame(matrix(NA, nrow = 9, ncol = 5))

rownames(m) <- unique(df$VEG_TYPE)
colnames(m) <- 2:6

head(df)

for (i in rownames(m)) {
  for (j in colnames(m)) {
    h <- df$PNT_VIT[df$VEG_TYPE == i]
    g <- table(h)
    
      m[i, j] <- sum(grepl(j, h))/length(h)
    
  }
}


m.log <- logmccune(m)

ip <- isopam(m.log)
isotab(ip)
classes <- ip$flat


nmds <- metaMDS (vegdist(m.log), k = 2) 
fig <- ordiplot(nmds, type = "points", display = "sites")
points (fig, "sites", pch = 20, col = classes, cex = 2)


dca <- decorana (vegdist(m.log))
fig <- ordiplot(dca, type = "points", display = "sites")
points (fig, "sites", pch = 20, col = classes, cex = (m.log [,1] + 0.5))
title (names (m.log) [1])