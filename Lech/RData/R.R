setwd("D:/Uni/Lech/tamarix/Lech/RData")

list.files()

T <- read.csv("tges.csv", stringsAsFactors = FALSE)

## pnt.live über pnt.vit ☻

head(T)

medi <- matrix(NA, 4, 4)
colnames(medi) <- c("Class", "Q25", "Median", "Q75")
medi[, 1] <- j <-2:5

x <- cbind(T[, 20], T[, 18])[-192, ]

for (i in j) {
  medi[i-1, 3] <- median(x[x[,1] == i, 2])
  medi[i-1, 2] <- quantile(x[x[,1] == i, 2], probs = c(0.25))
  medi[i-1, 4] <- quantile(x[x[,1] == i, 2], probs = c(0.75))
}

plot(x[, 1], x[, 2], xlim = c(2, 5), #ylim = c(0.6, 1),
     xlab = "", ylab = "NDVI", main = "Group 1 (>870m)", type = "n")
polygon(c(2:5, 5:2), c(medi[, 4], rev(medi[, 2])),
        col = "lightgrey", border = NA)
points(x[, 1], x[, 2], pch = 4)
lines(2:5, medi[, 3], col = "red", lwd = 2)
lines(2:5, medi[, 2], col = "darkblue", lwd = 2)
lines(2:5, medi[, 4], col = "blue", lwd = 2)
legend(2, 250, c("Q75", "Median", "Q25"), col = c("blue", "red", "darkblue"),
       bty = "n", lwd = 2)


## mean dist n = 5 über Vitalität

medi <- matrix(NA, 5, 4)
colnames(medi) <- c("Class", "Q25", "Median", "Q75")
medi[, 1] <- j <-2:6

x <- cbind(T[, 20], T[, 22])

for (i in j) {
  medi[i-1, 3] <- median(x[x[,1] == i, 2])
  medi[i-1, 2] <- quantile(x[x[,1] == i, 2], probs = c(0.25))
  medi[i-1, 4] <- quantile(x[x[,1] == i, 2], probs = c(0.75))
}

plot(x[, 1], x[, 2], xlim = c(2, 6), ylim = c(0, 10),
     xlab = "", ylab = "NDVI", main = "Group 1 (>870m)", type = "n")
polygon(c(2:6, 6:2), c(medi[, 4], rev(medi[, 2])),
        col = "lightgrey", border = NA)
points(x[, 1], x[, 2], pch = 4)
lines(2:6, medi[, 3], col = "red", lwd = 2)
lines(2:6, medi[, 2], col = "darkblue", lwd = 2)
lines(2:6, medi[, 4], col = "blue", lwd = 2)
legend(2, 250, c("Q75", "Median", "Q25"), col = c("blue", "red", "darkblue"),
       bty = "n", lwd = 2)

