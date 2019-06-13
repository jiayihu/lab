rm(list = ls())
library(rattle.data)
dim(wine)
attach(wine)
pairs(wine, col = Type, upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c("tipo 1", "tipo 2", "tipo 3"), pch = 16, col = c("black", "red", "green"))
pr = prcomp(wine[, -1], scale = TRUE)
names(pr)
pr
biplot(pr, scale = 0, cex = 0.5)
plot(pr$x[, 1:2], col = Type)
legend('bottomleft', pch=c(1,1,1), col=c(1,2,3), legend=c('Type 1', 'Type 2', 'Type 3'), bty='n')
variance = pr$sdev^2
prop.variance = variance / sum(variance)
prop.variance
par(mfrow = c(1, 2))
plot(prop.variance, xlab = "PCs", ylab = "Proportion of explained variance", type = "l", cex.lab = 0.7)
plot(cumsum(prop.variance), clab = "PCs", ylab = "Cumulative proportion of explained variance", type = "l", cex.lab = 0.7)
points(2, cumsum(prop.variance)[2], col = 2, pch = 16)
