# 因子分析のスクリプト
dat
dat.fac <- factanal(dat[,2:10], factors = 3)

par(mfrow=c(2,2))
barplot(dat.fac$loadings[,1], col = "lightblue")
barplot(dat.fac$loadings[,2], col = "lightblue")