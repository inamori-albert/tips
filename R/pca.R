# 主成分分析

dat
dat$type <- as.factor(dat$type)

prc <- prcomp(dat[c(3:ncol(dat))],scale=T)
fc.l <- sweep(prc$rotation, MARGIN=2, prc$sdev, FUN="*")

plot(fc.l[,1], type = "n", col=color, ylim=range(fc.l), main=paste0("PC",as.character(1)))
text(fc.l[,1], labels =subject, col=color, ylim=range(fc.l), main=paste0("PC",as.character(1)))
abline(h = 0)

plot(fc.l[,2], type = "n", col=color, ylim=range(fc.l), main=paste0("PC",as.character(2)))
text(fc.l[,2], labels =subject, col=color, ylim=range(fc.l), main=paste0("PC",as.character(2)))
abline(h = 0)

plot(fc.l[,1], fc.l[,2], type="n", xlim=c(-1,1), ylim=c(-1,1))
text(fc.l[,1], fc.l[,2], labels =subject, col=color, xlim=c(-1,1), ylim=c(-1,1))
abline(h = 0)
abline(v = 0)

biplot(prc)

# カイゼル基準:主成分得点の分散の平均は「1」
screeplot(prc, main="Screeplot for Variance (with scaled data)")
abline(h=1, lty=2)
