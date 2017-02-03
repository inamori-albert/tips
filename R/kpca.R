# カーネル主成分分析
library("kernlab")

x<-as.matrix(iris[,1:4])
head(x)

par(mfrow=c(2,2))
# 主成分分析
iris.pc1 <- prcomp(x,scale=T)
plot(iris.pc1$x, col=as.integer(iris[,5]), main="主成分分析")

# カーネル主成分分析
iris.kpc1<-kpca(x,kernel="rbfdot",kpar=list(sigma=0.1),features=2)
plot(rotated(iris.kpc1),col=as.integer(iris[,5]),main="Gaussianなカーネル主成分分析")

iris.kpc3<-kpca(x,kernel="polydot",kpar=list(degree=1),features=2)
plot(rotated(iris.kpc3),col=as.integer(iris[,5]),main="多項式なカーネル主成分分析")

# 新しい値の主成分属性のあてはめ
y<-matrix(c(5.1,3.5,1.4,0.2), nrow=1,ncol=4)
colnames(y)<-c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
predict(iris.pc1,y)
iris.pc1$x[1,]