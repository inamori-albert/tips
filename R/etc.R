data("iris")
str(iris)

# 要約統計量
summary(iris)
mean(iris$Sepal.Length)
median(iris$Sepal.Length)
min(iris$Sepal.Length)
max(iris$Sepal.Length)
quantile(iris$Sepal.Length)
IQR(iris$Sepal.Length)
range(iris$Sepal.Length)
names(which.max(table(iris$Species))) # 最頻値(mod)
prod(iris$Sepal.Length)^(1/length(iris$Sepal.Length)) # 幾何平均
stats::filter(iris$Sepal.Length,rep(1,3))/3 # 移動平均(3 period)

# 相関をプロット
plot(iris$Sepal.Length, iris$Sepal.Width)
pairs(iris[1:5])

plot(iris[,3],iris[,4])

# 相関係数を計算
# "pearson" ： ピアソンの積率相関係数の無相関検定を行う．
# "kendall" ： ケンドールの順位相関係数の無相関検定を行う．
# "spearman" ： スピアマンの順位相関係数の無相関検定を行う
cor(iris[1:4], method = "p")
str(iris)

# Factor型を数字型に
my_iris <- iris
my_iris[,5] <- as.numeric(my_iris[,5])

# 相関係数を計算
cor(my_iris)

# Factorで1種類に絞り、相関をプロット
pairs(iris[iris$Species == 'setosa',])

# 線形モデルで回帰(正規分布のglm)
fit = glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris, family = gaussian)

plot(iris$Sepal.Width, iris$Sepal.Length)
lines(fit$fitted.values)

fit = glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris, family = gaussian)
summary(fit)

# 相関係数の強すぎる説明変数は多重共線性が起きる可能性があるので、説明変数から除外
cor(my_iris[2:5])
fit = glm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, family = gaussian)
summary(fit)

# aicで自動的にモデルを決定(説明変数を除外するか自動判定)
fit = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)
my_step <- step(fit)

# 分類木
my_tree <- tree::tree(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = my_iris)
plot(my_tree)
text(my_tree)

iris.label<-c("S","C","V")[my_iris[,5]]
my_tree <- tree::tree(Species~., data = my_iris)
plot(my_iris[,3],my_iris[,4],type="n")
text(iris[,3],iris[,4],labels = iris.label)
tree::partition.tree(my_tree,add=T,col=2,cex=1.5)

# 木の剪定
my_tree2 <- tree::prune.tree(my_tree,best=10)
plot(my_tree2)
text(my_tree2)

my_tree <- tree::tree(Sepal.Length ~ Sepal.Width + Species, data = my_iris)
plot(my_tree)
text(my_tree)

# 決定木
library(rpart)
df <- iris
# data.rp <- rpart(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=df)
data.rp <- rpart(Sepal.Length~Sepal.Width+Species,data=df)
print(data.rp)
plot(data.rp,branch=0.6,margin=0.05)
text(data.rp,use.n=T,all=T,uniform=T)

prune(data.rp,cp=0.02)
plot(data.rp,branch=0.6,margin=0.05)
text(data.rp,use.n=T,all=T,uniform=T)

# 決定木2
library(rpart)
library(partykit)
library(epitools)

data_titanic<-expand.table(Titanic)
result<-rpart(Survived~., data = data_titanic)
plot(as.party(result))

# パレート図を作った
z <- table(cut(iris$Petal.Length, breaks = seq(3,8,0.2)))
qcc::pareto.chart(z)

# 種別で色分け
with(iris, plot(Petal.Length, Species, col=rainbow(7)[Species]))
with(iris, plot(Petal.Length, Sepal.Length, col=rainbow(7)[Species]))

# 主成分分析
my_prc <- prcomp(my_iris,scale=T)
summary(my_prc)
print(round(my_prc$rotation,digits = 3))
fc.l <- sweep(my_prc$rotation, MARGIN=2, my_prc$sdev, FUN="*")
subject <- c("l","w","L","W","S")
plot(fc.l[,1], pch=subject, ylim=range(fc.l), main="PC1")
plot(fc.l[,2], pch=subject, ylim=range(fc.l), main="PC2")
plot(fc.l[,1], fc.l[,2], pch=subject,xlim=c(-1,1), ylim=c(-1,1))
biplot(my_prc)

# 帯グラフ(積み上げ)
library(ggplot2)
for(i in 1:nrow(iris)){
  iris$rand[i] = c("hoge","hogehoge")[sample(1:2, 1, replace=TRUE)]
  print(iris[i,])
}
g <- ggplot2::ggplot(
  iris,
  aes (
    x = rand,             # 遺伝子別でグルーピング
    y = Sepal.Length,
    fill = Species       # 縦軸を生物種で fill up 
  )
)
g <- g + geom_bar(stat = "identity")
plot(g)

# 帯グラフ(割合)
g <- ggplot(
  iris,
  aes (
    x = rand,             # 遺伝子別でグルーピング
    y = Sepal.Length,
    fill = Species       # 縦軸を生物種で fill up 
  )
)
g <- g + geom_bar(stat = "identity", position = "fill")
plot(g)

# クロス集計表
table(iris$rand, iris$Sepal.Length)
table(iris$rand, iris$Species)
addmargins(table(iris$rand, iris$Species)) # 合計も付けてみた
addmargins(table(iris$rand, iris$Species), FUN = ) # 合計も付けてみた

# ヒートマップ(並び変えあり)
heatmap(table(iris$rand, iris$Species))
heatmap(
  table(iris$Sepal.Width, iris$Sepal.Length),
  main = "Sepal.width Vs Sepal.Length",
  Rowv = T,
  Colv = T,
  distfun = dist,
  hclustfun = hclust,
  color = heat.colors(256)
)

# ヒートマップ(並び変えなし)
heatmap(
  table(iris$Sepal.Width, iris$Sepal.Length),
  main = "Sepal.width Vs Sepal.Length",
  Rowv = NA,
  Colv = NA,
  color = heat.colors(256)
)

# 時系列データ
ts.plot(ldeaths,mdeaths,fdeaths,col=c(1,2,3))
# 単位根検定→両方とも単位根なので、一回微分しないと見せかけの回帰が生じる
library(tseries)
adf.test(ldeaths)
adf.test(mdeaths)

# 自己相関・偏自己相関を確認
library(forecast)
tsdisplay(ldeaths)
tsdisplay(mdeaths)

# arimaモデルで予測
my_arima <- auto.arima(ldeaths, max.p = 10, max.q = 10, max.order = 30, stepwise=F, trace=T, seasonal = F)
plot(forecast(my_arima, h = 30))

library("vars")
data("Canada")
# 単位根検定→単位根でないので、そのままVAR使ってOK
adf.test(Canada[,1])
adf.test(Canada[,2])
adf.test(Canada[,3])
adf.test(Canada[,4])

# 季節成分分解
plot(stl(ldeaths, s.window = "periodic"))

# 複数の系列でvarモデルを使って予測
library(vars)
my_var = VAR(Canada,p=VARselect(Canada)$selection[1])
summary(my_var)
plot(forecast(my_var, h = 10))

# dplyr
# 絞り込み
iris.virginica.hoge <- iris %>% dplyr::filter(Species == 'virginica') %>% dplyr::filter(rand == 'hoge')

# 集計
iris.sum <- iris %>% dplyr::summarise(max=max(Sepal.Length),min=min(Sepal.Length),mean=mean(Sepal.Length))

# Group by
iris.grouping.sum <- iris %>% dplyr::group_by(Species) %>% dplyr::summarise(max=max(Sepal.Length),min=min(Sepal.Length),mean=mean(Sepal.Length))

# テキストマイニング
res <- RMeCabC("すもももももももものうち")
unlist(res)
res2 <- unlist(res)
res2[names(res2) == "名詞"]
length(res2[names(res2) == "名詞"])