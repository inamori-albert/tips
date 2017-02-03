# 共分散構造分析・構造方程式モデル(SEM)分析

dat <- read.csv("~/ch14sem.csv", header=TRUE, fileEncoding="CP932")

library(psych)        # psychパッケージ使用
describe(dat)
mardia(dat)           #マルディアの多変量歪度・尖度検定
# 多変量正規性のShapiro-Wilk検定
library(mvnormtest)   # mvnormtestパッケージ使用
mshapiro.test(t(dat))

r <- cor(dat)      # 相関行列
r
cov <- cov(dat)    # 共分散行列
cov

library(sem)  #semパッケージ使用

model <- specify.model() 
中間テスト得点 < 到達度,NA,1
期末テスト得点 < 到達度,path2,NA
小テスト得点 < 到達度,path3,NA
模擬試験1の得点 < 熟達度,NA,1
模擬試験2の得点 < 熟達度,path5,NA
模擬試験3の得点 < 熟達度,path6,NA
熟達度 < 到達度,path7,NA
中間テスト得点 <> 中間テスト得点, e1, NA
期末テスト得点 <> 期末テスト得点, e2, NA
小テスト得点 <> 小テスト得点, e3, NA
模擬試験1の得点 <> 模擬試験1の得点, e4, NA
模擬試験2の得点 <> 模擬試験2の得点, e5, NA
模擬試験3の得点 <> 模擬試験3の得点, e6, NA
熟達度 <> 熟達度, e7, NA
到達度 <> 到達度, delta, NA

library(sem)   # semパッケージ使用

ans1 <- sem(model, r, N=284)
summary(ans1)

# 標準化係数
std.coef(ans1)

# 非標準化係数
ans2 <- sem(model, cov, N=284)
summary(ans2)

# 修正指数
mod.indices(ans1)

path.diagram(ans1, "ans1", ignore.double=FALSE, edge.labels="values", digits=3, standardize=TRUE, node.font=c("Osaka", 10), rank.direction="TB", same.rank="到達度, 熟達度")
