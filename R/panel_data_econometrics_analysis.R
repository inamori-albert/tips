# パネルデータ分析
# 参考:R によるパネルデータモデルの推定
# http://user.keio.ac.jp/~nagakura/R/R_panel.pdf
library(plm)

# データ読み込み
data("Grunfeld",package = "plm")
head(Grunfeld,25)

# OLS推定
result1 <- plm(inv~value+capital,data = Grunfeld,model = "pooling")
summary(result1)

# 固定効果モデルの推定をLSDV推定
result2 <- plm(inv~value+capital,data = Grunfeld,model = "within")
summary(result2)

# 個別効果µiの推定値
mu=fixef(result2)
summary(mu)

# 個別効果のその平均からの乖離
mu2=fixef(result2,type="dmean")
summary(mu2)

# 個別効果があるかどうかの F 検定
pFtest(result2,result1)

# GLS推定
result3=plm(inv~value+capital,data=Grunfeld,model="random")
summary(result3)

# ハウスマン検定:相関が0という帰無仮説を検定
phtest(result2,result3)
