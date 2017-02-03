# 分類木で分析
library(rpart)
library(rpart.plot)
library(partykit)

ct <- rpart(f ~. , data = dat_training, method = "class")
print(ct)

par(xpd = NA)
plot(ct, branch = 0.8, margin = 0.05)
text(ct, use.n = TRUE, all = TRUE)

rpart.plot(ct, type = 1, uniform = TRUE, extra = 1, under = 1, faclen = 0)

plot(as.party(ct))

printcp(ct)
plotcp(ct)

ct2 <- rpart(y_5_count_12_f ~. , data = dat_training, method = "class", cp = 0.030628)
print(ct2)
plot(as.party(ct2))
