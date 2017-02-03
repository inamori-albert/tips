# balanced random forestで分類
library("data.table")
library("dplyr")
require("randomForest")
library("forestFloor")

dat_origin <- fread('input.csv', header = T)

dat <- dplyr::sample_n(tbl=dat_origin, size=200000)

nrow(dat %>% dplyr::filter(a <= 0))
nrow(dat %>% dplyr::filter(a >= 1))

dat_origin <- dplyr::union((dat %>% dplyr::filter(a <= 0) %>% dplyr::sample_n(size=10000)), 
                                  (dat %>% dplyr::filter(a >= 1) %>% dplyr::sample_n(size=10000)))

# 2値分類
# データロード
dat  <- dat_origin[,c(1:11)]

# 正解ラベルの加工
dat$a_f <- as.character(dat_origin$a)
dat$a_f <- ifelse(as.integer(dat$a_f)==0, "0", dat$a_f)
dat$a_f <- ifelse(as.integer(dat$a_f)>=1, "1", dat$a_f)
dat$a_f <- as.factor(dat$a_f)

str(dat$a_f)
table(dat$a_f)

# 正解データと訓練データに分離
rowdata<-nrow(dat)
random_ids<-sample(rowdata,rowdata*0.5)
dat_training<-dat[random_ids, ]
dat_predicting<-dat[-random_ids, ]

# ランダムフォレストで分類
features<-dat_training[,1:9]
labels<-dat_training[,10]
table(dat_training[,37])

# 学習
forest <- randomForest(x=features,y=labels,importance=T, sampsize = 2000)
print(forest)
print(importance(forest))

# チューニング
forest.tune <- tuneRF(x=features,y=labels,doBest = T, sampsize = 2000)
print(forest.tune)

pred.forest <- predict(forest, newdata=dat_predicting,type="class")
table(pred.forest,dat_predicting$a_f)

importance(forest)
varImpPlot(forest)

partialPlot(forest,dat_training,b,"0")
partialPlot(forest,dat_training,b,"1")

# 感度を見る
forest.ff <- forestFloor(rf.fit = forest, X )