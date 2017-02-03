# xgboostで分類
dat_origin <- fread('input.csv', header = T)

dat <- dplyr::sample_n(tbl=dat_origin, size=20000)

dat <- dat[,c(2:11)]

dat$f <- ifelse(as.integer(dat$f)==0, "0", dat$f)
dat$f <- ifelse(as.integer(dat$f)>0, "1", dat$f)

# 正解データと訓練データに分離
rowdata<-nrow(dat)
random_ids<-sample(rowdata,rowdata*0.5)
dat_training<-dat[random_ids, ]
dat_predicting<-dat[-random_ids, ]
dat_predicting

# xgboostで分類
features<-dat_training[,1:9]
labels<-dat_training[,10]
labels<-as.factor(labels[[1]])
table(dat_training[,10])

train.mx<-sparse.model.matrix(f~., dat_training)
test.mx<-sparse.model.matrix(f~., dat_predicting)

dtrain<-xgb.DMatrix(train.mx, label=dat_training$f)
dtest<-xgb.DMatrix(test.mx, label=dat_predicting$f)
train.gdbt<-xgb.train(params=list(objective="multi:softmax", num_class=10, eval_metric="mlogloss", eta=0.2, max_depth=5, subsample=1, colsample_bytree=0.5), data=dtrain, nrounds=150, watchlist=list(eval=dtest, train=dtrain))

pred<-predict(train.gdbt,newdata=dtest)
sum(diag(table(dat_predicting$f,pred)))/nrow(dat_predicting)
table(dat_predicting$f,pred)

imp<-xgb.importance(names(dat),model=train.gdbt)
print(imp)
xgb.plot.importance(imp)

xgb.plot.tree(feature_names=names(dat[,-10]),model=train.gdbt, n_first_tree=2,plot_width = 2000, plot_height = 2000)
