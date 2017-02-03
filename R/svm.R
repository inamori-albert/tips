# SVM で分類
dat_svm <- ksvm(f ~., data=dat_training )
dat_svm

result_predict<-predict(dat_svm, dat_predicting)
result_predict

table(result_predict,dat_predicting$f)