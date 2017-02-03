# 不均衡データへの対処
# SMOTE
library(DMwR)
# 乱数種の固定
set.seed(123)
# 元のデータにサンプル名の付値
rownames(abalone) <- paste("original", 1:nrow(abalone), sep="")
# SMOTE関数を用いて人工的な正例の生成，負例をアンダーサンプリング
abalone.smote <- 
  SMOTE(label ~ ., data=abalone, perc.over=3500, perc.under=600)

idx <- sample(1:10, nrow(abalone.smote), replace=T)
pred <- rep(NA, nrow(abalone.smote))
# 10-fold クロスバリデーションの実行
for (i in 1:10) {
  is.test <- idx == i
  abalone.train <- abalone.smote[!is.test, ]
  abalone.test <- abalone.smote[is.test, -9]
  fit.ksvm <- ksvm(label ~., data=abalone.train,
                   kernel="polydot", kpar=list(degree=2))
  pred[is.test] <- predict(fit.ksvm, abalone.test)
}
# 分割表の表示
table(abalone.smote[, "label"], pred)
# 元々のデータ点のみを対象とした分割表の表示
is.original <- rownames(abalone.smote) %in% rownames(ablone)
table(pred[original], abalone.smote[is.original, "label"])