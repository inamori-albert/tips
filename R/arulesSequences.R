# 系列パターンマイニング

library(arulesSequences)
library(dplyr)

ds <- read_baskets("hogehoge.tsv", info = c("sequenceID","eventID","SIZE"), sep = "\t")

ds.df <- as(ds, "data.frame")
write.csv(ds.df, "ds.csv")

ds.sp <- cspade(ds, parameter=list(support=0.1), control=list(verbose=TRUE))

ds.sp.df <- as(ds.sp, "data.frame")
head(ds.sp.df)
tail(ds.sp.df)
write.csv(ds.sp.df, "ds_sp_df.csv")

rules <- ruleInduction(ds.sp, confidence=0.0)
df_rules <- as(rules, "data.frame")
write.csv(df_rules, "rules.csv")
