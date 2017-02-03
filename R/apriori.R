# アソシエーション分析
library("arules")
library("dplyr")

require(redshift)
conn <- redshift.connect(hogehoge)
tables <- redshift.tables(conn)
cols <- redshift.columns(conn, schema="public",hoge)

item.train <-
  redshift.query(conn,
                 "
                 SELECT id, no, listagg(nm, ',') within group (order by id) as nm
                 FROM table_a a
                 GROUP BY id,no
                 ;
                 "
  )
item.train$nm <- strsplit(item.train$nm, ",")
item.train.t <- item.train$nm
item.train.t <- as.factor(item.train.t)
item.trans <- as(item.train.t, "transactions")

head(as(item.trans, "data.frame"))

itemFrequencyPlot(item.trans, type="relative")
itemFrequencyPlot(item.trans, type="absolute")

grule1 <- apriori(item.trans)
grule2 <- apriori(item.trans,p=list(support=0.00001,confidence=0.5,maxlen=100,ext=TRUE))
print(grule2)

inspect(grule2)
inspect(head(sort(grule2, by = "support"),n=20))

write.table(inspect(grule2), "grule2.csv", sep = "\t")
