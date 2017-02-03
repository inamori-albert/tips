# 再発事象解析
# http://www.slideshare.net/gepuro/recurrent

library("survival")
library(data.table)
library(dplyr)
rats <- read.csv("rats.dat", header = F, sep = " ")

head(rats)
rats$total <- NA
for (i in unique(rats$id))
{
  rats.id <- subset(rats, rats$id == i)
  rats[which(rats$id == i), ]$total <- cumsum(rats.id$status)
}

kaidan.plot <-
  function(rats) {
    plot(
      rats$stop,
      rats$total,
      xlim = c(0, 125),
      ylim = c(0, 13),
      sub = "腫瘍の再発",
      type = "n"
    ) 
    for (j in 1:nrow(rats)) {
      rats.j <- rats[j, ] 
      segments(rats.j$start,rats.j$total - 1,rats.j$stop,rats.j$total - 1,col = rats.j$id)
      segments(rats.j$stop,rats.j$total - 1,rats.j$stop,rats.j$total,col = rats.j$id)
    }
  } 



kaidan.plot(rats)
kaidan.plot(subset(rats, rats$trt == 1))
kaidan.plot(subset(rats, rats$trt == 0)) 

data.plot <-
  function(rats) {
    plot(
      seq(1, max(rats$stop), length.out = 48),
      1:48,
      type = "n",
      xlab = "",
      ylab = ""
    ) 
    abline(h = 24:48)
    abline(h = 1:23, col = "red")
    rats.status1 <-
      subset(rats, rats$status == 1)
    points(rats.status1$stop, rats.status1$id)
  } 
data.plot(rats)

# 強度関数の推定
par(mfrow=c(2,1))
NPfit.1 <-
  coxph(
    Surv(start, stop, status) ~ 1,
    data = rats,
    subset = (trt == 1),
    method = "breslow"
  )
KM.1 <- survfit(NPfit.1, conf.int = .95, type = "aalen")
plot(KM.1, sub = "drug")

NPfit.2 <-
  coxph(
    Surv(start, stop, status) ~ 1,
    data = rats,
    subset = (trt == 0),
    method = "breslow"
  )
KM.2 <- survfit(NPfit.2, conf.int = .95, type = "aalen")
plot(KM.2, sub = "control")

# 累積強度関数
par(mfrow = c(1, 1))
NA.MF.1 <-
  data.frame(time = c(0, KM.1$time), na = -log(c(1, KM.1$surv)))
plot(
  NA.MF.1,
  type = "l",
  lty = 1,
  ylim = c(0, 8),
  xlim = c(0, 120)
)
NA.MF.2 <- data.frame(time = c(0, KM.2$time), na = -log(c(1, KM.2$surv)))
lines(NA.MF.2, type ="l", lty = 2)
legend('topleft', c("drug", "control"), lty = 1:2) 

# 比例ハザードモデル
NPfit <-
  coxph(
    Surv(start, stop, status) ~ factor(trt) + cluster(id),
    data = rats,
    method = "breslow"
  )
summary(NPfit) 
KM <- survfit(NPfit, type = "aalen") 
NA.MF <- data.frame(time = c(0, KM$time), na = -log(c(1, KM$surv)))
lines(NA.MF, type ="l", lty = 4) 
legend('topleft', c("drug", "control", "全部"), lty = c(1:2, 4))
