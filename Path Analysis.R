#Path Analysis - Mediator, Moderator


## Mediator

PA.RAW <- read.csv(file.choose())

#多元共線性檢定
library(car)
MED.test<-lm(reading_3 ~ eng_1 + WA_S_2, data=PA.RAW)
vif(MED.test)

# 建立中介路徑模型
library(lavaan)
MED.model <- ' WA_S_2 ~ a*eng_1
              reading_3 ~ c*eng_1 + b*WA_S_2
              indirect := a*b
              total    := c+(a*b) '

# 利用sem進行模型設定
fit.3 <- sem(MED.model, sample.nobs = 193, data = PA.RAW)

# 利用summary進行中介模型分析
MED.OP <- summary(fit.3, fit.measures=T, rsquare=T, standardized=T)

# 利用semPaths繪製模型圖
library(semPlot)
semPaths(fit.3, whatLabels = "std", layout = "spring",
         sizeMan = 10,sizeLat=10,nCharNodes=10,edge.label.cex = 1)
----------------------------------------------------
## Moderator
PA.mod <- PA.RAW[1:129, c(3,13,25)]
str(PA.mod)

# 轉換
library(dplyr)
PA.mod<-mutate(PA.mod,ethnic_h = ifelse(ethnic == "Human", 1, 0))
str(PA.mod)

# 調節
MOD.OP<-lm( math_3 ~ WA_S_1*ethnic_h, data = PA.mod)
summary(MOD.OP)

# 交互作用顯著，針對ethnic_h下水準進行迴歸分析

## @ human
MOD.OP.h <- lm( math_3 ~ WA_S_1, data = PA.mod[1:87,])
summary(MOD.OP.h)

## @ orc
MOD.OP.o <- lm( math_3 ~ WA_S_1, data = PA.mod[88:129,])
summary(MOD.OP.o)
  
# 利用ggplot繪製模型圖
library(ggplot2)
ggplot(PA.mod , aes(x = WA_S_1, y =math_3, group=ethnic)) +
              geom_point(size=3) +
              stat_smooth(method="lm",aes(color = ethnic))


