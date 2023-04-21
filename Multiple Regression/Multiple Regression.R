#多元迴歸 Multiple-regression R
#2018.05.16

# 讀檔
REG.RAW <- read.csv(file.choose())
MUREG <- REG.RAW[c(2,13,20,21)]
MUREG$ID <- as.factor(MUREG$ID)
str(MUREG)

# run multiple regression
MUREG.OP <-lm( math_3 ~ CO_S_2 + WA_S_2 , data= MUREG)
summary(MUREG.OP)

# 標準化迴歸係數
library(lm.beta)
Std.MUREG.OP <-lm.beta(MUREG.OP)
summary(Std.MUREG.OP)


#多元共線性檢定
library(car)
vif(MUREG.OP)

# 畫圖
library(ggplot2)
ggplot(MUREG.OP, aes(x = CO_S_2, y = math_3)) +
      geom_point(size=3) +stat_smooth(method="lm")
ggplot(MUREG.OP, aes(x = WA_S_2, y = math_3)) +
  geom_point(size=3) +stat_smooth(method="lm")
----------------------------------------------------

# Dummy coding
DUM <- REG.RAW[c(3,13)]
str(DUM)

#check dummy coding
contrasts(DUM$ethnic)    #Human as reference

# dummy coding regression
DUM.OP.HUMAN <- lm(math_3 ~ ethnic, data=DUM)
summary(DUM.OP.HUMAN)

# 對照ANOVA
ANOVA.OP <- aov(math_3 ~ ethnic, data=DUM)
summary(ANOVA.OP)

DUM$ethnic <- relevel(DUM$ethnic,"Orc")
contrasts(DUM$ethnic)


DUM.OP.ORC <- lm(math_3 ~ ethnic, data=DUM)
summary(DUM.OP.ORC)


DUM$ethnic.N <- as.numeric(DUM$ethnic)
str(DUM)

levels(DUM$ethnic)

library(dplyr)
DUM<-mutate(DUM, D1 = ifelse(ethnic.N == 1,"1","0"))
DUM<-mutate(DUM, D2 = ifelse(ethnic.N == 2,"1","0"))
DUM<-mutate(DUM, D3 = ifelse(ethnic.N == 3,"1","0"))

DUM.OP.ORC2 <- lm(math_3 ~ D2+D3 , data=DUM)
summary(DUM.OP.ORC2)

library(lm.beta)
Std.DUM.OP.ORC2 <- lm.beta(Std.DUM.OP.ORC2)
summary(Std.DUM.OP.ORC2)

DUM.OP.HUMAN2 <- lm(math_3 ~ D1+D3, data=DUM)    #Human
summary(DUM.OP.HUMAN2)
