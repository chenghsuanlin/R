#Simple linear regression 


# 讀檔
REG.RAW <- read.csv(file.choose())

# run regression
REG.OP <-lm( math_3 ~ CO_S_2 , data= REG.RAW)
summary(REG.OP)

# 標準化迴歸係數
library(lm.beta)
Std.REG.OP <-lm.beta(REG.OP)
summary(Std.REG.OP)

----------------------------------
names(REG.OP)
#殘差常態性假設
library(psych)
describe(REG.OP$residuals)

#獨立性假設
library(car)
durbinWatsonTest(REG.OP)

#同質假設
library(car)
ncvTest(REG.OP)
----------------------------------------------------
# 畫圖
library(ggplot2)
ggplot(REG.OP, aes(x = CO_S_2, y = math_3)) +
            geom_point(size=3) +
            stat_smooth(method="lm")

# outlier
REG.outlier <- influence.measures(REG.OP)
summary(REG.outlier)

