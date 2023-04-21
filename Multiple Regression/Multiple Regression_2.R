#HW5 多元迴歸 Multiple-regression R
#2018.05.18

# 讀檔
REG.RAW <- read.csv(file.choose())
MUREG <- REG.RAW

str(MUREG)

# run multiple regression
MUREG.OP <-lm( y ~ x1 + x2 + x3 + x4 + x5, data= MUREG)
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
ggplot(MUREG.OP, aes(x = x4, y = y)) +
      geom_point(size=3) +stat_smooth(method="lm")

-----------------------------------------------------
Q2
# 讀檔
REG.RAW <- read.csv(file.choose())
MUREG <- REG.RAW
str(MUREG)


# run multiple regression
MUREG.OP <-lm( y ~ x, data= MUREG)
summary(MUREG.OP)

# 標準化迴歸係數
library(lm.beta)
Std.MUREG.OP <-lm.beta(MUREG.OP)
summary(Std.MUREG.OP)


