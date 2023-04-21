#ANOVA


##1.讀取檔案 
INDAOV.raw <- read.csv(file.choose(), header=TRUE)

##2.檢查資料
head(INDAOV.raw) #view(Indaov.raw)

##3.選取要分析的變項
INDAOV<-INDAOV.raw[c(2,3,12)]
INDAOV

##4.基本統計資訊
summary(INDAOV)

##5.資料型態
str(INDAOV) #class(INDAOV$ID)

##6.轉換資料型態
INDAOV$ID <-as.factor(INDAOV$ID)
class(INDAOV$ID)

##7.1描述性統計
library(psych)
describe(INDAOV[2:3])

##7.2變異數同質性檢定 (依~自, data=資料)
bartlett.test( reading_3 ~ ethnic, data=INDAOV)

#levene test 
library(car)
leveneTest(reading_3 ~ ethnic, data=INDAOV)

##8.ANOVA分析
INDAOV.op  <- aov(reading_3 ~ ethnic, data=INDAOV)
summary(INDAOV.op)

##9.繪製圖形
library(car)
library(sandwich)
library(RcmdrMisc)
with(INDAOV, plotMeans(reading_3, ethnic, error.bars="se"))
with(INDAOV, boxplot  (reading_3 ~ethnic, error.bars="se"))
title(xlab="ethnic", ylab="score")


##10.進行事後比較
TukeyHSD(INDAOV.op)

#Scheffe.test
library(agricolae)
sch<-scheffe.test(INDAOV.op,"ethnic",group=F)
sch$comparison

library(userfriendlyscience)
oneway(INDAOV$ethnic, y = INDAOV$reading_3, posthoc = 'games-howell')
oneway(INDAOV$ethnic, y = INDAOV$reading_3, posthoc = 'bonferroni')

#--------------------------------------------------------------
#RM-ANOVA
##1.
RMaov.raw<- read.csv(file.choose())
RMaov<-RMaov.raw[c(12,17,22)]
head(RMaov)
#2.資料轉換 多變量->單變量
RMaov.U<-stack(RMaov)
head(RMaov.U)
#3.
RMaov.U$RM_id = rep(RMaov.raw$ID, 3)
#4.
colnames(RMaov.U) = c("RM_read_score", "RM_time", "RM_id")
head(RMaov.U)

#5.同質性檢定
bartlett.test(RM_read_score ~ RM_time, data=RMaov.U)

library(car)
leveneTest(RM_read_score ~ RM_time, data=RMaov.U)

#6.繪製平均數圖
library(car)
library(sandwich)
library(RcmdrMisc)
with(RMaov.U,plotMeans(RM_read_score,RM_time,error.bars="se"))

#7.1設定因子 & 排序
time <- ordered(  c("reading_1", "reading_2", "reading_3"),
                  levels = c("reading_1", "reading_2", "reading_3"))
time<-data.frame(time)
time

#7.2將RMaov轉matrix格式，並建立模型
RMaov <- as.matrix(RMaov)
RMaov.model <- lm(RMaov ~ 1)

#7.3進行分析
RM<-Anova(RMaov.model, idata = time,idesign = ~time)
summary(RM)


#8.事後比較
library(MASS)
library(nlme)
library(multcomp)

Lme.mod<-lme(RM_read_score ~ RM_time,random = ~1|RM_id/RM_time,data = RMaov.U)
summary(glht(Lme.mod, linfct=mcp(RM_time="Tukey")))

