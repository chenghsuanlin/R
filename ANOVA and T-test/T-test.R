# t test
#HW1 Q1

##1.讀檔
t.data <- read.csv(file.choose(), header=TRUE)

##2.?ˬd????
head(INDAOV.raw) #view(Indaov.raw)

##3.?????n?��R???ܶ?
INDAOV<-INDAOV.raw[c(2,3,12)]
INDAOV

##4.?򥻲έp???T
summary(INDAOV)

##5.???ƫ??A
str(INDAOV) #class(INDAOV$ID)

##6.?ഫ???ƫ??A
INDAOV$ID <-as.factor(INDAOV$ID)
class(INDAOV$ID)

##7.1?y?z?ʲέp
library(psych)
describe(INDAOV[2:3])

##7.2?ܲ??ƦP?????˩w (??~??, data=????)
bartlett.test( reading_3 ~ ethnic, data=INDAOV)

#levene test 
library(car)
leveneTest(reading_3 ~ ethnic, data=INDAOV)

##8.ANOVA?��R
INDAOV.op  <- aov(reading_3 ~ ethnic, data=INDAOV)
summary(INDAOV.op)

##9.ø?s?ϧ?
library(car)
library(sandwich)
library(RcmdrMisc)
with(INDAOV, plotMeans(reading_3, ethnic, error.bars="se"))
with(INDAOV, boxplot  (reading_3 ~ethnic, error.bars="se"))
title(xlab="ethnic", ylab="score")


##10.?i???ƫ?????
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
#2.?????ഫ ?h?ܶq->???ܶq
RMaov.U<-stack(RMaov)
head(RMaov.U)
#3.
RMaov.U$RM_id = rep(RMaov.raw$ID, 3)
#4.
colnames(RMaov.U) = c("RM_read_score", "RM_time", "RM_id")
head(RMaov.U)

#5.?P?????˩w
bartlett.test(RM_read_score ~ RM_time, data=RMaov.U)

library(car)
leveneTest(RM_read_score ~ RM_time, data=RMaov.U)

#6.ø?s?????ƹ?
library(car)
library(sandwich)
library(RcmdrMisc)
with(RMaov.U,plotMeans(RM_read_score,RM_time,error.bars="se"))

#7.1?]?w?]?l & ?Ƨ?
time <- ordered(  c("reading_1", "reading_2", "reading_3"),
                  levels = c("reading_1", "reading_2", "reading_3"))
time<-data.frame(time)
time

#7.2?NRMaov??matrix?榡?A?ëإ߼ҫ?
RMaov <- as.matrix(RMaov)
RMaov.model <- lm(RMaov ~ 1)

#7.3?i???��R
RM<-Anova(RMaov.model, idata = time,idesign = ~time)
summary(RM)


#8.?ƫ?????
library(MASS)
library(nlme)
library(multcomp)

Lme.mod<-lme(RM_read_score ~ RM_time,random = ~1|RM_id/RM_time,data = RMaov.U)
summary(glht(Lme.mod, linfct=mcp(RM_time="Tukey")))

