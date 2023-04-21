# Two-Way Mixed design ANOVA

# 1.read data file
MIXAOV.raw	<- read.csv(file.choose())
MIXAOV<-MIXAOV.raw[c(2,3,12,17,22)]

#2. check data structure
str(MIXAOV)

#ID: int -> factor
MIXAOV$ID <- as.factor(MIXAOV$ID)
class(MIXAOV$ID)

#3. reset data format
library(reshape2)
MIXAOV.U <- melt(MIXAOV, id.vars = c("ID","ethnic"))

colnames(MIXAOV.U) = 
c("MIX_id","MIX_ethnic","MIX_time","MIX_read_score")
head(MIXAOV.U)


#4. Homogeneity of Variance
#time
bartlett.test(MIX_read_score ~ MIX_time, data=MIXAOV.U)
#ethnic
library(car)
leveneTest(MIX_read_score ~ MIX_ethnic, data=MIXAOV.U)

# 5.sphericity test & Mixed design ANOVA
library(ez)
ezANOVA(data=MIXAOV.U, dv=.(MIX_read_score), wid=.(MIX_id), within=.(MIX_time),
        between=.(MIX_ethnic), detailed=T,type = 3)

# 6.mean plot
library(car)
library(sandwich)
library(RcmdrMisc)
with(MIXAOV.U, plotMeans(MIX_read_score, MIX_ethnic,MIX_time,error.bars="se")) 
with(MIXAOV.U, plotMeans(MIX_read_score, MIX_time,MIX_ethnic,error.bars="se")) 

#7. post-hoc
library(MASS)
library(nlme)
library(multcomp)
#time
pHoc_time<-lme(MIX_read_score~MIX_time,random = ~1|MIX_id/MIX_time, data = MIXAOV.U)
summary(glht(pHoc_time, linfct=mcp(MIX_time="Tukey")))
#ethnic
pHoc_ethnic<-lme(MIX_read_score~MIX_ethnic,random = ~1|MIX_id/MIX_ethnic, data = MIXAOV.U)
summary(glht(pHoc_ethnic, linfct=mcp(MIX_ethnic="Tukey")))


#########################################################
# 9.  Split   T:time   E:ethnic
FIX.T <- split(MIXAOV.U,MIXAOV.U$MIX_time)     #ANOVA
FIX.E <- split(MIXAOV.U,MIXAOV.U$MIX_ethnic)   #RM-ANOVA


#10.ANOVA

#time1
FIX.T1.OP<- aov(MIX_read_score ~ MIX_ethnic ,data=FIX.T$reading_1)
summary(FIX.T1.OP)

#time2
FIX.T2.OP<- aov(MIX_read_score ~ MIX_ethnic ,data=FIX.T$reading_2)
summary(FIX.T2.OP)

#time3
FIX.T3.OP<- aov(MIX_read_score ~ MIX_ethnic ,data=FIX.T$reading_3)
summary(FIX.T3.OP)

#11. p-value
#time1
pf(12.88, df1=2, df2=570,lower.tail=FALSE)
#time2
pf(13.05, df1=2, df2=570,lower.tail=FALSE)
#time3
pf(15.35, df1=2, df2=570,lower.tail=FALSE)

# 進行相依因子單純主要效果檢驗
#Orc
ezANOVA(data=FIX.E$Orc, dv=.(MIX_read_score), wid=.(MIX_id), within=.(MIX_time), detailed=TRUE)
#Undead
ezANOVA(data=FIX.E$Undead, dv=.(MIX_read_score), wid=.(MIX_id), within=.(MIX_time), detailed=TRUE)
#Human
ezANOVA(data=FIX.E$Human, dv=.(MIX_read_score), wid=.(MIX_id), within=.(MIX_time), detailed=TRUE)

# p-value
#Orc
pf(98.62, df1=2, df2=380,lower.tail=FALSE)
#Undead
pf(165.98, df1=2, df2=380,lower.tail=FALSE)
#Human
pf(237.38, df1=2, df2=380,lower.tail=FALSE)

