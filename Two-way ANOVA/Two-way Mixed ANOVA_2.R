# Two-Way Mixed design ANOVA 
# HW3 Q2

# 1.read data file
MIXAOV.raw	<- read.csv(file.choose())
MIXAOV<-MIXAOV.raw

#2. check data structure
str(MIXAOV)

#ID: int -> factor
MIXAOV$Mix_Id<- as.factor(MIXAOV$Mix_Id)
class(MIXAOV$Mix_Id)
head(MIXAOV)

#3 check 樣本常態性
libarary(psych)
describe(MIXAOV$MIX_score)  

#4. Homogeneity of Variance
#subject
bartlett.test(score ~ Mix_subject, data=MIXAOV)
#method
bartlett.test(score ~ Mix_method, data=MIXAOV)

# 5.sphericity test & Mixed design ANOVA
library(ez)
ezANOVA(data=MIXAOV, dv=.(score), wid=.(Mix_Id), within=.(Mix_subject),
        between=.(Mix_method), detailed=T,type = 3)

# 6.mean plot
library(car)
library(sandwich)
library(RcmdrMisc)
with(MIXAOV, plotMeans(score, Mix_subject,Mix_method,error.bars="se")) 
with(MIXAOV, plotMeans(score, Mix_method,Mix_subject,error.bars="se")) 


summary(MIXAOV)

#7. post-hoc
library(MASS)
library(nlme)
library(multcomp)
#subject
pHoc_subject<-lme(score~Mix_subject,random = ~1|Mix_Id/Mix_subject, data = MIXAOV)
summary(glht(pHoc_subject, linfct=mcp(Mix_subject="Tukey")))
#method
pHoc_method<-lme(score~Mix_method,random = ~1|Mix_Id/Mix_method, data = MIXAOV)
summary(glht(pHoc_method, linfct=mcp(Mix_method="Tukey")))


#########################################################
# 9.  Split   S:subject   M:method
FIX.S <- split(MIXAOV,MIXAOV$Mix_subject)       #ANOVA
FIX.M <- split(MIXAOV,MIXAOV$Mix_method)       #RM-ANOVA


#10.ANOVA

#subject1
FIX.S1.OP<- aov(score ~ Mix_method ,data=FIX.S$Chinese)
summary(FIX.S1.OP)


#subject3
FIX.S3.OP<- aov(score ~ Mix_method ,data=FIX.S$Math)
summary(FIX.S3.OP)

#11. p-value
#subject1
pf(0.01, df1=1, df2=18,lower.tail=FALSE)
#subject2
pf(3.35, df1=1, df2=18,lower.tail=FALSE)
#subject3
pf(16.22, df1=1, df2=18,lower.tail=FALSE)

# 進行相依因子單純主要效果檢驗
#New
ezANOVA(data=FIX.M$New, dv=.(score), wid=.(Mix_Id), within=.(Mix_subject), detailed=TRUE)
#Traditional
ezANOVA(data=FIX.M$Traditional, dv=.(score), wid=.(Mix_Id), within=.(Mix_subject), detailed=TRUE)


# p-value
#New
pf(60.9, df1=2, df2=12,lower.tail=FALSE)
#Traditional
pf(0.75, df1=2, df2=12,lower.tail=FALSE)


