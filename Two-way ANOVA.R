# Two-Way independent samples & Mixed design ANOVA


# 1.read data file
INDAOV2.raw		<- read.csv(file.choose(), header=TRUE)

#2.select variables and check data
INDAOV2<-INDAOV2.raw[c(2,3,4,12)] 
head(INDAOV2)
str(INDAOV2)

#3.ID: int -> factor; gender: 0,1 --> female, male
INDAOV2$ID <-as.factor(INDAOV2$ID)
class(INDAOV2$ID)

# gender: int -> character -> factor
INDAOV2$gender <-as.factor(INDAOV2$gender)
class(INDAOV2$gender)

# or
library(plyr)
INDAOV2$gender <- mapvalues(INDAOV2$gender,
                            from = c(0,1),to = c("female", "male"))

class(INDAOV2$gender) 
INDAOV2$gender <-as.factor(INDAOV2$gender)
str(INDAOV2)

#4.describe statitc information
summary(INDAOV2)

#5.check normality and homogeniety
#5.1
library(psych)
describe(INDAOV2)

#5.2 Homogeneity of Variance
# ethnic
bartlett.test(reading_3 ~ ethnic, data=INDAOV2)
# gender
library(car)
leveneTest(reading_3 ~ gender, data=INDAOV2)

# 6.two-way ANOVA
INDAOV2.op <- aov(reading_3 ~ ethnic*gender ,data=INDAOV2)
summary(INDAOV2.op)

# 7.mean plot
library(car)
library(sandwich)
library(RcmdrMisc)

with(INDAOV2, plotMeans(reading_3, ethnic,gender,error.bars="se")) 
with(INDAOV2, plotMeans(reading_3, gender,ethnic,error.bars="none")) 

#8. eta-squared
library(lsr)
etaSquared(INDAOV2.op,anova=TRUE)

#9.post-hoc
#Scheffe.test
library(agricolae)
sch<-scheffe.test(INDAOV2.op,"ethnic",group = F)
sch$comparison


#########################################################
# 11.  Split   G:gender   E:ethnic
FIX.G <-split(INDAOV2,INDAOV2$gender)
head(FIX.G$male)
head(FIX.G$female)

FIX.E <-split(INDAOV2,INDAOV2$ethnic) 
head(FIX.E$Human)
head(FIX.E$Orc)
head(FIX.E$Undead)

#12. ???¥D?n?ĪG????

# gender
FIX.GF.op	<- aov(reading_3 ~ ethnic ,data=FIX.G$female)
summary(FIX.GF.op)
FIX.GM.op	<- aov(reading_3 ~ ethnic ,data=FIX.G$male)
summary(FIX.GM.op)

# p-value
pf(3.93, df1=2, df2=187,lower.tail=FALSE)
pf(14.91, df1=2, df2=187,lower.tail=FALSE)


# ethnic
FIX.EO.op	<- aov(reading_3 ~ gender ,data=FIX.E$Orc)
summary(FIX.EO.op)
FIX.EU.op	<- aov(reading_3 ~ gender ,data=FIX.E$Undead)
summary(FIX.EU.op)
FIX.EH.op	<- aov(reading_3 ~ gender ,data=FIX.E$Human)
summary(FIX.EH.op)
# p-value
pf(4.73, df1=1, df2=187,lower.tail=FALSE)
pf(0.56, df1=1, df2=187,lower.tail=FALSE)
pf(0.14, df1=1, df2=187,lower.tail=FALSE)




