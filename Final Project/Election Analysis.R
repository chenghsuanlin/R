#call package
library(psych)
library(car)
library(RcmdrMisc)
library(lsr)
library(agricolae)
library(apaTables)
library(lm.beta)
library(ggplot2)
library(ppcor)
library(stargazer)

#import data
Elec_raw<-read.csv(file.choose(), header=T, sep=",")
Elec<-Elec_raw[c(1,2,3,4,7)]
head(Elec)
str(Elec)
summary(Elec)

#Assumption of ANOVA
describe(Elec) #normal dist
leveneTest(Elec$Income~Party,data=Elec) #homogeneity 
leveneTest(Elec$Income~Competitiveness,data=Elec)
leveneTest(Elec$Expenditure~Party,data=Elec)
leveneTest(Elec$Expenditure~Competitiveness,data=Elec)

##ANOVA for income
Elec_income<-aov(Income~Party*Competitiveness,data=Elec)
summary(Elec_income)
etaSquared(Elec_income,anova=TRUE)

#post hoc for income
elec_sch_income<-scheffe.test(Elec_income,"Party",group=F)
elec_sch_income$comparison

##ANOVA for expenditure
Elec_expen<-aov(Elec$Expenditure~Party*Competitiveness,data=Elec)
summary(Elec_expen)
etaSquared(Elec_expen,anova=TRUE)

#post hoc for expenditure
elec_sch_expen<-scheffe.test(Elec_expen,"Party",group=F)
elec_sch_expen$comparison

##multiple regression
Elec_2<-Elec_raw[c(3,4,5)]
str(Elec_2)

#correlation
PEARSON.r<-rcorr.adjust(Elec_2,type="pearson")
PEARSON.r

#regression model
colnames(Elec_2)<-c("Vote_Income","Vote_Expenditure","Vote_rate")
Elec_lm<-lm(Vote_rate~Vote_Income+Vote_Expenditure,data=Elec_2)
summary(Elec_lm)

#assumptions
describe(Elec_lm$residuals)
durbinWatsonTest(Elec_lm)#not met
ncvTest(Elec_lm)#not met

#standardized coefficient
Std.Elec_lm<-lm.beta(Elec_lm)
summary(Std.Elec_lm)

#VIF
vif(Elec_lm)

#plot
ggplot(Elec_lm,
       aes(x=Vote_Income,y=Vote_rate))+
  geom_point(size=2)+stat_smooth(method="lm")
ggplot(Elec_lm,
       aes(x=Vote_Expenditure,y=Vote_rate))+
  geom_point(size=2)+stat_smooth(method="lm")

#outlier testing
Elec_outlier<-influence.measures(Elec_lm)
summary(Elec_outlier)

##additional test
#transformation of log
Elec_2_log<-log(Elec_2)
Elec_2_log<-Elec_2_log[-c(3,20,21,32),]#cut the invalid data
str(Elec_2_log)

#regression model
colnames(Elec_2_log)<-c("Vote_Income","Vote_Expenditure","Vote_rate")
Elec_lm_log<-lm(Vote_rate~Vote_Income+Vote_Expenditure,data=Elec_2_log)
summary(Elec_lm_log)

#assumptions
describe(Elec_lm_log$residuals)
durbinWatsonTest(Elec_lm_log)#not met
ncvTest(Elec_lm_log)

##patial correlation
pcor(Elec_2)
spcor(Elec_2)