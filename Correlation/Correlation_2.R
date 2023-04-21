#Chi-square test
#HW4/Q1
#2018.05.09

#適合度考驗(單因子考驗) #第一種方式
CHI.goodness <- read.csv(file.choose(), header=TRUE)
str(CHI.goodness)
CHI.goodness$number <- as.factor(CHI.goodness$number)
str(CHI.goodness)
# count table and chi-square test
count <-table(CHI.goodness$number)
count

# set probability 
null.probs = c(0.0625,0.1875,0.1875,0.5625)
chisq.test(count, p=null.probs)
   


-------------------------------------------------
#適合度考驗(單因子考驗) #第二種方式

CHI.goodness<-c(315,108,101,32)
CHI.goodness<-data.frame(CHI.goodness)
null.probs<-c(0.5625,0.1875,0.1875,0.0625)
chisq.test(CHI.goodness, p=null.probs)

-------------------------------------------------
#相關性 correlation R
#HW4/Q2
#2018.05.09
  
COR.raw <- read.csv(file.choose())
COR <- COR.raw[c(1,2)]
str(COR) 


# 分組
quantile(COR$Q1,c(0.27, 0.73))
COR$Q1_g <- recode(COR$Q1,"lo:1.62='1'; 3.38:hi='3'; else='2'") #1=low, 2=mid, 3=high

quantile(COR$Q2,c(0.27, 0.73))
COR$Q2_g <- recode(COR$Q2,"lo:1.62='1'; 3:hi='3'; else='2'") #1=low, 2=mid, 3=high


# Spearman rho
library(car)
library(sandwich)
library(RcmdrMisc)
COR.recode <- COR[c(1,2)]
SPEARMAN.rho <-rcorr.adjust(COR.recode,type="spearman")
SPEARMAN.rho



