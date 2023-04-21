#Chi-square test
#2018.04.25

#適合度考驗(單因子考驗)
CHI.raw <- read.csv(file.choose(), header=TRUE)

#
CHI.goodness <- CHI.raw[c(2,4)]
str(CHI.goodness) 

# as factor
CHI.goodness$ID <- as.factor(CHI.goodness$ID)


library(plyr)
CHI.goodness$gender <- mapvalues(CHI.goodness$gender,from = c(0,1),to = c("female", "male")
CHI.goodness$gender <- as.factor(CHI.goodness$gender)
str(CHI.goodness) 

# count table and chi-square test
count <-table(CHI.goodness$gender)
count
chisq.test(count)

# set probability (p(female), p(male))
null.probs.1 = c(0.5,0.5)
chisq.test(count, p=null.probs.1)
# or
null.probs.2 = c(0.4,0.6)
chisq.test(count, p=null.probs.2)

--------------------------------------------------------------------
#獨立性考驗 Test of Independence

#input data and check
CHI.ind <- CHI.raw[c(2,3,4)]
str(CHI.ind) 

CHI.ind$ID <- as.factor(CHI.ind$ID)
library(plyr)
CHI.ind$gender <- mapvalues(CHI.ind$gender,from = c(0,1),to = c("female", "male")
CHI.ind$gender <- as.factor(CHI.ind$gender)
str(CHI.ind) 

# contingency table 列聯表
ind.table <- table(CHI.ind$gender,CHI.ind$ethnic)
ind.table

chisq.test(ind.table)

----------------------------------------------------------------
#相依樣本卡方考驗 2 by 2 contingency table
# select variables and check data 
CHI.McNemar<-CHI.raw[c(2,12,22)]
str(CHI.McNemar) 

CHI.McNemar$ID <- as.factor(CHI.McNemar$ID)
str(CHI.McNemar) 
summary(CHI.McNemar)

# group
library(dplyr)
CHI.McNemar<-mutate(CHI.McNemar, reading3 = ifelse(reading_3 >= 499.3,"3H","3L"))
CHI.McNemar<-mutate(CHI.McNemar, reading1 = ifelse(reading_1 >= 477.3,"1H","1L"))

# contingency table
MC.test <- table(CHI.McNemar$reading3,CHI.McNemar$reading1)
MC.test

# chi-squared test
mcnemar.test(MC.test)

# Milk tea 奶茶實驗
# Fisher
# 小樣本
TeaTasting <- matrix(c(4, 0, 0, 4),nrow = 2,
               dimnames = list(Guess = c("Milk_First_Guess", "Tea_First_Guess"),
                               Truth = c("Milk_First_Truth", "Tea_First_Truth")))
TeaTasting

fisher.test(TeaTasting)


# Yate's correlation for continuity
# 小樣本
TeaTasting.2 <- matrix(c(10, 0, 0, 10),nrow = 2,
                     dimnames = list(Guess = c("Milk_First_Guess", "Tea_First_Guess"),
                                     Truth = c("Milk_First_Truth", "Tea_First_Truth")))
TeaTasting.2

chisq.test(TeaTasting.2)

   ##就算增加到10杯也真的分得出來