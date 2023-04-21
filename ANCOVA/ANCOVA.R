# Jiun-Yu Wu, MLlab, IED, NCTU
#
#107.03.24 ANCOVA


##1.Input and check data.
ACOV.raw	<- read.csv(file.choose(), header=TRUE)
ACOV<-ACOV.raw[c(3,12,21)]
head(ACOV)
str(ACOV)

##2.Test the homogeneity of regression coefficient.
TEST1	<- aov(reading_3 ~ CO_S_2*ethnic, data=ACOV)
summary(TEST1)

##3.Drawing interaction plots.
#install.packages("ggplot2")
library(ggplot2)  
ggplot(ACOV, aes(CO_S_2, reading_3)) +
      facet_grid(. ~ ethnic)+ 
      geom_point()  + 
      stat_smooth(method="lm")
ggplot(ACOV, aes(x = CO_S_2, y = reading_3, color = ethnic)) + 
       geom_point()  + 
       stat_smooth(method="lm")

##4.Test the homogeneity of variance.
bartlett.test(reading_3 ~ethnic, data=ACOV)

#install.packages("car")
library(car)
leveneTest(reading_3 ~ ethnic, data=ACOV)


##5.Doing ANCOVA.
ACOV.OP	<- aov(reading_3 ~ CO_S_2+ethnic,data=ACOV)
summary(ACOV.OP)

##6.Original mean of variables.
aggregate(reading_3 ~ethnic ,mean ,data=ACOV)

##7.Adjusted mean.
#install.packages("effects")
library(effects)
adj.mean<-effect("ethnic",ACOV.OP)
data.frame(adj.mean)

##8.Post-hoc.
#install.packages("multcomp")
library(multcomp)
posthoc <- glht(ACOV.OP, linfct = mcp(ethnic = "Tukey"))
summary(posthoc)

##9.Drawing the plot.
plot(adj.mean)

##10.Calculate the eta squared.
#install.packages("lsr")
library(lsr)
etaSquared(ACOV.OP)