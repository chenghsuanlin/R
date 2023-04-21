#相關性 correlation R
#2018.04.25

COR.raw <- read.csv(file.choose())


COR <- COR.raw[c(4,12,17,22,15,21,26)]
str(COR) 

COR$gender <- as.factor(COR$gender)
colnames(COR) <- c("male", "reading_3","reading_2",
                   "reading_1","CO_S_3","CO_S_2","CO_S_1")
str(COR) 

# Pearson's r
library(car)
library(sandwich)
library(RcmdrMisc)
PEARSON.r <-rcorr.adjust(COR,type="pearson")
PEARSON.r

# 分組
quantile(COR$reading_1,c(0.27, 0.73))
COR$reading_1_g <- recode(COR$reading_1,
                    "lo:469.84='1'; 489.00:hi='3'; else='2'") #1=low, 2=mid, 3=high

quantile(COR$reading_2,c(0.27, 0.73))
COR$reading_2_g <- recode(COR$reading_2,
                          "lo:482.00='1'; 499.16:hi='3'; else='2'") #1=low, 2=mid, 3=high

quantile(COR$reading_3,c(0.27, 0.73))
COR$reading_3_g <- recode(COR$reading_3,
                          "lo:490.00='1'; 511.16:hi='3'; else='2'")  #1=low, 2=mid, 3=high

# Spearman rho
library(car)
library(sandwich)
library(RcmdrMisc)
COR.recode <- COR[c(5:10)]
SPEARMAN.rho <-rcorr.adjust(COR.recode,type="spearman")
SPEARMAN.rho


#corrplot
library(corrplot)
corrplot(PEARSON.r$R$r ,method = "circle")    #顏色深淺
corrplot(PEARSON.r$R$r ,method = "ellipse")   #橢圓形

corrplot.mixed(PEARSON.r$R$r ,lower = "number", upper="ellipse")


library(apaTables)
apa.cor.table(COR, filename="/Users/chenghsuanlin/Desktop/COR_table.doc")


