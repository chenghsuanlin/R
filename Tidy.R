##Practicing gather/spread/separate function
##Data set: TWSE_Stock Data_2012-2017.csv

install.packages("tidyverse")
library("tidyverse")
library(readr)
library(tidyr)

TWSE <- read_csv("TWSE_Stock Data_2012-2017.csv")
print(TWSE)

TWSE.1 <- gather(TWSE,
                 key = "date",
                 value = "cases",
                 TWSE.colnames[3:ncol(TWSE)])
head(TWSE.1)
  
TWSE.2 <- spread(TWSE.1,
                 key = "type",
                 value = "cases",
                 fill = NA,
                 convert = FALSE,
                 drop = TRUE,
                 sep = NULL)
head(TWSE.2)

TWSE.3 <- separate(TWSE.2,
                   col = date,
                   into = c("year", "month", "day"),
                   sep = "/",
                   convert = TRUE)
head(TWSE.3)
