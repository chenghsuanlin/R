## 問題 1 (Tidy 原則)

Ans: ABC

# A.Column 其實是值而不是變數

(columnn名是日期這些column)

# B.把變數當成值 

(column "type"。應該分成2個變數:open(開盤價)及close(收盤價))

# C.多個變數儲存在同一個 column 中 

(算不符合也算符合: 
    
    - 日期要年月日或有分析需求(通常有)需要把年、月、日分開
  
  - "security_id"是"證券代碼"及"公司名稱"連在一起，不過這二個資料是唯一對應
  
  ## 問題 2 (實際操作：gather 函數)
  
install.packages("tidyverse")
library("tidyverse")
library(readr)
  
library(tidyr)
  
stock.data <- read_csv("TWSE_Stock Data_2012-2017.csv")
stock.data.colnames <- colnames(stock.data)
  
stock.data <- stock.data %>%
    gather(
      key = "date", 
      value = "price", 
      stock.data.colnames[3 : ncol(stock.data)] 
    )
head(stock.data)

stock.data <- stock.data %>%
  spread(
    key = "type", 
    value = "price",
    )

head(stock.data)