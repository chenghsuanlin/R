## Tibble/Tidy

install.packages("tidyverse")
library("tidyverse")		
library("tibble")
iris
iris2 <- as_tibble(iris)
iris2

# 用變數/欄(column)建立tibble物件
tibble(x = 1:5, y = 1, z = x^2+1)

# 用個體/列(row)建立tribble物件
tribble(
  ~x, ~y, ~z, 
  "a", 2, 3.6,
  "b", 1, 8.5,
)

df <- tibble(x = 1:3, y = 3:1)

#add row
add_row(df, x = 4, y = 0)

#You can specify where to add the new rows
add_row(df, x = 4, y = 0, .before = 2)


#You can supply vectors, to add multiple rows
#Not recommended due to the difficulty of understanding/reading
add_row(df, x = 4:5, y = 0:-1)


#The absent variables get missing values
add_row(df, x = 4)

#Too add two columns z and w
add_column(df, z = -1:1, w = 0)

library(dplyr)
bind_rows(iris2[1:5, ], iris[6:10, ])

bind_cols(iris2[, 1:2], iris[, 3:4])


# tibble與data.frame的差異
df <- data.frame(
  abc = 1:10,
  def = runif(10),
  xyz = sample(letters, 10)
)
tb <- as_tibble(df)

# dataframe => a直接與最接近的abc比對
df$a

# tibble => a => Unknown or uninitialized column: `a`
tb$a

class(as.data.frame(tb))

#########################################################

#Tidy

library(readr)

read_csv("file1.csv")
read_csv2("file2.csv")
read_delim("file3.txt", delim = "|")
read_tsv("file4.tsv")
write_delim(read_delim("file3.txt", delim = "|"),
                       "file3_write.txt", delim = "|")
library(readxl)
excel_sheets("datasets.xlsx")
read_excel("datasets.xlsx")
read_excel("datasets.xlsx", sheet = "chickwts")
read_excel("datasets.xlsx", sheet = 4)
read_excel("datasets.xlsx", n_max = 3)
read_excel("datasets.xlsx", range = "C1:E4")
read_excel("datasets.xlsx", range = cell_rows(1:4))
read_excel("datasets.xlsx", range = cell_cols("B:D"))
read_excel("datasets.xlsx", range = "mtcars!B1:D5")
read_excel("datasets.xlsx", na = "setosa")

#######################################################

#2.4 Tidy資料集合


install.packages("tidyverse")
library("tidyverse")		
library("tibble")
library("tidyr")


pew <- read_delim(
  "http://stat405.had.co.nz/data/pew.txt",
  delim = "\t"
)

print(pew)

pew.colnames <- colnames(pew)
pew.new <- pew %>%
  gather(key = "income",
       value = "cases",
       pew.colnames[2:ncol(pew)])

print(pew.new)


table2

#spread跟gather相反
table2 %>%
  spread(key = type,
       value = count)


table2 %>%
  spread(key = type,
         value = count,
         sep = "_")

tb <- read_csv("tb.csv")
print(tb)

tb.colnanmes <- colnames(tb)

tb.new <- tb %>%
  gather(
    tb.colnames[4:ncol(tb)],
    key = "type",
    value = "cases")

tb.new


tb <- read_csv("tb_new.csv")
print(tb)

tb %>%
  unite(col = "age",
        c("age_lb", "age_ub"),
        sep = "-")




