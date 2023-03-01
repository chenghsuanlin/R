## Relational Database
#watch.table：User's watching and behavioral data
#user.table: User's demographic data
#drama.table：Drama's related data

install.packages("tidyverse")
library("tidyverse")		

watch.table <- read_csv("watch_table.csv")
watch.table

user.table <- read_csv("user_table.csv")
user.table

drama.table <- read_csv("drama_table.csv")
drama.table