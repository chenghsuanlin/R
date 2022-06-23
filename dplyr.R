## dplyr 

install.packages("tidyverse")
library(tidyverse)
library(dplyr)

mtcars.tb <- as_tibble(mtcars)  #mtcars is an existed table
print(mtcars.tb)

mtcars.tb %>%
  filter(mpg > 20, hp > 100)

mtcars.tb %>%
  select(mpg, hp, gear)

mtcars.tb %>%
  filter(
    mpg > 20,
    hp > 100) %>%
  select(gear)

mtcars.tb %>%  
  arrange(
    cyl,
    disp          #default to be ascending
  )

mtcars.tb %>%
  arrange(
    desc(disp)    #descending
  )


# Newly created variables are available immediately
mtcars.tb %>%
  mutate(
    cyl2 = cyl * 2,
    cyl4 = cyl2 * 2
  )

# Remove an existed variable (mpg) and convert disp to a different unit
mtcars.tb %>%
  mutate(
    mpg = NULL,
    disp = disp * 0.0163871 # convert to liter
  )

#Only show the new variable
mtcars.tb %>%
  transmute(displ_l = disp / 61.0237)

#grouping
mtcars.tb %>%
  group_by(cyl)

mtcars.tb %>%
  group_by(cyl) %>%
  do(browser())


mtcars.tb %>%
  group_by(cyl) %>%
  summarise(
    number = n(),
    avg_hp = mean(hp),
    sd_hp = sd(hp),
    max_hp = max(hp),
    min_hp = min(hp)
  ) %>%
  arrange(desc(avg_hp))

mtcars.tb %>%
  group_by(cyl) %>%
  filter(rank(desc(hp)) < 4) %>%
  arrange(desc(cyl), desc(hp))

