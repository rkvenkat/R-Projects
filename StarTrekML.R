library(tidyverse)
library(tidymodels)
library(tidytuesdayR)



df <- tidytuesdayR::tt_load("2021-08-17")
df
View(df)

computer_raw <- df$computer



computer_raw %>% 
  count(char_type)


# Testing different functions ---------------------------------------------



a <- tibble(
  g = c(1990, 1991, 1992, 1993,1994),
  x = c(1, 1, 2, 1,1) )


a %>% 
  mutate(decade = g%/%2) %>% 
  view()


a %>% 
  distinct(g,.keep_all = FALSE)


------------------------------
  
  
  computer_raw %>% 
    distinct(value_id,.keep_all = TRUE) %>% 
    count(char_type)