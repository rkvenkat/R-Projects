library(tidyverse)
library(tidymodels)
library(tidytuesdayR)



df <- tidytuesdayR::tt_load("2021-08-17")
df
View(df)

computer_raw <- df$computer

View(computer_raw)
