
library(tidyverse)

data <- read.csv('./Downloads/PracticeR/US_Legal_Betting/data.csv')

data %>% 
  mutate(status = str_replace(status,"Sports betting status: ","")) %>% 
  distinct(status)




status_map <- tibble(status =  c("Legal - Not Yet Operational","Live, Legal*","Active or Pre-Filed Legislation in 2021"),
                     betting_status = c("Legal and operational","Legal but not yet operational","Not legal"))



data %>% 
  mutate(status = str_replace(status,"Sports betting status: ","")) %>% 
  inner_join(status_map,by = c("status")) %>% 
  select("state","betting_status")


