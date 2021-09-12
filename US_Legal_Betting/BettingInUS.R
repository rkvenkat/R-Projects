
library(tidyverse)
library(maps)
library(statebins)

data <- read.csv('./Downloads/PracticeR/US_Legal_Betting/data.csv')

data %>% 
  mutate(status = str_replace(status,"Sports betting status: ","")) %>% 
  distinct(status)




status_map <- tibble(status =  c("Legal - Not Yet Operational","Live, Legal*","Active or Pre-Filed Legislation in 2021"),
                     betting_status = c("Legal and operational","Legal but not yet operational","Not legal"))



data <- data %>% 
  mutate(status = str_replace(status,"Sports betting status: ","")) %>% 
  inner_join(status_map,by = c("status")) %>% 
  select("state","betting_status")


us_states <- map_data("state")
head(us_states)

status_colors <- c("#31a354","#a1d99b","#e5f5e0")

a <- data %>% 
  mutate(state = str_replace_all(state,"-"," ")) %>% 
  full_join(us_states,by = c("state" = "region"),keep = TRUE) %>% 
  mutate(betting_status = ifelse(is.na(betting_status),"Not legal",betting_status))

a %>% 
  filter(is.na(region)) %>% 
  distinct(state)

p <- ggplot(data = a,
            mapping = aes(x = long, y = lat,
                          fill = betting_status,
                          group = group))

p1 <- p + geom_polygon(color = "gray", size = 0.3) 

p2 <- p1 + scale_fill_manual(values = status_colors) +
  labs(title = "Betting Status in US",
       fill = "Status") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())



