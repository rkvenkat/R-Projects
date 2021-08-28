
# Notes -------------------------------------------------------------------

# Trying to recreate: https://twitter.com/WSJGraphics/status/1431626881212760067




library(tidyverse)
library(lubridate)
library(showtext)
library(ggtext)

font_add_google("Roboto","roboto")
showtext_auto()

data <- tibble(category = c("18-29 year olds",
                            "30-39",
                            "40-49",
                            "50-59",
                            "60-69",
                            "70-79",
                            "80 and older"),
               "20210110" = c(5,10,15,15,20,25,10),
               "20210827" = c(10,15,20,20,25,5,5))


data <- data %>% 
  pivot_longer(!category,names_to = "date", values_to = "count")


data <- data %>% 
  mutate(date = ymd(as.integer(date))) %>% 
  mutate(date = format(date,"%b.%d"))


cat <- data %>% 
  distinct(category)

a <- data %>% 
  mutate(dat = factor(date)) %>% 
  levels(dat)

levels(a$dat)



data %>% 
  ggplot(aes(fill=category, y=count, x=date)) + 
  geom_bar(position="fill", stat="identity",width = 0.90) +
  labs(
    title = "Share of hospital admissions for\nCovid-19 by age group",
    subtitle = "Comparing week of Jan.10 with week of Aug.27",
    caption = "Source: U.S.Department of Health and Human Services"
  ) +
  theme(
    text = element_text(family = "roboto"),
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=20),
                             , family = "roboto"),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    plot.background = element_rect(fill="#F3FAFE"),
    panel.background = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(color = c(rep(NA, nrow(data)),
                                          rep("darkgray", nrow(data)+1)),
                                size=0.2),
    axis.text.x = element_text(color="#686868", size=6),
    axis.title.x = element_markdown(family="roboto", face="bold"),
    axis.line = element_line(color="darkgray", size=0.2)
  )




View(data)
