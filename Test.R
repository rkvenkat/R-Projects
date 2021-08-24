

library(tidyverse)
library(geojsonio)
library(broom)
library(rgeos)
library(patchwork)
library(showtext)

library(tidytuesdayR)

chain_investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')

tidytuesdayR::tt_load("2021-08-10")



head(chain_investment)
View(chain_investment)

chain_investment %>% 
  count(category)


main <- chain_investment%>%
  filter(group_num==1)%>%    # Filter thanks to "group_num" attribute                     
  mutate(lab=case_when(      # New column with simpler names
    category=="Total basic infrastructure"~"Basic",
    category=="Total digital infrastructure"~"Digital",
    category=="Total social infrastructure"~"Social"
  ))


ggplot(
  data=main,
  aes(x=year,y=gross_inv_chain,fill=lab)      # Choose newly created lab column for fill
)+
  geom_area()+
  labs(
    title = "Evolution of investment on US infrastructures",
    x="Year",
    y="Investment (millions of 2012 $)"
  )+
  theme_minimal()



# Reorder factors
main$lab<-factor(
  main$lab,
  c(
    "Digital",
    "Social",
    "Basic"
  ))

ggplot(
  data=main,
  aes(x=year,y=gross_inv_chain,fill=lab)   
)+
  geom_area()+
  labs(
    title = "Evolution of investment on US infrastructures",
    x="Year",
    y="Investment (millions of 2012 $)"
  )+
  theme_minimal()
