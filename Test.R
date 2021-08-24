

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
  geom_text(
    data=final,              
    aes(
      y=ypos-150000,         # Decrease label y position 
      label=lab),   
    x=2018, 
    hjust=0                  # Left align text
  )+
  scale_x_continuous(
    limits=c(1947,2022),     # Expand x axis to leave space for labels
    breaks=c(1950,1980,2010)
  )+
  guides(
    fill=FALSE                # No need for fill legend anymore !
  )+
  theme_minimal()


final<-main%>%
  filter(year=="2017")%>%              # Keep only 2017 value
  arrange(desc(lab))%>%                # Inverse factor order (first is at the bottom of plot)
  mutate(                              # Create new column ypos and
    ypos=cumsum(gross_inv_chain)       # fill with cumulative sum of invest for 2017
  )                                     


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
  geom_text(
    data=final,              
    aes(
      y=ypos-150000,         # Decrease label y position 
      label=lab),   
    x=2018, 
    hjust=0                  # Left align text
  )+
  scale_x_continuous(
    limits=c(1947,2022),     # Expand x axis to leave space for labels
    breaks=c(1950,1980,2010,2017)
  )+
  guides(
    fill=FALSE                # No need for fill legend anymore !
  )+
  theme_minimal()

max(main$year)




# Create color palette
pal<-c("#0F4C5C","#E36414","#9A031E")

# Specify color palette with a new column inside main
main<-main%>%
  mutate(
    col_lab=case_when(
      lab=="Basic"~"#0F4C5C",
      lab=="Social"~"#E36414",
      lab=="Digital"~"#9A031E"
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
  geom_text(
    data=final,              
    aes(
      y=ypos-150000,         
      label=lab,
      color=lab            # Add color inside the aes()
    ),   
    x=2018, 
    hjust=0                  
  )+
  scale_fill_manual(       # Specify fill palette
    breaks=main$lab,values=main$col_lab
  )+
  scale_color_manual(      # Same palette for color
    breaks=main$lab,values=main$col_lab
  )+
  scale_x_continuous(
    limits=c(1947,2022),     
    breaks=c(1950,1980,2010)
  )+
  guides(
    fill=FALSE,
    color=FALSE             # Hide color legend
  )+
  theme_minimal()
