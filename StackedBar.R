library(skimr)
library(tidyverse)
library(ggthemes)

athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')

View(athletes)

athletes %>%
  head() 

athletes %>%
  skim()

athletes %>%
  glimpse()

athletes  %>%
  count(country)


tidy_data <- athletes %>%
  group_by(abb, medal) %>%
  summarise(count_medals = n()) %>%
  filter(abb != "-") %>%
  ungroup()

tidy_data%>%
  head(10)


continent_Country <- read_csv("./Downloads/PracticeR/continent_country.csv") %>%
  select(Continent_Name, Country_Name, Three_Letter_Country_Code)


tidy_data_continent <- tidy_data %>%
  left_join(continent_Country, by = c("abb" = "Three_Letter_Country_Code") ) %>%
  filter(Continent_Name != " ")


tidy_data_continent  %>%
  head()


level_order <- factor(tidy_data_continent$medal, level = c("Silver", "Bronze", "Gold"))

ggplot(tidy_data_continent, aes(fill=level_order, y=count_medals, x=Continent_Name)) +
  geom_bar(position="fill", stat = "identity") + coord_flip() +
  scale_fill_manual(values = c(
    "Bronze" = "#A77044",
    "Gold" = "#D6AF36",
    "Silver" = "#A7A7AD"
  ))  + 
  labs(title = "Paralympics - Medals won by Continent 
       between 1980 - 2016", 
       subtitle = "Asia and North America are the continents that have 
       hitorically won more Gold Medals during the Paralympics",
       y = "Percent rate", 
       x = "Continent", 
       fill = "Type of Medal"
  )  +
  theme_fivethirtyeight() +
  theme(axis.title = element_text() )
