library(tidyverse)
library(sf)
library(lubridate)

theme_set(theme_minimal())

setwd("/home/guest/datafest/")

dPath = "/home/guest/datafest/data/"


attorney = read_csv(paste0(dPath,"attorneys.csv")) %>% 
  janitor::clean_names()
  
attorneyTimeEntry = read_csv(paste0(dPath,"attorneytimeentries.csv")) %>% 
  janitor::clean_names()

category = read_csv(paste0(dPath,"categories.csv")) %>% 
  janitor::clean_names()

client = read_csv(paste0(dPath,"clients.csv")) %>% 
  janitor::clean_names()

question = read_csv(paste0(dPath,"questions.csv")) %>% 
  janitor::clean_names()

stateSite = read_csv(paste0(dPath,"statesites.csv")) %>% 
  janitor::clean_names()

subCategory = read_csv(paste0(dPath,"subcategories.csv")) %>% 
  janitor::clean_names()

post = read_delim(paste0(dPath,"questionposts.csv"),delim = NULL) %>% 
  janitor::clean_names()


usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

## Attorney Analysis


## 1. Attorney Signups


attorneySignup <- attorney %>% 
 mutate(signupDate = as.Date(created_utc)) %>% 
  count(signupDate,sort = TRUE)
  

attorneySignup %>% 
  filter(n>=50) %>% 
  mutate(signupDate = factor(signupDate)) %>% 
  ggplot(aes(x= signupDate, y=n)) +
  geom_col()+
  labs(x = "Signup Date",
       y = "# of signups")



AtSignupPMonth <- attorneySignup %>% 
  mutate(monthYear = make_date(year(signupDate),month(signupDate),1)) %>% 
  group_by(monthYear) %>% 
  summarize(n = sum(n)) %>% 
  mutate(year = year(monthYear)) %>% 
  ungroup()


AtSignupPMonth %>% 
  filter(year==2020) %>% 
  ggplot(aes(x=monthYear, y=n)) +
  geom_col() +
  scale_x_date(NULL, date_labels = "%b %y", breaks = "month")+
  facet_wrap(~year, scales="free_x")

AtSignupPMonth %>% 
  filter(year>2015,year!=2022) %>% 
  mutate(month = month(monthYear), monthName = format(monthYear,"%b")) %>%
  mutate(monthName = fct_reorder(monthName,month)) %>% 
  ggplot(aes(x=monthName, y=n, group = year)) +
  geom_line() +
  facet_wrap(~year)



AtSignupPMonth %>% 
  filter(year>2018,year!=2022) %>% 
  mutate(month = month(monthYear), monthName = format(monthYear,"%b")) %>%
  mutate(monthName = fct_reorder(monthName,month)) %>% 
  ggplot(aes(x=monthName, y=n, group = year)) +
  geom_line(fill())

AtSignupPMonth %>% 
  filter(year==2022)

## 2. No. of attorneys by state

atByStates <- attorney |>
  
  count(state_name, sort = TRUE) %>% 
  mutate(state_name = tolower(state_name))


library(sf)
library(rcartocolor)


states <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

atByStates %>% 
  left_join(states, by=c("state_name"="ID")) %>% 
  ggplot(aes(geometry = geom, fill = n)) +
  geom_sf(data = states,color = "#2b2b2b",fill="white", size=0.125) +
  geom_sf()+
  scale_fill_carto_c(name = "# of attorneys ",
                     type = "diverging", palette = "Teal", direction = 1, breaks=c(0,200,400,600,800,1000,1200))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank())


## Client analysis
View(client)

library(albersusa)
library(viridis)

counties_sf <- get_urbn_map("counties", sf = TRUE)
remotes::install_git("https://git.rud.is/hrbrmstr/albersusa.git")

client %>% 
  count(state_abbr,county) %>% 
  mutate(county = paste0(county," County")) %>% 
  left_join(counties_sf, by=c("state_abbr"="state_abbv","county"="county_name")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(data = counties_sf %>% filter(state_abbv!= c("AK","HI")))+
  geom_sf(aes(fill = n,mapping="black"))+
  scale_fill_viridis(option = "B",direction=-1) + 
  scale_color_viridis(option = "B",direction=-1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank())



counties_sf %>% 
  count(state_name)



postClean <- post %>% 
  mutate(n = ifelse(str_detect(created_utc,"[A-Za-z]"),
                    paste0(post_text," ",created_utc),
                    post_text)) %>% 
  mutate(n = str_replace_all(n,",([^,]*$)","")) %>% 
  mutate(m = ifelse(str_detect(created_utc,"[A-Za-z]"),
                    str_extract(created_utc,"(?<=,)[^,]*$"),
                    created_utc)) %>%
  select(-post_text,-created_utc) %>% 
  rename("post_text"=n,"created_utc"=m) 
  











  






  


