
library(scales)
View(question)


a <- question %>% 
  left_join(client, by = c("asked_by_client_uno"="client_uno"))


totCases <- a %>% 
  count(state_abbr.x,county) %>% 
  rename("tot" = n)


resCases <- a %>% 
  filter(flag=="yes") %>% 
  count(state_abbr.x,county) %>% 
  rename("res" = n)


x <- totCases %>% 
  left_join(resCases) %>% 
  mutate(res = replace_na(res,0)) %>% 
  mutate(per = res/tot*100)


x %>% 
  rename("state_abbr"=state_abbr.x) %>% 
  mutate(county = paste0(county," County")) %>% 
  left_join(counties_sf, by=c("state_abbr"="state_abbv","county"="county_name")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(data = counties_sf %>% filter(state_abbv!= c("AK","HI")))+
  geom_sf(aes(fill = per,mapping="black"))+
  scale_fill_viridis(option = "C",direction=-1) + 
  scale_color_viridis(option = "C",direction=-1) +
  theme_minimal() +
  labs(fill="%") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        legend.key.size = unit(4, "mm")
        ) 



library(mapdata)




## By state

totCases <- a %>% 
  count(state_name) %>% 
  rename("tot" = n)


resCases <- a %>% 
  filter(flag=="yes") %>% 
  count(state_name) %>% 
  rename("res" = n)

x <- totCases %>% 
  left_join(resCases) %>% 
  mutate(res = replace_na(res,0)) %>% 
  mutate(per = res/tot*100)

x %>% 
  mutate("state_name" = tolower(state_name)) %>% 
  left_join(states, by=c("state_name"="ID")) %>%
  ggplot(aes(geometry = geom, fill = per)) +
  geom_sf(data = states,color = "#2b2b2b",fill="white", size=0.125) +
  geom_sf()+
  scale_fill_carto_c(name = "% resolved ",
                     type = "diverging", palette = "Teal",breaks=seq(0,90,10))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank())


resHour <- question %>% 
  select(asked_on_utc,taken_on_utc,category,subcategory) %>%
  filter(taken_on_utc!="NULL") %>% 
  mutate(asked_on_utc=lubridate::as_datetime(asked_on_utc),
         taken_on_utc=lubridate::as_datetime(taken_on_utc),
         diff = difftime(taken_on_utc,asked_on_utc,units = "days"))



question %>% 
  distinct(state_abbr) %>% 
  View()

hl <- mean(resHour$diff)


resHour %>% 
  group_by(category) %>% 
  summarize(time = mean(diff)) %>% 
  ggplot(aes(x=time,y=fct_reorder(category,time)))+
  geom_col(fill = "#023E8A")+
  labs(x="",y="")+
  theme_grey(base_size = 15) +
  geom_vline(xintercept = hl, linetype = "longdash") +
  labs(x="",
       y=element_blank(),
       title="Time between post and taken",
       labels= comma_format()) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank()) +
  scale_x_continuous(labels = comma_format()) +
  annotate("text", x = 10.2,y=3, label = "Average: 10 days",hjust=0)




question %>% 
  count(category,subcategory)

reply <- postClean %>% 
  filter(!str_detect(id,"[A-Za-z]")) %>% 
  group_by(question_uno) %>% 
  arrange(question_uno,id) %>% 
  mutate(x = rank(id)) %>% 
  filter(x %in% c(1,2)) %>% 
  ungroup()


r <- reply %>% 
  select(question_uno,created_utc,x) %>% 
  pivot_wider(names_from = x, values_from = created_utc) %>% 
  filter(question_uno!="NULL") %>% 
  rename("post"="1","reply"="2") %>% 
  filter("post"!='NA') %>% 
  filter(reply!='NA') %>% 
  mutate(post = lubridate::as_datetime(post),
         reply = lubridate::as_datetime(reply),
         diff = difftime(reply,post,units = "days"))
  
  
h2 <- mean(r$diff)

unhighlighted_color <- 'grey80'
highlighted_color <- '#E69F00'
  

r %>% 
  left_join(question) %>% 
  group_by(category) %>% 
  summarize(time = mean(diff)) %>% 
  mutate(cl = ifelse(time > h2, highlighted_color, unhighlighted_color)) %>% 
  ggplot(aes(x=time,y=fct_reorder(category,time)))+
  geom_col(aes(fill= cl))+
  labs(x="",y="")+
  theme_grey() +
  geom_vline(xintercept = h2, linetype = "longdash", color = "#474044") +
  labs(x="Days",
       y=element_blank(),
       labels= comma_format()) +
  C +
  scale_x_continuous(labels = comma_format()) +
  scale_fill_manual(values = c(highlighted_color, unhighlighted_color))+
  annotate("text", x = 8,y=3, label = "Average: 8 days",hjust=0)

  

# cases by ethnicity


a <- question %>% 
  left_join(client, by = c("asked_by_client_uno"="client_uno"))

library(waffle)
library(tidyverse)
library(camcorder)
library(MetBrewer)
library(ggtext)
library(stringr)

 x <- a %>% 
  mutate(year = year(asked_on_utc)) %>% 
  filter(year>2018,year!=2022) %>%
  mutate(n = str_detect(ethnic_identity,","),
         race = ifelse(str_detect(ethnic_identity,","),"Multiracial",ethnic_identity)) %>%
  filter(!race %in% c("I'd rather not answer","Other","NULL",NA,"Not Hispanic or Latino")) %>%
  mutate(race = ifelse(ethnic_identity=="Latino or Hispanic","Hispanic or Latino",race)) %>% 
  count(year,race) 
  
   
  
  
id <- x %>% 
  distinct(race) %>% 
  mutate(item_id = row_number())

x <- x %>% 
  left_join(id)

x <- x %>% 
  filter(year >2018, year!=2022) %>% 
  group_by(year) %>% 
  mutate(
    total = sum(n),
    pct = round(n / total * 100, 1),
    color = rev(met.brewer("Redon"))[item_id],
  ) %>% 
  arrange(-pct) %>% 
  ungroup()



ggplot(x, aes(fill=race, values=pct)) +
  geom_waffle(color = "white", size=1.125, n_rows = 10) +
  facet_wrap(~year, ncol=3) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal()  +
  theme(legend.key.size = unit(5, "mm"))+
  theme_enhance_waffle()








