library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

theme_set(theme_light())

#data <- read.delim("~/Downloads/sliced/JobOpenings/JobOpenings.txt")

openings <- read.delim(url("https://download.bls.gov/pub/time.series/jt/jt.data.2.JobOpenings")) %>% 
  mutate(series_id = str_trim(series_id)) %>% 
  filter(year >= 2021, series_id == 'JTS000000000000000JOL')


quits <- read.delim(url("https://download.bls.gov/pub/time.series/jt/jt.data.5.Quits")) %>% 
  mutate(series_id = str_trim(series_id)) %>% 
  filter(year >= 2021, series_id == 'JTS000000000000000QUL')


layoffs <- read.delim(url("https://download.bls.gov/pub/time.series/jt/jt.data.6.LayoffsDischarges")) %>% 
  mutate(series_id = str_trim(series_id)) %>% 
  filter(year >= 2021, series_id == 'JTS000000000000000LDL')

to_date <- function(tbl) {

  tbl %>% 
  mutate(x = paste0(year,str_remove(period,"M"),'01'),
         per = lubridate::ymd(x)) %>% 
  select(-c("series_id","year","footnote_codes","x","period"))

}
  
pal <- c(
  rep("gray70", length(data$per) - 1), 
  "goldenrod1"
)


plot_data <- function(tbl) {

m <- round(max(tbl$value)/1000) 
  
tbl %>% 
  ggplot(aes(x = per, y = value/1000, fill = as.factor(per))) +
  geom_col() +
  scale_x_date(labels = date_format("%b'%y")) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(
    breaks = seq(0,m,2),
    expand = c(0,0)
  ) +
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    plot.title = element_text(size = 24),
    panel.grid.major.x= element_blank(),
    panel.grid.minor = element_blank(),
    panel.border  = element_blank(),
    axis.line.x = element_line(size = .5),
    axis.text = element_text(size = 16),
    axis.text.y = element_text(vjust = -.5),
    axis.title = element_blank(),
    axis.ticks.length.x = unit(.3,"cm"),
    axis.ticks.x =  element_line(size = .5, colour = "black"),
    plot.caption = element_text(hjust = 0, size = 10),
    ) 
  
}


o <- openings %>% 
  to_date() %>% 
  plot_data() +
  labs(title = "U.S. job openings, monthly (million)",
       caption = "Note: Seasonally adjusted. Jan.2022 data are preliminary.\nSource: Labour Department")



q <- quits %>% 
  to_date() %>% 
  plot_data() +
  labs(title = "Quits (million)")

l <- layoffs %>% 
  to_date() %>% 
  plot_data() +
  labs(title = "Layoffs and discharges (million)")


(o | (l / q)) 
