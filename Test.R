

library(tidyverse)
library(geojsonio)
library(broom)
library(rgeos)
library(patchwork)
library(showtext)




font_add_google("Lobster", "Lobster")
font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto Mono", "Roboto Mono")

theme_set(theme_bw(base_family = "Lobster"))
theme_update(rect = element_rect(fill = "#e8d8c3",
                                 color = "#e8d8c3"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill = "#e8d8c3",
                                             color = NA),
             panel.border = element_blank(),
             plot.background = element_rect(fill = "#e8d8c3",
                                            color = "#e8d8c3"),
             axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             legend.position = c(0.5, 0.875),
             legend.title = element_text(size = 13),
             legend.text = element_text(family = "Montserrat"),
             plot.title = element_text(size = 30,
                                       face = "bold",
                                       hjust = 0.5,
                                       margin = margin(t = 10, b = 0)),
             plot.subtitle = element_text(size = 18,
                                          color = "grey55",
                                          face = "bold",
                                          hjust = 0.5,
                                          margin = margin(t = 0, b = 15)),
             plot.caption = element_text(size = 12,
                                         color = "grey55",
                                         hjust = 0.5,
                                         margin = margin(t = 15, b = 10)))


df_nps <-
  readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
readr::read_csv(here::here("data", "2019_38", "50_us_states_all_data.csv")
                