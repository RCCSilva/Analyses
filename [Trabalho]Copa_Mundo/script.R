rm(list = ls())

library(tidyverse)
library(rgdal)
library(sp)

serie_df <- read_rds("[Trabalho]Copa_Mundo/serie.rds")

serie_df %>% 
  ggplot(mapping = aes(x = ano, y = attendance)) +
  geom_line()

serie_df %>% 
  ggplot(mapping = aes(x = ano, y = teams)) +
  geom_line()

serie_df %>% 
  ggplot(mapping = aes(x = ano, y = goals_scored/matches)) +
  geom_line()

mundo <- readOGR("[Base][SP]Mundo/", "TM_WORLD_BORDERS-0.3")

plot(mundo)

mundo %>% 
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "white", fill = "darkgrey")

mundo@data
