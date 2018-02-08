rm(list = ls())

library(ggthemes)
library(tidyverse)
library(sp)
library(rgdal)

saopaulo <- readOGR("[Base][SP]SaoPaulo", "35MUE250GC_SIR")

saopaulo_df <- fortify(saopaulo) %>% 
  mutate(id = as.numeric(id))

saopaulo_int <- saopaulo@data %>% 
  mutate(id     = as.numeric(0:644)) %>% 
  arrange(NM_MUNICIP) %>% 
  mutate(id_alf = 1:645)
  

saopaulo_df <- saopaulo_df %>% 
  left_join(saopaulo_int, by = "id")

saopaulo_df %>% 
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = NM_MUNICIP == "SANTOS")) +
  geom_polygon(color = "white") +
  coord_map() +
  theme_map()
