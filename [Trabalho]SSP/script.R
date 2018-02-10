rm(list = ls())

library(ggthemes)
library(tidyverse)
library(sp)
library(rgdal)
library(readxl)

saopaulo <- readOGR("[Base][SP]SaoPaulo", "35MUE250GC_SIR")

saopaulo_df <- fortify(saopaulo) %>% 
  mutate(id = as.numeric(id))

saopaulo_int <- saopaulo@data %>% 
  mutate(id     = as.numeric(0:644)) %>% 
  arrange(NM_MUNICIP) %>% 
  mutate(id_alf = 1:645)
  
pop_sao_paulo <- read_xlsx("[Trabalho]SSP/pop_esti_mun2001-2017.xlsx",
                           skip = 3) %>% 
  rename(mun = X__1) %>% 
  select("mun", "2017") %>% 
  mutate(lista = str_extract_all(mun, boundary("word")))

pop_sao_paulo$uf <- NA
pop_sao_paulo$cidade <- NA

for(i in seq_along(pop_sao_paulo$lista)){
  pop_sao_paulo$uf[i] <- pop_sao_paulo$lista[[i]][length(pop_sao_paulo$lista[[i]])]
  
  pop_sao_paulo$cidade[i] <- str_c(pop_sao_paulo$lista[[i]][-length(pop_sao_paulo$lista[[i]])], collapse = " ")
}

pop_sao_paulo <- pop_sao_paulo %>% 
  filter(uf == "SP") %>% 
  mutate(cidade = str_to_upper(cidade))

saopaulo_int <- saopaulo_int %>% 
  left_join(pop_sao_paulo, by = c("NM_MUNICIP" = "cidade"))

serie_mensal_df <- read_rds("[Trabalho]SSP/serie_mensal.rds")

serie_mensal_df <- serie_mensal_df %>% 
  select(Natureza, Total, municipio, ano)

total_mun_2017 <- serie_mensal_df %>% 
  group_by(ano, municipio, Natureza) %>% 
  summarise(soma = sum(quantidade)) %>% 
  left_join(saopaulo_int, by = c("municipio" = "id_alf")) %>% 
  mutate(taxa = (soma/`2017`) * 10000) %>% 
  select(ano, municipio, uf, id, Natureza, taxa) %>% 
  spread(key = "Natureza", value = "taxa")

saopaulo_df <- saopaulo_df %>% 
  left_join(total_mun_2017, by = "id")

saopaulo_df %>% 
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = split(`HOMICÍDIO DOLOSO (2)`, f = c("baixo")))) +
  geom_polygon(color = "white") +
  coord_map() +
  theme_map() +
  scale_fill_discrete()
