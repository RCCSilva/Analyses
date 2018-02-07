rm(list = ls())

library(tidyverse)
library(magrittr)
library(sp)
library(rgdal)
library(ggthemes)

source("[Trabalho]ENEM/FUN.R")

c_quali <- c("#e41a1c",
             "#377eb8",
             "#4daf4a",
             "#984ea3",
             "#ff7f00",
             "#ffff33",
             "#a65628",
             "#f781bf",
             "#999999",
             "#cab2d6",
             "#ffff99")


# 1. Banco de Dados -------------------------------------------------------

#Banco de dados com apenas os incristos no ENEM que compareceram nas
# 4 provas
enem.df <- read_rds("[Trabalho]ENEM/enem2013-2016_USO.rds")


# 2. Análise Exploratória -------------------------------------------------

###Distribuição das notas

####Em função do tipo de escola do Ensino Médio
enem_ens_medio <- enem.df %>%
  group_by(ano, tipo_escola) %>% 
  summarise(`Ciências Humanas` = mean(nota_cie_hum, na.rm = T),
            `Ciências Naturais` = mean(nota_cie_nat, na.rm = T),
            `Linguagens e Códigos` = mean(nota_lin_cod, na.rm = T),
            `Matemática` = mean(nota_mat, na.rm = T),
            `Redação` = mean(nota_red, na.rm = T)) %>% 
  gather(`Ciências Humanas`:`Redação`, key = "prova", value = "media")

enem_ens_medio %>%
  mutate(tipo_escola = case_when(ano %in% c(2013, 2014) & tipo_escola == 1 ~ "Pública",
                                 ano %in% c(2013, 2014) & tipo_escola == 2 ~ "Privada",
                                 ano %in% c(2015, 2016) & tipo_escola == 2 ~ "Pública",
                                 ano %in% c(2015, 2016) & tipo_escola == 3 ~ "Privada"),
         tipo_escola = as.factor(tipo_escola)) %>% 
  ggplot(mapping = aes(x = ano, y = media, color = tipo_escola)) +
  geom_point() +
  geom_line() +
  facet_wrap(~prova, ncol = 1) +
  scale_color_manual(values = c_quali,
                     labels = c("Privada", "Pública")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(color = "Tipo de Escola",
       x = "Ano",
       y = "Média",
       title = "Médias no ENEM por tipo de escola (pública x privada)",
       caption = "Fonte: INEP")

ggsave("[Trabalho]ENEM/g1_esco.png", width = 8.0, height = 15.0)


####Em função da renda familiar
enem_renda_fami <- enem.df %>%
  group_by(ano, rend_fam) %>% 
  summarise(`Ciências Humanas` = mean(nota_cie_hum, na.rm = T),
            `Ciências Naturais` = mean(nota_cie_nat, na.rm = T),
            `Linguagens e Códigos` = mean(nota_lin_cod, na.rm = T),
            `Matemática` = mean(nota_mat, na.rm = T),
            `Redação` = mean(nota_red, na.rm = T)) %>% 
  gather(`Ciências Humanas`:`Redação`, key = "prova", value = "media")

enem_renda_fami %>% 
  ggplot(mapping = aes(x = ano, y = media, color = rend_fam)) +
  geom_point() +
  geom_line() +
  facet_wrap(~prova, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

####Em função de gênero

enem_gen <- enem.df %>%
  group_by(ano, sexo) %>% 
  summarise(`Ciências Humanas` = mean(nota_cie_hum, na.rm = T),
            `Ciências Naturais` = mean(nota_cie_nat, na.rm = T),
            `Linguagens e Códigos` = mean(nota_lin_cod, na.rm = T),
            `Matemática` = mean(nota_mat, na.rm = T),
            `Redação` = mean(nota_red, na.rm = T)) %>% 
  gather(`Ciências Humanas`:`Redação`, key = "prova", value = "media")

enem_gen %>% 
  ggplot(mapping = aes(x = ano, y = media, color = sexo)) +
  geom_point() +
  geom_line() +
  facet_wrap(~prova, ncol = 1) +
  scale_color_manual(values = c_quali,
                     labels = c("Feminino", "Masculino")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(color = "Sexo",
       x = "Ano",
       y = "Média",
       title = "Médias no ENEM por sexo",
       caption = "Fonte: INEP")

ggsave("[Trabalho]ENEM/g2_sexo.png", width = 8.0, height = 15.0)

###Em função de raça

enem_raca <- enem.df %>% 
  group_by(ano, cor_raca) %>% 
  summarise(`Ciências Humanas` = mean(nota_cie_hum, na.rm = T),
            `Ciências Naturais` = mean(nota_cie_nat, na.rm = T),
            `Linguagens e Códigos` = mean(nota_lin_cod, na.rm = T),
            `Matemática` = mean(nota_mat, na.rm = T),
            `Redação` = mean(nota_red, na.rm = T))  %>% 
  gather(`Ciências Humanas`:`Redação`, key = "prova", value = "media")

enem_raca %>% 
  filter(!(cor_raca %in% c(0,6))) %>% 
  ggplot(mapping = aes(x = ano, y = media, color = as.factor(cor_raca))) +
  geom_point() +
  geom_line() +
  facet_wrap(~prova, ncol = 1) +
  scale_color_manual(values = c_quali,
                     labels = c("Branca", "Preta", "Parda", "Amarela", "Indígena")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(color = "Cor/Raça",
       x = "Ano",
       y = "Média",
       title = "Médias no ENEM por cor/raça",
       caption = "Fonte: INEP")

ggsave("[Trabalho]ENEM/g3_raca.png", width = 8.0, height = 15.0)


# 3. Mapas e diferenças por municípios ------------------------------------

# 3.1. Mapa Base
brasil <- readOGR("[Base][SP]Brasil/", "UFEBRASIL")

# 3.2. Municípios e geolocalização
municipios_brasil <- read_csv("[Base][SP]Brasil/Municipios_Brasileiros.csv")

# 3.3. Banco de dados base para confecção dos mapas
enem_cidades <- enem.df %>% 
  group_by(cod_mun, ano) %>% 
  summarise(media_ch = mean(nota_cie_hum, na.rm = T),
            media_cn = mean(nota_cie_nat, na.rm = T),
            media_lc = mean(nota_lin_cod, na.rm = T),
            media_mt = mean(nota_mat, na.rm = T),
            media_rd = mean(nota_red, na.rm = T))

enem_cidades <- enem_cidades %>% 
  group_by(ano) %>% 
  mutate(diff_ch = media_ch - mean(media_ch, na.rm = T),
         diff_cn = media_cn - mean(media_cn, na.rm = T),
         diff_lc = media_lc - mean(media_lc, na.rm = T),
         diff_mt = media_mt - mean(media_mt, na.rm = T),
         diff_rd = media_rd - mean(media_rd, na.rm = T))

enem_cidades <- enem_cidades %>% 
  left_join(municipios_brasil, by = "cod_mun")

enem_cidades <- enem_cidades %>% 
  ungroup()

enem_cidades %>% 
  select(diff_ch:diff_rd) %>% 
  map(range)

# 3.4. GIFS das diferenças dos municípios em relação à média
cria_gif(enem_cidades,
         brasil,
         "diff_cn",
         "Ciências Naturais")

cria_gif(enem_cidades,
         brasil,
         "diff_ch",
         "Ciências Humanas")

cria_gif(enem_cidades,
         brasil,
         "diff_lc",
         "Linguagens e Códigos")

cria_gif(enem_cidades,
         brasil,
         "diff_mt",
         "Matemática")

cria_gif(enem_cidades,
         brasil,
         "diff_rd",
         "Redação")
