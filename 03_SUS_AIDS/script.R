rm(list = ls())

library(tidyverse)
library(magrittr)
library(sp)
library(rgdal)
library(ggthemes)

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

###Frequência geral
aids_geral <- read_csv2("[Trabalho]AIDS/aids_geral.csv") %>% 
  rename(ano = `Ano Diagnóstico`,
         freq= Freqüência)

aids_geral %>% 
  ggplot(mapping = aes(x = ano, y = freq)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Gráfico 1 - Diagnósticos de AIDS ao longo dos anos",
       caption = "Fonte: TABNET/DATASUS",
       x = "Ano",
       y = "Frequência")

ggsave("[Trabalho]AIDS/g1_freq.png", width = 8.0, height = 5.0)

###Frequência por sexo
aids_sexo <- read_csv2("[Trabalho]AIDS/aids_sexo.csv") %>% 
  rename(ano = `Ano Diagnóstico`,
         fem = Feminino,
         mas = Masculino,
         branco= `Em Branco`,
         total = Total)

aids_sexo %<>% 
  gather(fem, mas, branco, key = "sexo", value = "freq")

aids_sexo %>% 
  ggplot(mapping = aes(x = ano, y = freq, fill = sexo)) +
  geom_bar(stat = "identity", position = "fill")+
  theme_minimal() +
  scale_fill_manual(labels = c("Sem informação", "Feminino", "Masculino"),
                    values = c_quali) +
  labs(title = "Gráfico 2 - Diagnósticos de AIDS por sexo",
       fill = "Sexo",
       x = "Ano",
       y = "Proporção",
       caption = "Fonte: TABNET/DATASUS") +
  theme(legend.position = "bottom")

ggsave("[Trabalho]AIDS/g2_sexo.png",  width = 8.0, height = 5.0)


###Frequência por escolaridade

aids_esco <- read_csv2("[Trabalho]AIDS/aids_educ.csv")

aids_esco %<>% 
  rename(ano = `Ano Diagnóstico`)

aids_esco %<>% 
  gather(`1ª a 4ª série incompleta`:`não se aplica`, key = "Escolaridade", value = "freq")

aids_esco %>% 
  ggplot(mapping = aes(x = ano, y = freq, fill = Escolaridade)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c_quali,
                    labels = c("1ª a 4ª série incompleta",
                               "4ª série completa",
                               "5ª a 8ª série incompleta",
                               "Fundamental completo",
                               "Médio completo",
                               "Médio incompleto",
                               "Não se aplica",
                               "Superior completo",
                               "Superior Incompleto")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Gráfico 3 - Diagnósticos de AIDS por escolaridade",
       x = "Ano",
       y = "Proporção",
       caption = "Fonte: TABNET/DATASUS")

ggsave("[Trabalho]AIDS/g3_esco.png",  width = 8.0, height = 5.0)

###Frequência por Raça

aids_raca <- read_csv2("[Trabalho]AIDS/aids_raca.csv")

aids_raca %<>% 
  rename(ano = `Ano Diagnóstico`)

aids_raca %<>% 
  gather(Branca:Ignorado, key = "raca", value = "freq")


aids_raca %>% 
  mutate(raca = factor(raca, levels = c("Amarela", "Branca", "Parda", "Preta", "Indígena", "Ignorado"))) %>% 
  ggplot(mapping = aes(x = ano, y = freq, fill = raca)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_minimal() +
  scale_fill_manual(values = c("#e41a1c",
                               "#377eb8",
                               "#4daf4a",
                               "#984ea3",
                               "#ff7f00",
                               "#cab2d6")) +
  labs(title = "Gráfico 4 - Diagnósticos de AIDS por cor/raça",
       x = "Ano",
       y = "Proporção",
       fill = "Cor/Raça",
       caption = "Fonte: TABNET/DATASUS")+
  theme(legend.position = "bottom")

ggsave("[Trabalho]AIDS/g4_raca_lim.png",  width = 8.0, height = 5.0)

aids_raca %>% 
  filter(raca != "Ignorado") %>% 
  ggplot(mapping = aes(x = ano, y = freq, fill = raca)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_minimal() +
  scale_fill_manual(values = c("#e41a1c",
                               "#377eb8",
                               "#4daf4a",
                               "#984ea3",
                               "#ff7f00",
                               "#cab2d6")) +
  labs(title = "Gráfico 5 - Diagnósticos de AIDS por cor/raça",
       subtitle = 'Sem a categoria "ignorado"',
       fill = "Cor/Raça",
       x = "Ano",
       y = "Proporção",
       caption = "Fonte: TABNET/DATASUS") +
  theme(legend.position = "bottom")

ggsave("[Trabalho]AIDS/g5_raca_lim.png",  width = 8.0, height = 5.0)



###Frequência por município
aids_mun <- read_rds("[Trabalho]AIDS/aids_mun1980-2016.rds")

brasil_mun <- read_delim("[Base][SP]Brasil/Municipios_Brasileiros.csv",
                         delim = ",")

brasil <- readOGR("[Base][SP]Brasil/", "UFEBRASIL")

brasil_cap <- read_delim("[Base][SP]Brasil/Capitais.csv",
                       delim = ";", col_names = F) %>% 
  rename(cod_mun = X3)

brasil_cap <- brasil_cap %>% 
  left_join(brasil_mun) %>% 
  mutate(lat = as.double(lat),
         long = as.double(long))

aids_mun %<>%
  rename(muni = `Município (Res)`,
         freq = Freqüência)

aids_mun %<>% 
  mutate(cod_mun = str_extract(muni, "[0-9]{6}"),
         cod_mun = as.double(cod_mun))

brasil_mun %<>% 
  mutate(cod_6d = str_extract(cod_mun, "[0-9]{6}"),
         cod_6d = as.numeric(cod_6d))

aids_mun.df <- aids_mun %>% 
  left_join(brasil_mun, by = c("cod_mun" = "cod_6d"))

aids_mun.df %<>%
  mutate(lat = as.double(lat),
         long = as.double(long),
         ano = as.double(ano))

for(i in c(1980, 1982:2016)){
  aids_mun_uso <- aids_mun.df %>% 
    filter(ano == i)
  
  ggplot() +
    geom_polygon(data = brasil, mapping = aes(x = long, y = lat, group = group),
                 fill = "darkgrey", color = "white") +
    geom_point(data = aids_mun_uso, mapping = aes(x = long, y = lat, size = freq),
               alpha = 0.15, color =  "#ae003d") +
    geom_point(data = brasil_cap, mapping = aes(x = long, y = lat, color = "black"), 
               size = 0.2) +
    coord_map() +
    theme_map() +
    labs(size = "Frequência",
         title = str_c("Diagnósticos de AIDS por município de residência:",i, sep = " "),
         caption = "Fonte: TABNET/DATASUS",
         color = "") +
    scale_color_manual(values = "black",
                       labels = c("Capitais")) +
    scale_size_continuous(limits = c(1,3000),
                          breaks = c(1, 500, 1000, 2000, 3000),
                          range  = c(0, 6))
  
  ggsave(str_c("[Trabalho]AIDS/aids_",i,"_brasil.png"), height = 7.0, width = 7.0)
}

system("convert -delay 60 [Trabalho]AIDS/*brasil.png [Trabalho]AIDS/aids_brasil.gif")
