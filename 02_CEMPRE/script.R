######################################################## 
####### ANÁLISE DO Cadastro Central de Empresas  ####### 
########################################################

#######
rm(list = ls())

library(tidyverse)
library(magrittr)
library(ggmap)
library(sp)
library(rgdal)
library(ggthemes)

####### 1. Carregando os bancos #######

###CEMPRE 2015

empresas2015 <- read_delim("[Trabalho]CEMPRE/empresa2015.csv",
                           delim = ";",
                           skip = 4,
                           na = c("", NA, "-"))

empresas2015 %<>%
  filter(!is.na(Total))

glimpse(empresas2015)

### Criando variáveis de município e de UF

empresas2015$UF <-  str_match(empresas2015$Município, "\\((.+)\\)")[,2]

empresas2015$muni <- str_match(empresas2015$Município, "^.+ ") %>% 
  str_trim()

###Banco com os municípios

munic <- read_delim("https://raw.githubusercontent.com/RCCSilva/Municipios-Brasileiros/master/Municipios_Brasileiros.csv",
                    delim = ";")

munic <- munic %>% 
  rename("muni" = `Nome do Município`) 

munic <- munic %>% 
  mutate(muni = str_trim(muni),
         UF   = str_trim(UF))

###Merge entre os bancos de dados (ADD: Long e Lat)
dados.df <- empresas2015 %>% 
  left_join(munic)

dados.df %<>%
  rename(lat = "Latitude",
         long = "Longitude")

completos.df <- dados.df %>% 
  filter(!is.na(lat))

no_comple.df <- dados.df %>% 
  filter(is.na(lat))

  
loca_no_compl.df <- geocode(no_comple.df$Município)

no_comple.df %<>%
  mutate(long = loca_no_compl.df$lon,
         lat  = loca_no_compl.df$lat,
         long = as.character(long),
         lat  = as.character(lat))

final.df <- completos.df %>% 
  bind_rows(no_comple.df)

final.df %>%
  summarise(sem_geo = sum(is.na(lat)))

###Criando as proporções das variáveis

final.df %>%
  mutate(str_replace())
  
colnames(final.df)[3:23] <- colnames(final.df)[3:23] %>% 
  str_replace("^[A-U]", "") %>% 
  str_trim

###Banco das Capitais

capitais <- read_csv2("[Base][SP]Brasil/Capitais.csv", col_names = F)

capitais %<>%
  rename(`Código IBGE` = "X3")

capitais %<>% 
  left_join(munic, by = "Código IBGE") %>% 
  mutate(lat = as.numeric(Latitude),
         long = as.numeric(Longitude))

rm(completos.df, dados.df, empresas2015, loca_no_compl.df, munic, no_comple.df)
####### 3. ANÁLISES EXPLORATÓRIAS ####### 


####### 4. MAPAS ####### 

final.df %<>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long))

brasil <- readOGR("[Base][SP]Brasil/", "UFEBRASIL")

brasil %>% 
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "darkgrey", color = "white") +
  coord_map()

ggplot() +
  geom_polygon(data = brasil, mapping = aes(x = long, y = lat, group = group),
               fill = "darkgrey", color = "white") +
  geom_point(data = final.df,mapping = aes(x = long, y = lat),
             alpha = 0.5, size = 0.5) +
  coord_map() +
  theme_map()




varia <- colnames(final.df)[3:23]

for(i in 3:23){
  final.df$prop <- (final.df[,i] / final.df$Total)
  
  final.df$cont <- final.df[,i] 
  
  
  ggplot() +
    geom_polygon(data = brasil, mapping = aes(x = long, y = lat, group = group),
                 fill = "darkgrey",
                 color = "white") +
    geom_point(data = final.df, mapping = aes(x = long,
                                              y = lat,
                                              color = prop,
                                              size = cont),
               alpha = 0.4) +
    geom_point(data = capitais, mapping = aes(x = long,
                                              y = lat),
               color = "red", alpha = 0.8) +
    coord_map() +
    theme_map() +
    labs(color = "Proporção no Município",
         size = "Quantidade",
         title = str_c(i - 2,"-", varia[i - 2]),
         caption = "Fonte: IBGE - CEMPRE")
  
  ggsave(str_c("[Trabalho]CEMPRE/g",i - 2,"_map.png"), height = 11, width = 8)
  
  final.df$prop <- NA
  
  final.df$cont <- NA
}
