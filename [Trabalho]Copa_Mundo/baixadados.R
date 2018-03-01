rm(list = ls())

library(tidyverse)
library(rvest)
library(data.table)
library(lubridate)

sedes_copas_mundo <- read_html("http://www.fifa.com/fifa-tournaments/archive/worldcup/index.html") %>% 
  html_nodes(xpath = "//a[@class='link-wrap']") %>% 
  html_attr("href") %>% 
  str_split("/") %>% 
  map_chr(~.[4])

copas_mundo_lst <- lst()

for(i in seq_along(sedes_copas_mundo)){
  url_base = "http://www.fifa.com/worldcup/archive/PAIS/index.html"
  
  url_uso  = str_replace(url_base, "PAIS", sedes_copas_mundo[i])
  
  page_html <- read_html(url_uso) %>% 
    html_nodes(xpath = "//div[@class = 'competitions-stats-main-data']/div/div")
  
  var_name <- page_html %>% 
    html_attr("class") %>% 
    str_replace_all("num", "") %>% 
    str_trim()
  
  valores <- page_html %>% 
    html_text()
  
  valores <- c(valores, sedes_copas_mundo[i])
  
  names(valores) <- c(var_name, "pais_ano")
  
  copas_mundo_lst[[i]] <- valores
} 

serie_df <- rbind_list(copas_mundo_lst)

serie_df <- serie_df %>% 
  map_df(parse_guess) %>% 
  map_df(parse_guess) %>% 
  mutate(pais = str_extract(pais_ano, "[a-z]+"),
         ano  = str_extract(pais_ano, "\\d{4}") %>% parse_number())

write_rds(serie_df, "[Trabalho]Copa_Mundo/serie.rds")

gols_times <- lst()

for(i in seq_along(sedes_copas_mundo)){
  url_base <- "http://www.fifa.com/worldcup/archive/PAIS/statistics/teams/goal-scored.html"
  
  url_uso <- str_replace(url_base, "PAIS", sedes_copas_mundo[i])
  
  tabela <- read_html(url_uso) %>% 
    html_table() %>%
    first()
  
  colnames(tabela) <- str_replace(colnames(tabela), "▴▾", "")
  
  colnames(tabela)[colnames(tabela) == "Teams"] <- c("nome_time", "nome_time_iso")
  
  gols_times[[i]] <- tabela %>% 
    mutate(pais_nome = sedes_copas_mundo[i])
}

rbind_list(gols_times) %>% View()
