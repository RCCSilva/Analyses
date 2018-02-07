rm(list = ls())

library(tidyverse)
library(rvest)

lista <- lst()

for(i in 2012:2017){
  url_base <- "https://www.cbf.com.br/competicoes/brasileiro-serie-a/tabela/ANO"
  
  url_uso <- str_replace(url_base, "ANO", str_c(i))
  
  resp_html <- read_html(url_uso)
  
  node_cont <- html_node(resp_html, xpath = "//table")
  
  br_df <- html_table(node_cont)
  
  lista[[(i - 2011)]] <- br_df
  
}

serie_df <- bind_rows(lista)

write_rds(serie_df, "[Trabalho]Brasileirao/serie.rds")