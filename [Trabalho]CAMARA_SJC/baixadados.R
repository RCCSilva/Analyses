rm(list = ls())

library(tidyverse)
library(rvest)
library(httr)
library(lubridate)

url_base <- "http://portal.camarasjc.sp.gov.br:8080/cmsjc/websis/portal_transparencia/financeiro/contas_publicas/index.php?consulta=../lei_acesso/lai_remuneracoes"


baixa_camara_sjc <- function(mes, ano){
  
  mes <- ifelse(mes > 9, str_c(mes), str_c("0", mes))
  
  params <- lst(
    `competencia` = str_c(mes, "/", ano)
  )
  
  tabela <- POST(url_base, body = params) %>% 
    read_html(url_base) %>% 
    html_table() %>% 
    first()
  
  tabela <- tabela %>% 
    mutate(ano = ano,
           mes = mes)
}

serie_mensal_ls <- lst()

for(ano in 2016:2018){
  for(mes in 1:12){
    tamanho <- length(serie_mensal_ls) + 1
    serie_mensal_ls[[tamanho]] <- baixa_camara_sjc(mes,ano)
  }
}

serie_mensal_df <- bind_rows(serie_mensal_ls)

write_rds(serie_mensal_df, "[Trabalho]CAMARA_SJC/serie_mensal.rds")
