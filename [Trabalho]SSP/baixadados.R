#Função para baixar dados da Secretaria de Segurança Pública de São Paulo
# Autor Rafael de Castro Coelho Silva
# Baseado no Blog Curso-R


# 1. Limpando ambiente e carregando pacotes -------------------------------

rm(list = ls())

library(tidyverse)
library(httr)
library(rvest)


# 2. Função ---------------------------------------------------------------

url_base <- "http://www.ssp.sp.gov.br/Estatistica/Pesquisa.aspx"

baixa_bo_municipio_ano  <- function(ano, municipio){
  
  pivot <- GET(url_base)
  
  view_state <- pivot %>% 
    read_html() %>% 
    html_nodes("input[name='__VIEWSTATE']") %>% 
    html_attr("value")
  
  event_validation <- pivot %>% 
    read_html() %>% 
    html_nodes("input[name='__EVENTVALIDATION']") %>% 
    html_attr("value")
  
  params <- lst(
    `__EVENTARGUMENT`           = "",
    `__EVENTTARGET`	            = "ctl00$conteudo$ddlMunicipios",
    `__EVENTVALIDATION`         =	event_validation,
    `__LASTFOCUS`               = "",
    `__VIEWSTATE`	              = view_state,
    `ctl00$conteudo$ddlAnos`	      = ano,
    `ctl00$conteudo$ddlMunicipios`	= municipio,
    `ctl00$conteudo$ddlRegioes`	    = "0",
    `ctl00$conteudo$ddlDelegacias`	= "0"
  )
  
  
  tabela <- POST(url_base, params) %>% 
    read_html() %>% 
    html_table() %>% 
    first() %>% 
    mutate(municipio = municipio,
           ano       = ano)
}

mun_cod <- read_html(url_base) %>% 
  xml_find_all(xpath = "//select[@name = 'ctl00$conteudo$ddlMunicipios']/option")

mun_cod <- tibble(cod = as.numeric(html_attr(mun_cod, "value")),
                  nome= xml_text(mun_cod))

# loop --------------------------------------------------------------------

dados <- lst()

for(ano in 2001:2017){
  for(i in 1:655){
    dados[[i]] <- baixa_bo_municipio_ano(str_c(ano), str_c(i))
  }
  
  dados <- bind_rows(dados)
  
  dados %>% 
    left_join(mun_cod, by = c("muni" = "cod")) %>% 
    write_rds(str_c("dados",ano,".rds"))
}




# atribuindo nome aos municipios ------------------------------------------




