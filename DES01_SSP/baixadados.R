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

mun_cod <- read_html(url_base) %>% 
  html_nodes(xpath = "//select[@name = 'ctl00$conteudo$ddlMunicipios']/option")

mun_cod <- tibble(cod      = as.numeric(html_attr(mun_cod, "value")),
                  nome     = html_text(mun_cod))

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
    `__EVENTTARGET`	            = "ctl00$conteudo$btnMensal",
    `__EVENTVALIDATION`         =	event_validation,
    `__LASTFOCUS`               = "",
    `__VIEWSTATE`	              = view_state,
    `ctl00$conteudo$ddlAnos`	      = ano,
    `ctl00$conteudo$ddlDelegacias`	= "0",
    `ctl00$conteudo$ddlMunicipios`	= municipio,
    `ctl00$conteudo$ddlRegioes`	    = "0"
  )

  tabela <- POST(url_base, body = params) %>% 
    read_html() %>% 
    html_table() %>% 
    first() %>% 
    mutate(municipio = municipio,
           ano       = ano)
}



baixa_bo_dp_ano <- function(){
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
    `__EVENTTARGET`	            = "ctl00$conteudo$ddlDelegacias",
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

# loop --------------------------------------------------------------------

serie_mensal_ls <- lst()

ano = 2017

for(municipio in 1:645){
    tamanho = length(serie_mensal_ls) + 1
    
    serie_mensal_ls[[tamanho]] <- baixa_bo_municipio_ano(ano, municipio)
    
}

serie_mensa_df <- bind_rows(serie_mensal_ls)

write_rds(serie_mensa_df, "[Trabalho]SSP/serie_mensal.rds")



# atribuindo nome aos municipios ------------------------------------------
rm(list = ls())

source("[Trabalho]SSP/caso_especial.R")
limpa <- function(tabela, ano){
  
  for(i in 1:nrow(tabela)){
    if(tabela[i,1] == "ITEM"){
      tabela <- tabela[i:nrow(tabela),]
      break()
    }
  }
  
  nomes <- tabela[1,] %>% as.character()
  
  nomes[is.na(nomes) | nomes %in% c('',"NA")] <- str_c("excluir", 1:sum(is.na(nomes) | nomes %in% c("", "NA")))
  
  nomes <- str_to_lower(nomes)
  colnames(tabela) <- c("ITEM", "ocorrencia", nomes[3:length(nomes)])
  
  tabela <- tabela %>% 
    select(-starts_with("excluir")) %>% 
    filter(!(str_detect(tabela$ITEM, "FONTE") | str_detect(tabela$ITEM, "\\(.*\\)")))
}

atr_item <- function(tabela){
  tabela <- tabela %>% 
    mutate(var = case_when(ITEM == "ITEM" ~ 1,
                           T  ~ 0),
           var = cumsum(var)) 
  
  itens <- tabela %>%
    filter(ITEM == "ITEM") %>% 
    rename("tipo" = "ocorrencia") %>% 
    select(tipo, var)
  
  tabela %>% 
    left_join(itens, by = "var")
}


caso_normal <- function(ano, tri){
  url_base <- "http://www.ssp.sp.gov.br/Estatistica/plantrim/ANOTRI.htm"
  
  divisor <- ifelse(ano < 2004, "_", "-")
  
  url_uso <- str_replace(url_base, "ANOTRI", str_c(ano, divisor, "0",tri))
  
  tabela <- read_html(url_uso) %>% 
    html_table(fill = T)
  
  tabela <- bind_rows(tabela)
  
  tabela <- limpa(tabela, ano)
  
  tabela <- atr_item(tabela)
  
  tabela <- tabela %>% 
    filter(ITEM != "ITEM") %>%
    filter(ocorrencia != "") %>% 
    select(-ITEM) %>% 
    gather(`capital`:`estado`, key = "local", value = "quantidade") %>% 
    mutate(ano = ano,
           tri =tri)
}

lista <- lst()

for(ano in 1996:2017){
  for(tri in 1:4){
    tamanho <- length(lista)
    if(ano == 2001){
      lista[[tamanho + 1]]<- caso_2001(tri)
    } else {
      lista[[tamanho + 1]] <- caso_normal(ano, tri)
    }
  }
}

serie_df <- bind_rows(lista)

serie_df <- serie_df %>% 
  filter(!(ocorrencia %in% c("(EM FLAGRANTE+PREVENTIVA+POR MANDADO)", "(EM FLAGRANTE+POR MANDADO)", "(EM\r\n            FLAGRANTE+POR MANDADO)",
                             ("EM FLAGRANTE+PREVENTIVA+POR MANDADO"))))

serie_df <- serie_df %>% 
  mutate(quantidade = parse_number(quantidade, locale = locale(decimal_mark = ",", grouping_mark = ".")),
         quantidade = ifelse(is.na(quantidade), 0, quantidade)) %>% 
  filter(!is.na(ano))


write_rds(serie_df, "[Trabalho]SSP/serie.rds")
