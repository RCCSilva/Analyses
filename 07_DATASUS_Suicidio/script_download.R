setwd("07_DATASUS_Suicidio/") #Transferindo WD para a pasta do projeto

rm(list = ls())

library(magrittr)

# 1. Definindo Funções ----------------------------------------------------

url_use <- "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sim/cnv/ext10br.def"

params <- list(`Arquivos`                      = "extbr16.dbf",
               `Coluna`                        = "Grande_Grupo_CID10",
               `formato`	                     = "table",
               `Incremento`	                   = "%D3bitos_p%2FResid%EAnc",
               `Linha`	                       = "Faixa_Et%E1ria",
               `mostre`                        = "Mostra",
               `pesqmes1`                      = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes15`                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes16`                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes17`                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes18`                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes19`                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes2`	                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes3`	                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes4`	                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes5`	                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes6`	                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes7`	                     = "Digite+o+texto+e+ache+f%E1cil",
               `pesqmes8`	                     = "Digite+o+texto+e+ache+f%E1cil",
               `SAcid._Trabalho`               = "TODAS_AS_CATEGORIAS__",
               `SAmaz%F4nia_Legal`             = "TODAS_AS_CATEGORIAS__",
               `SCapital`	                     = "TODAS_AS_CATEGORIAS__",
               `SCategoria_CID10`              = "TODAS_AS_CATEGORIAS__",
               `SCor%2Fra%E7a`                 = "TODAS_AS_CATEGORIAS__",
               `SEscolaridade`                 = "TODAS_AS_CATEGORIAS__",
               `SEstado_civil`                 = "TODAS_AS_CATEGORIAS__",
               `SFaixa_de_Fronteira`           = "TODAS_AS_CATEGORIAS__",
               `SFaixa_Et%E1ria`               = "TODAS_AS_CATEGORIAS__",
               `SFaixa_Et%E1ria_det`           = "TODAS_AS_CATEGORIAS__",
               `SFaixa_Et%E1ria_OPS`           = "TODAS_AS_CATEGORIAS__",
               `SFx.Et%E1ria_Menor_1A`         = "TODAS_AS_CATEGORIAS__",
               `SGrande_Grupo_CID10`           = "TODAS_AS_CATEGORIAS__",
               `SGrupo_CID10`	                 = "TODAS_AS_CATEGORIAS__",
               `SLocal_ocorr%EAncia`           = "TODAS_AS_CATEGORIAS__",
               `SMacrorregi%E3o_de_Sa%FAde`	   = "TODAS_AS_CATEGORIAS__",
               `SMesorregi%E3o_PNDR`	         = "TODAS_AS_CATEGORIAS__",
               `SMicrorregi%E3o_IBGE`	         = "TODAS_AS_CATEGORIAS__",
               `SMunic%EDpio`                  = "TODAS_AS_CATEGORIAS__",
               `SMunic%EDpio_de_extrema_pobreza`  =	"TODAS_AS_CATEGORIAS__",
               `SRegi%E3o_de_Sa%FAde_%28CIR%29`   =	"TODAS_AS_CATEGORIAS__",
               `SRegi%E3o_Metropolitana_-_RIDE`	  = "TODAS_AS_CATEGORIAS__",
               `SSemi%E1rido`	                    = "TODAS_AS_CATEGORIAS__",
               `SSexo`                            =	"TODAS_AS_CATEGORIAS__",
               `STerrit%F3rio_da_Cidadania`	      = "TODAS_AS_CATEGORIAS__",
               `SZona_de_Fronteira`	              = "TODAS_AS_CATEGORIAS__")

params <- stringr::str_c(names(params), params, sep = "=")

params <- stringr::str_c(params, collapse = "&")

site <- httr::POST(url_use, body = params)

tabdados <- httr::content(site, encoding = "Latin1") %>%
  rvest::html_nodes(".tabdados tbody td") %>%
  rvest::html_text() %>%
  trimws()

col_tabdados <- httr::content(site, encoding = "Latin1") %>%
  rvest::html_nodes("th") %>%
  rvest::html_text() %>%
  trimws()

f1 <- function(x) x <- gsub("\\.", "", x)
f2 <- function(x) x <- as.numeric(as.character(x))

tabela_final <- as.data.frame(matrix(data = tabdados, nrow = length(tabdados)/length(col_tabdados),
                                     ncol = length(col_tabdados), byrow = TRUE))

names(tabela_final) <- col_tabdados

tabela_final[-1] <- lapply(tabela_final[-1], f1)
tabela_final[-1] <- suppressWarnings(lapply(tabela_final[-1], f2))

tabela_final

# 2. Download -------------------------------------------------------------


