rm(list = ls())

library(tidyverse)
library(httr)

url_aids <- "http://www2.aids.gov.br/cgi/tabcgi.exe?tabnet/br.def"

params <- lst(`Linha`                 = "Munic%EDpio_%28Res%29",
               `Coluna`                = "--N%E3o-Ativa--",
               `Incremento`            = "Freq%FC%EAncia",
               `Arquivos`              = "aids_82.dbf",
               `SAno_Diagn%F3stico`    = "TODAS_AS_CATEGORIAS__",
               `SAno_Notifica%E7%E3o`  = "TODAS_AS_CATEGORIAS__",
               `SUF_Resid%EAncia`      = "TODAS_AS_CATEGORIAS__",
               `SRegi%E3o_Res.`        = "TODAS_AS_CATEGORIAS__",
               `SUF_Notifica%E7%E3o`   = "TODAS_AS_CATEGORIAS__",
               `SRegi%E3o_Not.`        = "TODAS_AS_CATEGORIAS__",
               `SFx._Et%E1ria%2811%29` = "TODAS_AS_CATEGORIAS__",
               `SFx._Et%E1ria%2813%29` = "TODAS_AS_CATEGORIAS__",
               `SFx._Et%E1ria%28SINAN%29` = "TODAS_AS_CATEGORIAS__",
               `SIdade_detalhada`         = "TODAS_AS_CATEGORIAS__",
               `SSexo`                    = "TODAS_AS_CATEGORIAS__",
               `SRa%E7a%2Fcor`            = "TODAS_AS_CATEGORIAS__",
               `SEscolaridade`            = "TODAS_AS_CATEGORIAS__",
               `SCateg_Exp_Hierar`        = "TODAS_AS_CATEGORIAS__",
               `SMunic%EDpio%28Res%29`    = "TODAS_AS_CATEGORIAS__",
               `SCapital_(Res)`           = "TODAS_AS_CATEGORIAS__",
               `SRegi%E3o_Met._%28Res%29` = "TODAS_AS_CATEGORIAS__",
               `SMunic%EDpio%28Not%29`    = "TODAS_AS_CATEGORIAS__",
               `SCapital_(Not)`           = "TODAS_AS_CATEGORIAS__",
               `SRegi%E3o_Met._%28Not%29` = "TODAS_AS_CATEGORIAS__",
               `SOrigem_dos_Dados`        = "TODAS_AS_CATEGORIAS__",
               `formato`                  = "prn",
               `mostre`                   = "Mostra")

teste <- POST(url_aids, body = params, encode = "json")

content(teste)
