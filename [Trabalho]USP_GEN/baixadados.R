rm(list = ls())

library(tidyverse)
library(rvest)
library(httr)

url_GET <- "https://uspdigital.usp.br/portaltransparencia/informacaoServidorSexo"
url_POST <- "https://uspdigital.usp.br/portaltransparencia/dwr/call/plaincall/InformacaoServidorControleDWR.listarInfoServidor.dwr"

posit <- GET(url_GET)

posit$cookies

params <- lst(
  `callCount`               = "1",
  `windowName`              = "",
  `nextReverseAjaxIndex`    = "0",
  `c0-scriptName`           = "InformacaoServidorControleDWR",
  `c0-methodName`           = "listarInfoServidor",
  `c0-id`                   = "0",
  `c0-param0`               = "SPlistarInfoServidorSexo",
  `c0-param1`               = "07%2F01%2F2015",
  `c0-param2`               = "",
  `c0-param3`               = "",
  `batchId`                 = "3",
  `instanceId`              = "0",
  `page`                    = "%2Fportaltransparencia%2FinformacaoServidorSexo",
  `scriptSessionId`         = posit$cookies$value)

POST(url_base, body = params, encode = "json") %>% 
  content() %>% 
  writeLines()
