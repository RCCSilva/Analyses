rm(list = ls())

library(tidyverse)
library(rvest)
library(httr)
library()

# Web ---------------------------------------------------------------------

url_tipos <- "https://mortalidade.inca.gov.br/MortalidadeWeb/pages/Modelo06/consultar.xhtml#panelResultado"

html_session(url_tipos)
