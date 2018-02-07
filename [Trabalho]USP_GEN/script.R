rm(list = ls())

library(tidyverse)
library(httr)
library(rvest)
library(decryptr)
url_imag <- "https://uspdigital.usp.br/urania/CriarImagemTuring"

decryptr::captcha_download(url_imag) %>% 
  read_captcha() %>% 
  plot()
  

url_base <- "https://uspdigital.usp.br/urania/pessoaListar"

params <- lst(
  `chars`      = "1vbt",
  `codund`     =	30,
  `incalu`     = "aluno",
  `incdoc`	   = "docente",
  `incfonema`  = "sim",
  `incfun`     =	"funcionario",
  `nomabvset` 	= "",
  `texto`       = "" 
)

res_POST <- POST(url_base, body = params)

content(res_POST, as = "parsed") 

  html_node(xpath = "//table") %>% 
  html_table()
