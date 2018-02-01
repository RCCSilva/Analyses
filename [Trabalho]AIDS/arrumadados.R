rm(list = ls())

library(tidyverse)
library(magrittr)

ler_dados <- function(){
  csv_files <- list.files("[Trabalho]AIDS/", pattern = "aids_mun")
  
  str_extract(csv_files, "[0-9]{4}")
  
  arquivo <- read_delim("[Trabalho]AIDS/aids_mun1980.csv",
                       delim = ";")
  
  arquivo %<>%
    mutate(ano = "1980")
  
  for(i in 2:length(csv_files)){
    meio <- read_delim(str_c("[Trabalho]AIDS/", csv_files[i]),
                       delim = ";")
    
    meio %<>%
      mutate(ano = str_extract(csv_files[i], "[0-9]{4}"))
    
    arquivo <- arquivo %>% 
      bind_rows(meio)
  }
  
  write_rds(arquivo, "[Trabalho]AIDS/aids_mun1980-2016.rds")
}

system("convert -delay 60 *.png aids_brasil.gif")
