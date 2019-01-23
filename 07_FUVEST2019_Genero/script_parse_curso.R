rm(list = ls())

library(tidyverse)
 
curso <- pdftools::pdf_text('relacao_candidato_vaga_completa_2019.pdf')

#Banco de dados do Curso
curso_sen <- str_split(curso, "\\n") %>% 
  unlist() %>% 
  str_trim() %>% 
  str_subset('^\\d{3}.+') %>% 
  str_extract("^\\d{3}\\s+[a-zA-Zà-úÀ-Ú ]+\\s+(\\([a-zA-Zà-úÀ-Ú ]*(\\.\\.\\.)?\\))?(−)?[a-zA-Zà-úÀ-Ú ]+") %>% 
  str_trim()

curso_sen <- curso_sen[!is.na(curso_sen)]

curso_df <- tibble(id   = str_sub(curso_sen,1,3),
                   name = str_trim(str_remove(curso_sen, '\\d{3}')))

