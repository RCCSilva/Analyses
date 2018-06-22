rm(list = ls())

library(tidyverse)
library(magrittr)

enem <- read_delim("[Trabalho]ENEM/MICRODADOS_ENEM_2013.csv",
                   delim = ";", 
                   locale = locale(encoding = "ISO-8859-1"),
                   escape_double = FALSE,
                   trim_ws = TRUE,
                   col_types = cols_only(NU_ANO     = col_double(),
                                         COD_MUNICIPIO_RESIDENCIA = col_double(),
                                         IDADE      = col_double(),
                                         TP_SEXO    = col_character(),
                                         TP_ESCOLA  = col_double(),
                                         TP_COR_RACA= col_double(),
                                         IN_PRESENCA_CN   = col_double(),
                                         IN_PRESENCA_CH   = col_double(),
                                         IN_PRESENCA_LC   = col_double(),
                                         IN_PRESENCA_MT   = col_double(),
                                         IN_STATUS_REDACAO= col_double(),
                                         NOTA_CN    = col_double(),
                                         NOTA_CH    = col_double(),
                                         NOTA_LC    = col_double(),
                                         NOTA_MT    = col_double(),
                                         NU_NOTA_REDACAO = col_double(),
                                         Q003            = col_character()))

write_rds(enem, "[Trabalho]ENEM/enem2013.rds")

rm(enem)

enem <- read_delim("[Trabalho]ENEM/MICRODADOS_ENEM_2014.csv",
                   delim = ",", 
                   locale = locale(encoding = "ISO-8859-1"),
                   escape_double = FALSE,
                   trim_ws = TRUE,
                   col_types = cols_only(NU_ANO     = col_double(),
                                         COD_MUNICIPIO_RESIDENCIA = col_double(),
                                         IDADE      = col_double(),
                                         TP_SEXO    = col_character(),
                                         TP_ESCOLA  = col_double(),
                                         TP_COR_RACA= col_double(),
                                         IN_PRESENCA_CN   = col_double(),
                                         IN_PRESENCA_CH   = col_double(),
                                         IN_PRESENCA_LC   = col_double(),
                                         IN_PRESENCA_MT   = col_double(),
                                         IN_STATUS_REDACAO= col_double(),
                                         NOTA_CN    = col_double(),
                                         NOTA_CH    = col_double(),
                                         NOTA_LC    = col_double(),
                                         NOTA_MT    = col_double(),
                                         NU_NOTA_REDACAO = col_double(),
                                         Q003            = col_character()))

write_rds(enem, "[Trabalho]ENEM/enem2014.rds")

rm(enem)

enem <- read_delim("[Trabalho]ENEM/MICRODADOS_ENEM_2015.csv",
                   delim = ",", 
                   locale = locale(encoding = "ISO-8859-1"),
                   escape_double = FALSE,
                   trim_ws = TRUE,
                   col_types = cols_only(NU_ANO     = col_double(),
                                         CO_MUNICIPIO_RESIDENCIA= col_double(),
                                         NU_IDADE   = col_double(),
                                         TP_SEXO    = col_character(),
                                         TP_ESCOLA  = col_double(),
                                         TP_COR_RACA= col_double(),
                                         TP_PRESENCA_CN = col_double(),
                                         TP_PRESENCA_CH = col_double(),
                                         TP_PRESENCA_LC = col_double(),
                                         TP_PRESENCA_MT = col_double(),
                                         TP_STATUS_REDACAO = col_double(),
                                         NU_NOTA_CN = col_double(),
                                         NU_NOTA_CH = col_double(),
                                         NU_NOTA_LC = col_double(),
                                         NU_NOTA_MT = col_double(),
                                         NU_NOTA_REDACAO = col_double(),
                                         Q006            = col_character()))

write_rds(enem, "[Trabalho]ENEM/enem2015.rds")

rm(enem)

enem <- read_delim("[Trabalho]ENEM/MICRODADOS_ENEM_2016.csv",
                   delim = ";", 
                   locale = locale(encoding = "ISO-8859-1"),
                   escape_double = FALSE,
                   trim_ws = TRUE,
                   col_types = cols_only(NU_ANO     = col_double(),
                                         CO_MUNICIPIO_RESIDENCIA= col_double(),
                                         NU_IDADE   = col_double(),
                                         TP_SEXO    = col_character(),
                                         TP_ESCOLA  = col_double(),
                                         TP_COR_RACA= col_double(),
                                         TP_PRESENCA_CN = col_double(),
                                         TP_PRESENCA_CH = col_double(),
                                         TP_PRESENCA_LC = col_double(),
                                         TP_PRESENCA_MT = col_double(),
                                         TP_STATUS_REDACAO = col_double(),
                                         NU_NOTA_CN = col_double(),
                                         NU_NOTA_CH = col_double(),
                                         NU_NOTA_LC = col_double(),
                                         NU_NOTA_MT = col_double(),
                                         NU_NOTA_REDACAO = col_double(),
                                         Q006            = col_character()))

write_rds(enem, "[Trabalho]ENEM/enem2016.rds")

rm(list = ls())

enem2013 <- read_rds("[Trabalho]ENEM/enem2013.rds")
enem2014 <- read_rds("[Trabalho]ENEM/enem2014.rds")
enem2015 <- read_rds("[Trabalho]ENEM/enem2015.rds")
enem2016 <- read_rds("[Trabalho]ENEM/enem2016.rds")

enem2013 %<>%
  rename(ano          = "NU_ANO",
         cod_mun      = "COD_MUNICIPIO_RESIDENCIA",
         idade        = "IDADE",
         sexo         = "TP_SEXO",
         tipo_escola  = "TP_ESCOLA" ,
         cor_raca     = "TP_COR_RACA",
         pres_cie_nat = "IN_PRESENCA_CN",
         pres_cie_hum = "IN_PRESENCA_CH",
         pres_lin_cod = "IN_PRESENCA_LC",
         pres_mat     = "IN_PRESENCA_MT",
         stat_red     = "IN_STATUS_REDACAO",
         nota_cie_nat = "NOTA_CN",
         nota_cie_hum = "NOTA_CH",
         nota_lin_cod = "NOTA_LC",
         nota_mat     = "NOTA_MT",
         nota_red     = "NU_NOTA_REDACAO",
         rend_fam     = "Q003")

enem2014 %<>%
  rename(ano          = "NU_ANO",
         cod_mun      = "COD_MUNICIPIO_RESIDENCIA",
         idade        = "IDADE",
         sexo         = "TP_SEXO",
         tipo_escola  = "TP_ESCOLA" ,
         cor_raca     = "TP_COR_RACA",
         pres_cie_nat = "IN_PRESENCA_CN",
         pres_cie_hum = "IN_PRESENCA_CH",
         pres_lin_cod = "IN_PRESENCA_LC",
         pres_mat     = "IN_PRESENCA_MT",
         stat_red     = "IN_STATUS_REDACAO",
         nota_cie_nat = "NOTA_CN",
         nota_cie_hum = "NOTA_CH",
         nota_lin_cod = "NOTA_LC",
         nota_mat     = "NOTA_MT",
         nota_red     = "NU_NOTA_REDACAO",
         rend_fam     = "Q003")

enem2015 %<>%
  rename(ano          = "NU_ANO",
         cod_mun      = "CO_MUNICIPIO_RESIDENCIA",
         idade        = "NU_IDADE",
         sexo         = "TP_SEXO",
         tipo_escola  = "TP_ESCOLA" ,
         cor_raca     = "TP_COR_RACA",
         pres_cie_nat = "TP_PRESENCA_CN",
         pres_cie_hum = "TP_PRESENCA_CH",
         pres_lin_cod = "TP_PRESENCA_LC",
         pres_mat     = "TP_PRESENCA_MT",
         stat_red     = "TP_STATUS_REDACAO",
         nota_cie_nat = "NU_NOTA_CN",
         nota_cie_hum = "NU_NOTA_CH",
         nota_lin_cod = "NU_NOTA_LC",
         nota_mat     = "NU_NOTA_MT",
         nota_red     = "NU_NOTA_REDACAO",
         rend_fam     = "Q006")

enem2016 %<>%
  rename(ano          = "NU_ANO",
         cod_mun      = "CO_MUNICIPIO_RESIDENCIA",
         idade        = "NU_IDADE",
         sexo         = "TP_SEXO",
         tipo_escola  = "TP_ESCOLA" ,
         cor_raca     = "TP_COR_RACA",
         pres_cie_nat = "TP_PRESENCA_CN",
         pres_cie_hum = "TP_PRESENCA_CH",
         pres_lin_cod = "TP_PRESENCA_LC",
         pres_mat     = "TP_PRESENCA_MT",
         stat_red     = "TP_STATUS_REDACAO",
         nota_cie_nat = "NU_NOTA_CN",
         nota_cie_hum = "NU_NOTA_CH",
         nota_lin_cod = "NU_NOTA_LC",
         nota_mat     = "NU_NOTA_MT",
         nota_red     = "NU_NOTA_REDACAO",
         rend_fam     = "Q006")

enem.df <- enem2013 %>% 
  bind_rows(enem2014) %>% 
  bind_rows(enem2015) %>% 
  bind_rows(enem2016)

rm(enem2013, enem2014, enem2015, enem2016)

enem.df %<>%
  mutate(pres_cie_hum = as.factor(pres_cie_hum),
         pres_cie_nat = as.factor(pres_cie_nat),
         pres_lin_cod = as.factor(pres_lin_cod),
         pres_mati    = as.factor(pres_mat),
         pres_reda    = as.factor(stat_red))

presenca.df <- enem.df %>% 
  filter(pres_cie_nat == 1,
         pres_cie_hum == 1,
         pres_lin_cod == 1, 
         pres_mat     == 1)

write_rds(enem.df, "[Trabalho]ENEM/enem2013-2016.rds")

write_rds(presenca.df, "[Trabalho]ENEM/enem2013-2016_USO.rds")