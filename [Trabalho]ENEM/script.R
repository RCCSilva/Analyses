#Análises dos dados do ENEM 2012 - 2017
# Rafael de Castro Coelho Silva

rm(list = ls())

library(tidyverse)
library(data.table)

enem2016 <- read_delim("Microdados_enem_2016/DADOS/microdados_enem_2016.csv",
                      escape_double = FALSE, delim = ";",
                      col_types = cols_only(NU_ANO                  = col_double(),
                                            TP_COR_RACA             = col_integer(),
                                            TP_SEXO                 = col_character(),
                                            CO_MUNICIPIO_RESIDENCIA = col_double(),
                                            TP_ESCOLA      = col_integer(),
                                            TP_PRESENCA_CN = col_integer(),
                                            TP_PRESENCA_CH = col_integer(),
                                            TP_PRESENCA_LC = col_integer(),
                                            TP_PRESENCA_MT = col_integer(),
                                            NU_NOTA_CN     = col_guess(),
                                            NU_NOTA_CH     = col_guess(),
                                            NU_NOTA_LC     = col_guess(),
                                            NU_NOTA_MT     = col_guess(),
                                            Q006           = col_character()))

enem2016 %>% 
  group_by(TP_COR_RACA) %>% 
  summarise(mean = mean(NU_NOTA_CH, na.rm = T))

write_rds(enem2016, "enem2016.rds")
