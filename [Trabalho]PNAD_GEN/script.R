#SCRIPT 
# Rafael de Castro Coelho Silva

#Avaliando o gap de educação entre homens e mulheres nos municípios brasileiros
# O objetivo é visualizar se há diferenças na desigualdade de gênero entre municípios
# localizados no interiou brasileiro e os municípios mais próximos das capitais.

#Hipótese: Existe diferença. Quanto mais no interior, menor a proporção de mulheres com 
# nível de educação. 

####### VARIÁVEIS #########
# UF:    Unidade Federativa
#
# V0302: Sexo
#        - 2. Masculino
#        - 4. Feminino
#
# V6007: Curso mais elevado que frequentou anteriormente
#        - 1. Curso fundamental
#        - 2. Curso Médio
#        - 5.  
#        - 8. Superior
#        - 9. Metrado ou Doutorado

rm(list = ls())

library(tidyverse)
library(magrittr)

pnad2015.df <- read_delim("PNAD/PNAD 2015/pnad.pes_2015.csv", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

pnad2015.df

table(pnad2015.df$V6007, pnad2015.df$V0302)

