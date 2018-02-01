#Análise dos Servidores da Preifeitura
# url http://dados.prefeitura.sp.gov.br/pt_PT/dataset/servidores-ativos-da-prefeitura/resource/1b99d9f4-9a30-40f2-8ccf-8d2716f1d514
# Rafael de Castro Coelho Silva

rm(list = ls())

library(tidyverse)

serv12_2017 <- read_csv2("verificadoativos02-01-2018.csv",
                         locale = locale(encoding = "ISO-8859-1"))

glimpse(serv12_2017)

serv12_2017 %>% 
  ggplot(mapping = aes(x = RACA)) +
  geom_bar()+
  coord_flip()

serv12_2017 %>% 
  ggplot(mapping = aes(x = SEXO)) +
  geom_bar()+
  coord_flip()

serv12_2017 %>% 
  ggplot(mapping = aes(x = CARGO_BASICO)) +
  geom_bar() +
  coord_flip()

serv12_2017 %>%
  count(CARGO_BASICO) %>% 
  arrange(desc(n))


#Criar Grandes Grupos de Profissão
professor <- str_subset(serv12_2017$CARGO_BASICO, "PROFESSOR") %>% 
  unique()

escola <- str_subset(serv12_2017$CARGO_BASICO, "ESCOLA") %>% 
  unique()
