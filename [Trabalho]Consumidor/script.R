rm(list = ls()) 

library(tidyverse)
library(magrittr)

url <- "http://dados.mj.gov.br/dataset/0182f1bf-e73d-42b1-ae8c-fa94d9ce9451/resource/fcf4ae0f-9d5b-4a78-a875-099c3baf1210/download/20161.csv"

download.file(url = url, destfile = "consumidor2016.csv")

data.df <- read_delim("consumidor2016.csv", 
                      ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                      trim_ws = TRUE)

data.df %<>% 
  rename("seg_merc" = `Segmento de Mercado`,
         "nome_emp" = `Nome Fantasia`)

data.df %>% 
  group_by(seg_merc) %>% 
  summarise(n = n()) %>% 
  filter(n > 1000) %>% 
  ggplot(mapping = aes(x = reorder(seg_merc, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Reclamações - Segmentos do Mercado",
       subtitle = "Segmentos com mais de 1000 reclamções",
       x = "Segmento do Mercado", 
       y = "Quantidade",
       caption = "Fonte: consumidor.gov.br")

data.df %>% 
  filter(seg_merc == "Operadoras de Telecomunicações (Telefonia, Internet, TV por assinatura)") %>% 
  group_by(nome_emp) %>% 
  summarise(n = n()) %>% 
  ggplot(mapping = aes(x = reorder(nome_emp,n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Reclamações - Telecomunicação (Tel., Inter., TV)",
       x = "Segmento do Mercado", 
       y = "Quantidade",
         caption = "Fonte: consumidor.gov.br")

data.df %>% 
  filter(seg_merc == "Bancos, Financeiras e Administradoras de Cartão") %>% 
  group_by(nome_emp) %>% 
  summarise(n = n()) %>% 
  ggplot(mapping = aes(x = reorder(nome_emp,n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Reclamações - Telecomunicação (Tel., Inter., TV)",
       x = "Segmento do Mercado", 
       y = "Quantidade",
       caption = "Fonte: consumidor.gov.br")

data.df %>% 
  count(seg_merc) %>% 
  arrange(desc(n))
