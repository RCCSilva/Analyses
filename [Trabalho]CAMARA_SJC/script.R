rm(list = ls())

library(tidyverse)


# 1. Carregando o Banco ---------------------------------------------------

serie_mensal_df <- read_rds("[Trabalho]CAMARA_SJC/serie_mensal.rds")

serie_mensal_df <- serie_mensal_df %>%
  as.tibble()

parsed <- map(serie_mensal_df[,3:ncol(serie_mensal_df)], ~parse_number(., locale = locale(decimal_mark = ",", grouping_mark = ".")))

for(i in 3:ncol(serie_mensal_df)){
  serie_mensal_df[,i] <- parsed[[i - 2]]
}

serie_mensal_df <- serie_mensal_df %>% 
  mutate(data = str_c(mes,"/",ano),
         data = parse_date(data, format = "%m/%Y"))

# 2. Análise Exploratória Univariada --------------------------------------

serie_mensal_df %>% 
  ggplot(mapping = aes(x = `Salário Base`)) +
  geom_histogram() +
  facet_wrap(~ano)

serie_mensal_df %>% 
  ggplot(mapping = aes(x = `Salário Líquido`)) +
  geom_histogram()

serie_mensal_df %>% 
  group_by(data) %>% 
  summarise(n())

serie_mensal_df %>% 
  filter(Cargo == "VEREADOR") %>% 
  group_by(data) %>% 
  summarise(media = mean(`Salário Líquido`)) %>% 
  ggplot(mapping = aes(x = data,y = media)) +
  geom_line()

nomes <- str_split(serie_mensal_df$`Nome do Servidor`, " ")
prinome <- vector(mode = "character")
for(i in 1:nrow(serie_mensal_df)){
  
  prinome[i] <- nomes[[i]][1]
}

prinome <- tibble(nome = prinome) %>% distinct()

genero <- gender::gender(prinome$prinome)

lista <- lst()

for(i in seq_along(prinome$prinome)){
  lista[[i]] <- genderBR::get_gender(prinome$prinome[i])
}

bind_rows(lista)
