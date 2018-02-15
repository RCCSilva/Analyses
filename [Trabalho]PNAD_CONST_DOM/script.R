rm(list = ls())

library(tidyverse)


# 1. Banco ----------------------------------------------------------------

banco <- read_delim("[Trabalho]PNAD_CONST_DOM/pnad.pes_2015.csv",
                    delim = "\t")


# 2. Criando variáveis ----------------------------------------------------

banco <- banco %>% 
  mutate(sexo = case_when(V0302 == 4 ~ "Feminino", 
                          V0302 == 2 ~ "Masculino") %>% as.factor(),
         pos_fam = case_when(V0402 == 1 ~ "Pessoa de Referência",
                             V0402 == 2 ~ "Cônjuge",
                             V0402 == 3 ~ "Filho",
                             V0402 == 4 ~ "Outro parente",
                             V0402 == 5 ~ "Agregado",
                             V0402 == 6 ~ "Pensionista",
                             V0402 == 7 ~ "Empregado doméstico",
                             V0402 == 8 ~ "Parente do empregado doméstico") %>% as.factor())

glimpse(banco)


# 3. Análise Exploratória -------------------------------------------------

banco %>% 
  filter(V0401 == 1) %>% 
  ggplot(mapping = aes(x = sexo, y = ..prop.., group = 1)) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "bottom")

banco %>% 
  group_by(domicilioid) %>% 
  summarise(soma = n()) %>% 
  ggplot(mapping = aes(x = soma, y = ..prop.., group = 1)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0,20, by = 1)) +
  theme_minimal()

banco %>% 
  group_by(domicilioid, pos_fam) %>% 
  summarise(soma = n()) %>% 
  spread(pos_fam, soma, fill = 0) %>% 
  filter(Filho >= 1)
