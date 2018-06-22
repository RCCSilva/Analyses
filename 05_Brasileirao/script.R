rm(list = ls())

library(tidyverse)
library(lubridate)
library(magrittr)

source("[Trabalho]Brasileirao/FUN.R")
# 1. Banco de Dados -------------------------------------------------------

serie_df <- read_rds("[Trabalho]Brasileirao/serie.rds")

# 2. Arrumando o Banco ----------------------------------------------------

# 2.1. Horário e Data do Jogo

serie_df <- serie_df %>% 
  mutate(data = lubridate::dmy_hm(data))

serie_df <- serie_df %>% 
  mutate(ano = as.numeric(year(data)))

# 2.2. Criando um banco tidy

serie_df <- serie_df %>% 
  gather(mandante, visitante, key = "posicao", value = "time")

# 2.3. Serapando o Resuldado

serie_df <- serie_df %>% 
  mutate(gol_mandante = as.numeric(str_split(resultado, "x", simplify = T)[,1]),
         gol_visitant = as.numeric(str_split(resultado, "x", simplify = T)[,2]),
         gols_feitos  = case_when(posicao == "mandante" ~ gol_mandante,
                                  posicao == "visitante"~ gol_visitant),
         gols_tomados = case_when(posicao == "mandante" ~ gol_visitant,
                                  posicao == "visitante"~ gol_mandante),
         saldo_partida = gols_feitos - gols_tomados)
  
# 2.4. Arrumando o nome do Atlético

serie_df <- serie_df %>%
  mutate(time = str_replace(time, "Atletico - PR", "Atlético - PR"))

#2.4. Atribuindo pontuação para os times

serie_df <- serie_df %>% 
  mutate(pontos_ganhos = case_when(saldo_partida >  0 ~ 3,
                                   saldo_partida == 0 ~ 1,
                                   saldo_partida <  0 ~ 0))

serie_df <- serie_df %>% 
  group_by(ano, time) %>% 
  arrange(rodada) %>% 
  mutate(pontos_atual    = cumsum(pontos_ganhos),
         saldo_gol_fav   = cumsum(gols_feitos),
         saldo_gol_con   = cumsum(gols_tomados),
         saldo_total     = saldo_gol_fav - saldo_gol_con)

segundo <- serie_df %>% #pontuação do segundo lugar
  group_by(ano, rodada) %>% 
  summarise(seg_lugar = sort(pontos_atual, decreasing = T)[2])

serie_df <- serie_df %>% 
  left_join(segundo, by = c("ano", "rodada")) %>% 
  ungroup() %>% 
  arrange(ano, rodada, jogo) %>% 
  group_by(ano, time) %>% 
  mutate(pot_seg = seg_lugar + ((38 - rodada) * 3),
         ganhou  = pontos_atual > pot_seg,
         ganhado = lag(ganhou),
         ganhado = if_else(is.na(ganhado), F, ganhado))

serie_df <- serie_df%>% 
  filter(!is.na(time) & !is.na(pontos_atual))

serie_df <- serie_df %>%
  select(time,
         posicao,
         rodada,
         jogo,
         saldo_partida,
         gols_feitos,
         gols_tomados,
         data,
         ano,
         pontos_atual,
         saldo_gol_fav,
         saldo_gol_con,
         saldo_total,
         ganhou,
         ganhado,
         pot_seg,
         seg_lugar)

# 3. Análise Exploratória -------------------------------------------------

serie_df %>% 
  group_by(posicao) %>% 
  summarise(prop_vit = mean(saldo_partida > 0),
            prop_emp = mean(saldo_partida == 0),
            prop_der = mean(saldo_partida < 0)) %>% 
  gather(prop_vit:prop_der, key = "tipo", value = "prop") %>% 
  ggplot(mapping = aes(x = posicao, y = prop, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(labels = c("Derrotas", "Empates", "Vitórias")) +
  scale_x_discrete(labels = c("Mandante", "Visitante")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Resultado dos Jogos em Função da Posição do Time",
       x = "Posição",
       y = "Proporção",
       fill = "Resultado",
       caption = "Fonte: CBF")

ggsave("[Trabalho]Brasileirao/g1_res_pos.png", width = 8.0, height = 5.0)

for(i in 2012:2017){
  resultado_ganhado(i)
  
  ggsave(str_c("[Trabalho]Brasileirao/res_ganhado",i,".png"), width = 8.0, height = 5.0)
}


serie_df %>% 
  filter((time == "Corinthians - SP" & ano == 2017) |
           (time == "Palmeiras - SP" & ano == 2016) |
           (time == "Corinthians - SP" & ano == 2015) |
           (time == "Cruzeiro - MG" & ano == 2014) |
           (time == "Cruzeiro - MG" & ano == 2013) |
           (time == "Fluminense - RJ" & ano == 2012)) %>% 
  group_by(time,ano, ganhado) %>% 
  summarise(saldo_medio = mean(saldo_partida)) %>% 
  mutate(ano_time = str_c(ano, time, sep = " ")) %>% 
  ggplot(mapping = aes(x = saldo_medio, y = ano_time, color = ganhado, group = ano_time)) +
  geom_line(color = "black",size = 3, alpha = 0.2) +
  geom_point(size = 3) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_discrete(labels = c("Não", "Sim")) +
  labs(title = "Diferenças de Médias de Saldos de Gols",
       x = "Média de Saldo de Gols",
       y = "Time Campeão",
       color = "Campeão",
       caption = "Fonte: CBF")

ggsave("[Trabalho]Brasileirao/diff_saldo.png", width = 8.0, height = 5.0)