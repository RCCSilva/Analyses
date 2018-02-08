rm(list = ls())

library(tidyverse)
library(lubridate)

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
  mutate(pontos_atual    = cumsum(pontos_ganhos),
         saldo_gol_fav   = cumsum(gols_feitos),
         saldo_gol_con   = cumsum(gols_tomados),
         saldo_total     = saldo_gol_fav - saldo_gol_con)

segundo <- serie_df %>% #pontuação do segundo lugar
  group_by(ano, rodada) %>% 
  summarise(seg_lugar = sort(pontos_atual, decreasing = T)[2])

serie_df <- serie_df %>% 
  left_join(segundo, by = c("ano", "rodada")) %>% 
  mutate(pot_seg = seg_lugar + ((38 - rodada) * 3),
         ganhou = pontos_atual > pot_seg)

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
         pot_seg,
         seg_lugar)

# 3. Análise Exploratória -------------------------------------------------

for(i in 2012:2017){
  saldo_ganhou(i) +
    scale_y_continuous(limits = c(-2, 2))
  
  ggsave(str_c("[Trabalho]Brasileirao/",i,".png"))
}


system("convert -delay 120 [Trabalho]Brasileirao/*.png [Trabalho]Brasileirao/vit.gif")

# 4. Gráficos -------------------------------------------------------------


