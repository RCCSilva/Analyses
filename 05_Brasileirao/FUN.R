resultado_ganhado <- function(ano_esc){
  if(ano_esc == 2012){
    time_g <- "Fluminense - RJ"
  } else {
    time_g <- serie_df %>% 
      filter(ano == ano_esc) %>% 
      filter(ganhou == T) %>% 
      .$time %>% 
      .[1] %>% 
      as.character()
  }
  
  serie_df %>% 
    filter(ano == ano_esc) %>% 
    filter(time == time_g) %>% 
    group_by(ganhado) %>% 
    summarise(prop_vit = sum(saldo_partida > 0),
              prop_emp = sum(saldo_partida == 0),
              prop_der = sum(saldo_partida < 0)) %>% 
    gather(prop_vit:prop_der, key = "tipo", value = "quanti") %>% 
    ggplot(mapping = aes(x = ganhado, y = quanti, fill = tipo)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_fill_discrete(labels = c("Derrotas", "Empates", "Vitórias")) +
    scale_x_discrete(labels = c("Antes da Vitória", "Depois da Vitória")) +
    labs(title = str_c(ano_esc,": ", time_g),
         x = "Posição",
         y = "Quantidade",
         fill = "Resultado",
         caption = "Fonte: CBF")
}

saldo_ganhado <- function(ano_esc){
  if(ano_esc == 2012){
    time_g <- "Fluminense - RJ"
  } else {
    time_g <- serie_df %>% 
      filter(ano == ano_esc) %>% 
      filter(ganhou == T) %>% 
      .$time %>% 
      .[1] %>% 
      as.character()
  }
  
  serie_df %>% 
    filter(ano == ano_esc) %>% 
    filter(time == time_g) %>% 
    group_by(ganhado) %>% 
    summarise(media = mean(saldo_partida)) %>% 
    ggplot(mapping = aes(x = ganhado, y = media)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_x_discrete(labels = c("Antes da Vitória", "Depois da Vitória")) +
    labs(title = str_c(ano_esc,": ", time_g),
         x = "Posição",
         y = "Média de Saldo de Gols",
         caption = "Fonte: CBF")
}
