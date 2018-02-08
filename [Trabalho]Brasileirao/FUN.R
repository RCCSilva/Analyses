saldo_ganhou <- function(ano_esc){
  if(ano_esc == 2012){
    time_g <- "Fluminense - RJ"
  } else {
    time_g <- serie_df %>% 
      filter(ano == ano_esc) %>% 
      filter(ganhou == T) %>% 
      .[1,1] %>% 
      as.character()
  }
  
  saldos_media <- serie_df %>% 
    filter(ano == ano_esc) %>% 
    filter(time == time_g) %>% 
    group_by(ganhou) %>% 
    summarise(mean = mean(saldo_partida))
  
  print(saldos_media)
  
  saldos_media %>% 
    ggplot(mapping = aes(x = ganhou, y = mean)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_x_discrete(labels = c("Antes da Vitória", "Depois da Vitória")) +
    labs(title = str_c(ano_esc,": ", time_g),
         x = "",
         y = "Média de Saldo de Gols")
}
