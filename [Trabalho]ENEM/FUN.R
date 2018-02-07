inteiro_prox <- function(x){
  x <- x %/% 1
  while(x %% 60 != 0 & x & x %% 40 != 0){
    x <- x + 1
  }
  return(x)
}

cria_gif <- function(data, brasil, var1, title, tamanho = 0.1, transp = 1){
  
  ranges <- range(data[,str_c(var1)])
  
  if(abs(ranges[1]) > abs(ranges[2])){
    limites <- c(-abs(ranges[1]), abs(ranges[1]))
  } else {
    limites <- c(-abs(ranges[2]), abs(ranges[2]))
  }
  
  inteiro <- inteiro_prox(limites[2])
  
  breaks <- seq(-inteiro, inteiro, by = ifelse(inteiro > 200, 60,
                                               ifelse(inteiro > 81, 40, 20)))
  
  breaks <- c(breaks, 0)
  
  for(i in 2013:2016){
    data_uso <- data %>% 
      filter(ano == i)
    
    ggplot() +
      geom_polygon(data= brasil,
                   mapping = aes(x = long, y = lat, group = group),
                   fill = "darkgrey", color = "white") +
      geom_point(data = data_uso,
                 mapping = aes_string(x = "long", y = "lat", color = var1),
                 alpha = transp,
                 size = tamanho) +
      coord_map() +
      scale_colour_gradient2(high = "#09622A",
                             low = "#9C0824",
                             mid = "#CACACA",
                             breaks = breaks,
                             limits = limites) +
      theme_map() +
      labs(title = str_c(title,": ", i,sep = ""),
           subtitle = "Diferenças de cada município em relação à média dos municípios para cada ano",
           color = "Diferença",
           caption = "Fonte: INEP")
    
    ggsave(str_c("[Trabalho]ENEM/", i, var1,".png", sep = ""),
           width = 6.0, height = 6.0)
  }
  
  system(str_c("convert -delay 120 [Trabalho]ENEM/*", var1,".png ", "[Trabalho]ENEM/", var1,".gif", sep = ""))
}
