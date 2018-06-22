rm(list = ls())

library(tidyverse)
library(lubridate)

serie_df <- read_rds("[Trabalho]SSP/serie.rds")

serie_df <- serie_df %>% 
  mutate(ocorrencia = case_when(ocorrencia == "TOT. DE BOL. DE OCORRÊNCIA"                                       ~ "Total de boletins de ocorrência",
                                ocorrencia == "PESSOAS MORTAS EM CONF.POL.CIV. EM SERVIÇO."                      ~ "Pessoas mortas em confronto com a polícia civil em serviço",
                                ocorrencia == "Pessoas mortas em confronto com a polícia civil em\r\n  serviço"  ~ "Pessoas mortas em confronto com a polícia civil em serviço",
                                ocorrencia == "PESSOAS MORTAS POR POL. CIV. DE FOLGA"                            ~ "Pessoas mortas por policiais civis de folga",
                                ocorrencia == "POL. CIV. MORTOS EM SERVIÇO"                                      ~ "Policiais civis mortos em serviço",
                                ocorrencia == "PESSOAS MORTAS EM CONF.POL.MIL. EM SERVIÇO"                       ~ "Pessoas mortas em confronto com a polícia militar em serviço",
                                ocorrencia == "Pessoas mortas em confronto com a polícia militar em\r\n  serviço"~ "Pessoas mortas em confronto com a polícia militar em serviço",
                                ocorrencia == "PESSOAS MORTAS POR POL. MIL. DE FOLGA"                            ~ "Pessoas mortas por policiais militares de folga",
                                ocorrencia == "POL. MIL. MORTOS EM SERVIÇO"                                      ~ "Policiais militares mortos em serviço",
                                T ~ ocorrencia),
         ocorrencia = str_replace(ocorrencia, "\\(\\d{1,}\\)", "") %>% str_trim(),
         mes_tri = case_when(tri == 1 ~ 1, 
                             tri == 2 ~ 4,
                             tri == 3 ~ 8,
                             tri == 4 ~ 12),
         data = str_c(ano,"/",mes_tri),
         data = parse_date(data, format = "%Y/%m"),
         data_tri = case_when(tri == 1 ~ "Jan-Mar",
                              tri == 2 ~ "Abr-Jun",
                              tri == 3 ~ "Ago-Set",
                              tri == 4 ~ "Out-Dez"),
         data_tri = str_c(data_tri,"/",ano))

str_subset(serie_df$ocorrencia[serie_df$ano == "2017"], pattern = "folga")

format_quarters <- function(x) {
  x <- zoo::as.yearqtr(x)
  year <- as.integer(x)
  quart <- as.integer(format(x, "%q"))
  
  paste(c("Jan-Mar","Apr-Jun","Jul-Sep","Oct-Dec")[quart], 
        year)
}

serie_df %>% 
  filter(local == "estado") %>% 
  filter(ocorrencia %in% c("Policiais militares mortos em serviço",
                           "Policiais militares mortos de folga",
                           "Pessoas mortas por policiais militares de folga",
                           "Pessoas mortas em confronto com a polícia militar em serviço")) %>% 
  ggplot(mapping = aes(x = data, y = quantidade, color = ocorrencia)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 40, hjust = 1))

