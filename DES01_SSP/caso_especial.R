caso_2001 <- function(tri){
  url_base <- "http://www.ssp.sp.gov.br/Estatistica/plantrim/2001_0TRI.htm"
  
  url_uso <- str_replace(url_base, "TRI", str_c(tri))
  
  resposta <- read_html(url_uso) %>% 
    html_nodes(xpath = "//table/tr") %>% 
    html_text()
  
  limpa <- resposta[5:55]
  
  limpa <- limpa[-(c(1,8,9,12,13,18,19,24,25,27,29,30,32:38))]
  
  teste <- str_replace_all(limpa, "\r\n", "")
  
  ocorrencias <- c("DELITOS CONTRA PESSOA",
                   "DELITOS CONTRA PATRIMÔNIO",
                   "DELITOS CONTRA COSTUMES",
                   "ENTORPECENTES",
                   "OUTROS DELITOS (INCLUI CONTRAVENÇÕES)",
                   "TOTAL DE DELITOS",
                   "TOT. DE BOL. DE OCORRÊNCIA",
                   "TOT. INQ. POL. INSTAURADOS",
                   "PESSOAS MORTAS EM CONF. POL. CIV.",
                   "PESSOAS FERIDAS EM CONF. POL. CIV.",
                   "POL. CIV. MORTOS EM SERVIÇO",
                   "POL. CIV. FERIDOS EM SERVIÇO",
                   "PESSOAS MORTAS EM CONF. POL. MIL.",
                   "PESSOAS FERIDAS EM CONF. POL. MIL.",
                   "POL. MIL. MORTOS EM SERVIÇO",
                   "POL. MIL. FERIDOS EM SERVIÇO",
                   "PRISÕES EFETUADAS",
                   "ARMAS DE FOGO APREENDIDAS",
                   "HOMICÍDIO DOLOSO",
                   "HOMICÍDIO CULPOSO",
                   "TENTATIVA DE HOMICÍDIO",
                   "LESÃO CORPORAL (CULP. E DOL.)",
                   "LATROCÍNIO",
                   "ESTUPRO",
                   "EXTORSÃO MEDIANTE SEQÜESTRO (4)",
                   "TRÁFICO DE ENTORPECENTES",
                   "ROUBO DE VEÍCULO",
                   "ROUBO - OUTROS",
                   "TOTAL DE ROUBO",
                   "FURTO DE VEÍCULO",
                   "FURTO - OUTROS",
                   "TOTAL DE FURTO")
  
  quantidades <- str_extract_all(teste, "[0-9]+.?[0-9]*")
  
  quantidades <- map(quantidades,
                     ~parse_number(x = .,locale = locale(grouping_mark = ".", decimal_mark = ",")))
  
  capital <- vector(mode = "numeric")
  `gde sp (1)` <- vector(mode = "numeric")
  interior <- vector(mode = "numeric")
  estado <- vector(mode = "numeric")
  for(i in 1:32){
    capital[i] <- quantidades[[i]][1]
  }
  
  for(i in 1:32){
    `gde sp (1)`[i] <- quantidades[[i]][2]
  }
  
  for(i in 1:32){
    interior[i] <- quantidades[[i]][3]
  }
  
  for(i in 1:32){
    estado[i] <- quantidades[[i]][4]
  }
  
  tabela <- tibble(ocorrencia  = ocorrencias,
                   capital      = as.character(capital),
                   `gde sp (1)` = as.character(`gde sp (1)`),
                   interior     = as.character(interior),
                   estado       = as.character(estado))
  
  tabela %>% 
    gather(capital:estado, key = "local", value = "quantidade")
  
}
