# Função para ler todos os vancos de dados presentes na área de trabalho (wd)
# Cada uma dessas tabelas são uma a uma colados no final da tabela anteior
# gerando, assim, uma 

ler_dados <- function(){
  tempfiles <- list.files(pattern = "*.txt") #Lê e salva todos os nomes de arquivo com final .txt
  
  sal_usp <- read_csv2(tempfiles[1]) #Lê e salva o primeiro banco de dados em 'sal_usp'
  
  sal_usp <- sal_usp[,1:13] #Por algum motivo, alguns bancos ficam como uma 14ª coluna. Essa função seleciona apenas as 13 colunas com informações relevantes
  
  sal_usp$obs <- 1 #Atribui o período da observação
  
  sal_usp$date <- stringr::str_replace(tempfiles[1], pattern = ".txt", replacement = "-01")
  
  sal_usp$date <- stringr::str_replace(sal_usp$date,pattern = "_", replacement = "-")
  
  # Lê banco por banco; quando necessário, seleciona apenas as 13 colunas com dados 
  # Atribui o número 'i' da observação e adiciona o banco no final do banco anterior
  # No final, temos uma grande banco em que cada observação corresponde a um salário em um mês
  for(i in 2:length(tempfiles)){ 
    data <- read_csv2(tempfiles[i])
    if(ncol(data) == 14){
      data <- data[,1:13]
    } 
    
    data$obs <- i
    
    data$date <- stringr::str_replace(tempfiles[i], pattern = ".txt", replacement = "-01")
    
    data$date <- stringr::str_replace(data$date,pattern = "_", replacement = "-")
    
    sal_usp <- rbind(sal_usp, data)
  }
  
  sal_usp$date <- parse_date(sal_usp$date)
  
  return(sal_usp)
}
