# Função para ler todos os vancos de dados presentes na área de trabalho (wd)
# Cada uma dessas tabelas são uma a uma colados no final da tabela anteior
# gerando, assim, uma 

ler_dados <- function(dir){
  
  files_path <- list.files(path = dir,pattern = "*.txt") #Lê e salva todos os nomes de arquivo com final .txt
  
  files_path <- str_c(dir,"/",tempfiles)
  
  dates <- stringr::str_extract(files_path, "\\d{4}_\\d{2}") %>% 
    stringr::str_replace("_", "-") %>% 
    stringr::str_c("-01")
  
  sal_usp <- purrr::map(tempfiles, read_delim, delim = ";", locale = locale(decimal_mark = ",")) %>% 
    purrr::map_if(~ncol(.) == 14, ~.[,-14])
  
  for(i in seq_along(sal_usp)){
    sal_usp[[i]][["DATA"]] <- dates[[i]]
  }
  
  sal_usp <- bind_rows(sal_usp)
  
  sal_usp$DATA <- parse_date(sal_usp$DATA, format = "%Y-%m-%d")
  
  return(sal_usp)
}
