rm(list = ls())

library(tidyverse)


# 1. Banco de Ingressantes ------------------------------------------------

texto <- pdftools::pdf_text("[Trabalho]FUVEST/fuvest_2018_chamada_1.pdf")

texto_sen <- str_split(texto, "\\n") %>% 
  str_trim() %>% 
  str_extract_all("[a-zA-Zà-úÀ-Ú ]+(\\.\\.\\.)?\\s+\\d{7}\\s+\\d{3}−\\d{2}") %>% 
  unlist()

chamada_1 <- tibble(texto = texto_sen)

chamada_1 <- chamada_1 %>% 
  mutate(nome = str_extract(texto, "^[a-zA-Zà-úÀ-Ú ]+"),
         nome = str_replace(nome, "[A-Z]{10,}", "") %>% str_trim(),
         pri_nome = str_extract(nome, "^[a-zA-Zà-úÀ-Ú]*"),
         sob_nome = str_extract(nome, "[a-zA-Zà-úÀ-Ú]*$"),
         num = str_extract(texto, "\\d{7}") %>% str_trim(),
         curso_completo = str_extract(texto, "\\d{3}−\\d{2}") %>% str_trim(),
         curso = str_extract(curso_completo, "\\d{3}") %>% str_trim() %>% as.numeric())

curso <- pdftools::pdf_text("[Trabalho]FUVEST/relacao_candidato_vaga_2018.pdf")

#Banco de dados do Curso
curso_sen <- str_split(curso, "\\n") %>% 
  str_trim() %>% 
  str_extract_all("\\d{3}\\s+[a-zA-Zà-úÀ-Ú ]+\\s+(\\([a-zA-Zà-úÀ-Ú ]*(\\.\\.\\.)?\\))?(−)?[a-zA-Zà-úÀ-Ú ]+") %>% 
  unlist()

# 2. Banco dos Cursos -----------------------------------------------------

curso_df <- tibble(texto = curso_sen)

curso_df <- curso_df %>% 
  mutate(curso = str_extract(texto, "\\d{3}") %>% str_trim() %>% as.numeric(),
         nome_curso = str_extract(texto, "[a-zA-Zà-úÀ-Ú ]+\\s+(\\([a-zA-Zà-úÀ-Ú ]*(\\.\\.\\.)?\\))?(−)?[a-zA-Zà-úÀ-Ú ]+") %>% str_trim())

curso_df <- curso_df %>% 
  filter(!(nome_curso %in% c("Inscritos", ""))) %>% 
  select(nome_curso, curso)

#Merge dos bancos
chamada_1 <- chamada_1 %>% 
  left_join(curso_df, by = "curso")

# 3. Atribuindo Sexo aos Nomes --------------------------------------------

#nome <- tibble(pri_nome = chamada_1$pri_nome) %>% 
  distinct()

#sexo_USA <- gender::gender(nome$pri_nome)

#sexo_BR <- vector(mode = "character")

#for(i in seq_along(nome$pri_nome)){
#  sexo_BR[i] <- genderBR::get_gender(nome$pri_nome[i])
#}

#nome$gender_BR <- sexo_BR

#nome <- nome %>% 
#  left_join(sexo_USA, c("pri_nome" = "name")) %>% 
#  rename(gender_USA = gender)

#nome <- nome %>% 
#  select(pri_nome, gender_BR, gender_USA) %>% 
#  mutate(gender_BR    = str_to_lower(gender_BR),
#         gender        = case_when(!is.na(gender_BR) ~ gender_BR,
#                                   !is.na(gender_USA) ~ gender_USA))

#nome %>% 
#  count(gender)

#write_rds(nome, "[Trabalho]FUVEST/nome.rds")

nome <- read_rds("[Trabalho]FUVEST/nome.rds")

chamada_1 <- chamada_1 %>% 
  left_join(nome, by = "pri_nome")

table(chamada_1$gender)
table(chamada_1$gender_BR)
table(chamada_1$gender_USA)


# 4. Gráficos -------------------------------------------------------------
chamada_1 %>% 
  ggplot(mapping = aes(x = gender)) +
  geom_bar()

chamada_1 %>% 
  group_by(nome_curso, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, n) %>% 
  mutate(total = sum(female, male, `<NA>`, na.rm = T),
         prop_m= female/total) %>% 
  gather(female:`<NA>`, key = "gender", value = "n") %>% 
  ggplot(mapping = aes(x = reorder(nome_curso, prop_m), y = n, fill = gender)) +
  geom_bar(position = "fill", stat = "identity") +
  theme(legend.position = "bottom") +
  coord_flip()


