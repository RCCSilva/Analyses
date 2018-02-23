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

#arrumando os cursos errados na mão
curso_df <- curso_df %>% 
  mutate(nome_curso = case_when(curso == 165 ~ "Economia, Administração, Ciências Contábeis e Atuária",
                                curso == 460 ~ "Farmácia/Bioquímica",
                                curso == 465 ~ "Farmácia/Bioquímica − Ribeirão Preto",
                                curso == 790 ~ "Física/Física Computacional/Meteorologia/etc.",
                                T            ~ nome_curso))

#Cruando a variável de campi
curso_df <-  curso_df%>% 
  mutate(campus = str_extract(nome_curso, "−[ a-zA-Zà-úÀ-Ú]+"),
         campus = str_replace(campus,"−", "") %>% str_trim(),
         campus = if_else(is.na(campus) | campus %in% c("FAU", "ECA", "Bacharelado e Licenciatura"), "Cidade Universitária - Capital", campus),
         campus = if_else(curso == 865, "Ribeirão Preto", campus))

table(curso_df$campus)

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
  ggplot(mapping = aes(x = gender, y = ..prop.., group = 1)) +
  geom_bar() +
  theme_minimal() +
  scale_x_discrete(labels = c("Mulheres", "Homens", "Sem Classificação")) +
  labs(title = "Proporção de Homens e Mulheres",
       y = "Prpporção",
       x = "Sexo",
       caption = "Fonte: FUVEST")

ggsave("[Trabalho]FUVEST/g0_prop.png", width = 8.0, height = 5.0)

chamada_1 %>% 
  group_by(nome_curso, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, n) %>% 
  mutate(total = sum(female, male, `<NA>`, na.rm = T),
         prop_m= female/total)

chamada_1 %>% 
  group_by(nome_curso, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, n) %>% 
  mutate(total = sum(female, male, `<NA>`, na.rm = T),
         prop_m= female/total) %>% 
  gather(female:`<NA>`, key = "gender", value = "n") %>% 
  ggplot(mapping = aes(x = reorder(nome_curso, prop_m), y = n, fill = gender)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = NULL) +
  scale_fill_discrete(labels = c("Mulher", "Homem", "Sem Classificação")) +
  labs(title = "Proporção de Homens e Mulheres nos Cursos da USP",
       y = "Proporção",
       x = "",
       fill = "Sexo",
       caption = "Fonte: FUVEST")

ggsave("[Trabalho]FUVEST/g1_prop_geral.png", width = 8.0, height = 10.0)

chamada_1 %>% 
  group_by(nome_curso) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(quanti = cut(n, c(10,28,41,84,783))) %>% 
  ggplot(mapping = aes(x = quanti)) +
  geom_bar()

#   Min.   1st Qu.  Median    Mean 3rd Qu.    Max. 
#   10.00   28.00   41.00   79.25   84.00  783.00 
  
chamada_1 %>% 
  group_by(nome_curso, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, n) %>% 
  mutate(total = sum(female, male, `<NA>`, na.rm = T),
         prop_m= female/total) %>%
  filter(total >= 84) %>% 
  gather(female:`<NA>`, key = "gender", value = "n") %>% 
  ggplot(mapping = aes(x = reorder(nome_curso, prop_m), y = n, fill = gender)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels = c("Mulher", "Homem", "Sem Classificação")) +
  labs(title = "Proporção de Homens e Mulheres nos Cursos da USP",
       subtitle = "Cursos com pelo menos 84 ingressantes",
       y = "Proporção",
       x = "",
       fill = "Sexo",
       caption = "Fonte: FUVEST")

ggsave("[Trabalho]FUVEST/g2_prop_1.png", height = 10.0, width = 10.0)

chamada_1 %>% 
  group_by(nome_curso, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, n) %>% 
  mutate(total = sum(female, male, `<NA>`, na.rm = T),
         prop_m= female/total) %>%
  filter(total < 84 & total >= 41) %>% 
  gather(female:`<NA>`, key = "gender", value = "n") %>% 
  ggplot(mapping = aes(x = reorder(nome_curso, prop_m), y = n, fill = gender)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels = c("Mulher", "Homem", "Sem Classificação")) +
  labs(title = "Proporção de Homens e Mulheres nos Cursos da USP",
       subtitle = "Cursos com pelo menos 41 ingressantes e menos de 84",
       y = "Proporção",
       x = "",
       fill = "Sexo",
       caption = "Fonte: FUVEST")

ggsave("[Trabalho]FUVEST/g3_prop_2.png", height = 8.0, width = 10.0)

chamada_1 %>% 
  group_by(nome_curso, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, n) %>% 
  mutate(total = sum(female, male, `<NA>`, na.rm = T),
         prop_m= female/total) %>%
  filter(total < 41 & total >= 28) %>% 
  gather(female:`<NA>3`, key = "gender", value = "n") %>% 
  ggplot(mapping = aes(x = reorder(nome_curso, prop_m), y = n, fill = gender)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels = c("Mulher", "Homem", "Sem Classificação")) +
  labs(title = "Proporção de Homens e Mulheres nos Cursos da USP",
       subtitle = "Cursos com pelo menos 28 ingressantes e menos de 41",
       y = "Proporção",
       x = "",
       fill = "Sexo",
       caption = "Fonte: FUVEST")

ggsave("[Trabalho]FUVEST/g4_prop_3.png", height = 10.0, width = 10.0)

chamada_1 %>% 
  group_by(nome_curso, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, n) %>% 
  mutate(total = sum(female, male, `<NA>`, na.rm = T),
         prop_m= female/total) %>%
  filter(total < 28 & total >= 10) %>% 
  gather(female:`<NA>`, key = "gender", value = "n") %>% 
  ggplot(mapping = aes(x = reorder(nome_curso, prop_m), y = n, fill = gender)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels = c("Mulher", "Homem", "Sem Classificação")) +
  labs(title = "Proporção de Homens e Mulheres nos Cursos da USP",
       subtitle = "Cursos com pelo menos 10 ingressantes e menos de 28",
       y = "Proporção",
       x = "",
       fill = "Sexo",
       caption = "Fonte: FUVEST")

ggsave("[Trabalho]FUVEST/g5_prop_4.png", height = 10.0, width = 10.0)


chamada_1  %>% 
  group_by(campus, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, n) %>% 
  mutate(total = sum(female, male, `<NA>`, na.rm = T),
         prop_m= female/total) %>%  
  gather(female:`<NA>`, key = "gender", value = "n") %>% 
  ggplot(mapping = aes(x = reorder(campus, prop_m), y = n, fill = gender)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_hline(yintercept = 0.5) +
  theme(legend.position = "bottom") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels = c("Mulher", "Homem", "Sem Classificação")) +
  labs(title = "Proporção de Homens e Mulheres nos Cursos da USP",
       subtitle = "Cursos com pelo menos 10 ingressantes e menos de 28",
       y = "Proporção",
       x = "",
       fill = "Sexo",
       caption = "Fonte: FUVEST")


ggsave("[Trabalho]FUVEST/g6_prop_campus.png", height = 8.0, width = 8.0)
