rm(list = ls())

library(tidyverse)

# 1. Curso ----------------------------------------------------------------

curso <- pdftools::pdf_text('relacao_candidato_vaga_completa_2019.pdf')

## Banco de dados do Curso
curso_sen <- str_split(curso, "\\n") %>% 
  unlist() %>% 
  str_trim() %>% 
  str_subset('^\\d{3}.+') %>% 
  str_extract("^\\d{3}\\s+[a-zA-Zà-úÀ-Ú,\\-() ]+") %>% 
  str_trim()

curso_df <- tibble(curso_id    = str_sub(curso_sen,1,3) %>% as.integer(),
                   curso_nome = str_trim(str_remove(curso_sen, '\\d{3}')))
## Alterando o nome de alguns cursos

curso_df <- curso_df %>% 
  mutate(curso_nome = case_when(curso_id == 100 ~ 'Administração, Economia, etc. - Ribeirão Preto',
                                curso_id == 165 ~ 'Administração, Economia, etc. - FEA',
                                curso_id == 790 ~ 'Matemática, Física, etc.',
                                curso_id == 850 ~ 'Química com ênfase Forense',
                                curso_id == 870 ~ 'Química com ênfase Alimentos, etc.',
                                T         ~ curso_nome))

# 2. Convocados -----------------------------------------------------------

convocados <- pdftools::pdf_text('fuvest2020chamada1.pdf')

convocados <-str_split(convocados, "\\n") %>% 
  unlist() %>% 
  str_trim() 

convocados_ls <- convocados %>% 
  str_extract_all('[a-zA-Zà-úÀ-Ú ]+(\\.{3})?\\s+\\d{3}\\.\\d{3}\\s+\\d{3}−\\d{2}') %>% 
  unlist() %>% 
  str_trim() %>% 
  str_sort()

convocados_df <- tibble(texto = convocados_ls)

convocados_df <- convocados_df %>% 
  mutate(nome = str_extract(texto, "^[a-zA-Zà-úÀ-Ú ]+"),
         nome = str_replace(nome, "[A-Z]{10,}", "") %>% str_trim(),
         pri_nome = str_extract(nome, "^[a-zA-Zà-úÀ-Ú]*"),
         sob_nome = str_extract(nome, "[a-zA-Zà-úÀ-Ú]*$"),
         cpf = str_extract(texto, "\\d{3}\\.\\d{3}") %>% str_trim(),
         curso_completo = str_extract(texto, "\\d{3}−\\d{2}") %>% str_trim(),
         curso_id = str_extract(curso_completo, "\\d{3}") %>% str_trim() %>% as.integer()) %>% 
  select(-texto)

## Atribuindo genero de acordo com o primeio nome

convocados_df <- convocados_df %>% 
  mutate(genero = genderBR::get_gender(pri_nome),
         genero = case_when(genero == 'Male'   ~ 'Masculino',
                            genero == 'Female' ~ 'Feminino',
                            is.na(genero)      ~ "Sem classificação"))

convocados_df <- convocados_df %>% 
  left_join(curso_df, by = "curso_id")

# 3. Gráficos -------------------------------------------------------------

convocados_df %>% 
  ggplot(mapping = aes(x = genero, y = ..prop.., group = 1)) +
  geom_bar() +
  theme_minimal() +
  scale_y_continuous(labels = c('0', '20%', '40%', '60%')) +
  theme() +
  labs(title = "Proporção de Homens e Mulheres - 1ª Chamada FUVEST",
       x     = "Gênero",
       y     = "Proporção")

convocados_df %>% 
  ggplot(mapping = aes(x = genero)) +
  geom_bar() +
  theme_minimal() +
  theme() +
  labs(title = "Quantidade de Homens e Mulheres - 1ª Chamada FUVEST",
       x     = "Gênero",
       y     = "Quantidade")

cursos_ordered <- convocados_df %>% 
  group_by(curso_nome, curso_id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

prop_fem_df <- convocados_df %>% 
  group_by(curso_id, curso_nome, genero) %>% 
  summarise(n        = n()) %>%
  mutate(total    = sum(n),
         prop_fem = ifelse(genero == 'Feminino', n/total, NA)) %>% 
  ungroup() %>% 
  filter(genero == 'Feminino') %>% 
  select(curso_id, prop_fem)

convocados_df <- convocados_df %>% 
  left_join(prop_fem_df, by = 'curso_id')

convocados_df$genero <- ordered(convocados_df$genero, levels = c("Sem classificação", "Masculino", 'Feminino'))

row_cursos <- as.integer(seq(1,nrow(cursos_ordered), length.out = 10))

for(i in seq_along(row_cursos)[-length(row_cursos)]){
  i0 = row_cursos[i]
  i1 = row_cursos[i + 1]
  
  convocados_df %>% 
    filter(curso_id %in% c(cursos_ordered$curso_id[i0:i1])) %>% 
    group_by(curso_id, curso_nome, genero, prop_fem) %>% 
    summarise(n        = n()) %>% 
    ggplot(mapping = aes(x = reorder(curso_nome, prop_fem), y = n, fill = genero)) +
    geom_col(position = 'fill') +
    geom_hline(yintercept = 0.5) +
    scale_y_continuous(labels = c('0', '25%', '50%', '75%', '100%')) +
    scale_fill_manual(values = c('#777777', '#91cf20', '#ff8d59')) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 15)) +
    labs(title = "Proporção de gêneros por cursos- 1ª Chamada FUVEST",
         subtitle = paste0('Cursos de ', cursos_ordered$n[i1], ' a ', cursos_ordered$n[i0], ' ingressantes'),
         x     = "",
         y     = 'Proporção',
         fill  = 'Gênero',
         caption = 'Fonte: FUVEST 2019')
  
  ggsave(paste0('grafico',i,'.png'), width = 10, height = 5, units = 'in' )
}
