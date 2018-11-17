##########################################################################
#                                                                        #
#                 Script Salários e Gêneros na USP                       # 
#                 Autor: Rafael de Castro Coelho Silva                   #
#                                                                        #
##########################################################################

rm(list = ls()) #Limpando a área de trabalho do R

library(tidyverse) #Pacote para manipulação, leitura e visualização de dados
library(stringr)
library(gender)    #Pacote para atribuir sexo a partir dos nomes
library(genderBR)  #Pacote para atribuir sexo a partir 
library(readxl)    #

source("FUN.R")

# 1. Carregando Banco -----------------------------------------------------


rm(list = ls())

## 1.1. Motando o Banco de Dados e Validando a criação da variável "sexo"


# Carregando o banco de dados 'sal_usp.rds'
sal_usp <- read_rds("sal_usp.rds") %>% 
  rename("unid" = `Unid/Orgão`,
         "depto" = `Depto/Setor`) %>% 
  extract(Nome, c("nome", "sobrenome"), "([^ ]+) (.*)")

docentes.df <- sal_usp %>% 
  filter(Categoria == "Docente")

# Atribui um sexo para cada nome a partir do genderBR::get_gender()
# nomes_genderBR <- c()
# for(i in  1:length(nomes_usp$name)){nomes_genderBR[i] <- get_gender(nomes_usp$name[i])}

#Cria um data_frame com o nome e o sexo
# nomes_gender <- data_frame(nome = nomes_usp$name, sexo = nomes_genderBR)

# write_rds(nomes_gender, "nomes_genderBR.rds")

nomes_genderBR <- read_rds("nomes_genderBR.rds") %>% 
  rename("sexo_BR" = "sexo")

nomes_genderUS <- nomes_genderBR$nome %>% 
  gender() %>% 
  select(name, gender) %>% 
  rename("nome" = "name",
         "sexo_US" = "gender")

#Cria um data_frame com osnomes dos funcionários da USP
nomes_usp <- left_join(nomes_genderBR, nomes_genderUS, by = "nome")

nomes_usp$sexo_BR <-  str_to_lower(nomes_usp$sexo_BR, locale = "en")

#Nomes que não receberam nenhuma classificação
nomes_docentes_NA <- sal_usp %>% 
  filter(Categoria == "Docente") %>% 
  count(nome) %>% 
  left_join(nomes_usp) %>% 
  filter(is.na(sexo_BR) & is.na(sexo_US))

#Nomes que receberam classificações dintintas
nomes_docentes_dist <- sal_usp %>% 
  filter(Categoria == "Docente") %>% 
  count(nome) %>% 
  left_join(nomes_usp) %>% 
  filter(!is.na(sexo_BR) & !is.na(sexo_US)) %>% 
  filter(sexo_BR != sexo_US)

#Por motivos de praticidade eu só vou classificar na mão os nomes que ou
# foram classicados diferentes ou não foram classificados
doc_sem_class <- nomes_docentes_dist %>% 
  full_join(nomes_docentes_NA)

#Criando um data_frame com os docentes a serem pesquisados manualmente
# Link do Sheets: https://docs.google.com/spreadsheets/d/1CS0TBK31AABR_XenUT9ib4t-2XQWXHwAwMyI9GJi3iY/edit?usp=sharing
doc_pesquisar <- doc_sem_class %>% 
  left_join(docentes.df) %>% 
  select(nome, sobrenome, unid) %>% 
  
  distinct() %>% 
  mutate(pesquisa = paste(nome, sobrenome, unid)) %>% 
  select(pesquisa) %>% 
  arrange(pesquisa)

write_csv(doc_pesquisar, "doc_pesquisar.csv")

# Carregando o data_frame com os nomes classificados manualmente
nomes_classificados <- read_csv("doc_pesquisar_OK.csv") %>% 
  rename("nome" = "NOME") %>% 
  select(nome, RAFAEL)

# Selecionando apenas as variáveis 'nome', 'sexo_BR', 'sexo_US'
nomes_usp <- nomes_usp %>% 
  select(nome, sexo_BR, sexo_US)

# Criando um data_frame 'nomes' com todos os nomes_usp e as respectivas classificações
# O data_frame final tem os nomes com e sem classificação do nome

nomes <- left_join(nomes_usp, nomes_classificados, by = "nome") %>% 
  select(nome, sexo_BR, sexo_US, RAFAEL) %>% 
  semi_join(nomes_usp, by = "nome") %>% 
  distinct()

# Aqui estou tentando selecionar os casos específicos 
# já que tem nomes com mais de uma classificação

# Seleciono apenas as classificações feitas por mim
nomes_rafael <- nomes %>% 
  filter(!is.na(RAFAEL)) %>% 
  rename("sexo_final" = "RAFAEL") %>% 
  select(nome, sexo_final)

# Quando apenas temos o 'sexo_BR', ficamos com essa classificação
nomes_BR <- nomes %>% 
  filter(!is.na(sexo_BR) & is.na(sexo_US)) %>% 
  rename("sexo_final" = "sexo_BR")%>% 
  select(nome, sexo_final)

# Quando apenas temos o 'sexo_US', ficamos com essa classificação
nomes_US <- nomes %>% 
  filter(is.na(sexo_BR) & !is.na(sexo_US)) %>% 
  rename("sexo_final" = "sexo_US")%>% 
  select(nome, sexo_final)

# Quando o nome não foi classificado em nenhum método automatizado
# ficamos apenas com o NA
nomes_NA <- nomes %>% 
  filter(is.na(sexo_BR) & is.na(sexo_US)) %>% 
  rename("sexo_final" = "sexo_US") %>% 
  select(nome, sexo_final)

# Quando os nomes existem (!NA) e são iguais, 
# ficamos com o 'sexo_BR'
nomes_iguais <- nomes %>% 
  filter(sexo_BR == sexo_US) %>% 
  rename("sexo_final" = "sexo_BR")%>% 
  select(nome, sexo_final)

#Quando os nomes existem (!NA) mas são diferentes, 
# ficamos com o nome RAFAEL

nomes_difer <- nomes %>% 
  filter(!is.na(sexo_BR) & !is.na(sexo_US)) %>% 
  filter(sexo_BR != sexo_US) %>% 
  rename("sexo_final" = "RAFAEL") %>% 
  select(nome, sexo_final)

#Juntando todos os nomes em um único data_frame
# agora temos apenas uma classificação de sexo para cada nome
# de acordo com os critérios utilizados no passo anteior
# PS: Não tenho certeza do porquê da quantidade de observações ser menor
#     Mas acredito que isso não seja um problema, já que todos os nomes de docentes
#     foram classificados.
# Caso eu queira analisar os nomes dos outros funcionários, eu preciso olhar melhor isso
nomes_final <- rbind(nomes_rafael, nomes_BR, nomes_US, nomes_iguais, nomes_difer) %>% 
  distinct()

#Cria o data_frame final em que os nomes foram classificados
usp.df <- left_join(sal_usp, nomes_final, by = "nome") %>% 
  mutate(sexo_final = ifelse(sexo_final == "female", "Feminino", "Masculino"))

###################################
#      Limpando os dados          #
rm(list = ls(pattern = "nomes"))  #
rm(list = ls(pattern = "doc"))    #
rm(sal_usp)                       #  
###################################

c_quali <- c("#e41a1c",
             "#377eb8",
             "#4daf4a",
             "#984ea3",
             "#ff7f00",
             "#ffff33",
             "#a65628",
             "#f781bf",
             "#999999",
             "#cab2d6",
             "#ffff99")
c_diver <- c("#d8b365", "#5ab4ac")


############################################################################################
#### 2. Análise Descritiva dos Dados

############################################################################################
#### 2.1. Análise das médias salariais

usp.df %>% 
  filter(Categoria == "Docente") %>% 
  group_by(date) %>% 
  summarise(media = mean(Líquido, na.rm = T)) %>% 
  ggplot(mapping = aes(x = date, y = media)) +
  geom_point() +
  geom_line() +
  labs(title = "Média Salarial dos Docentes",
       y = "Média",
       x = "Tempo",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)")

ggsave("g1_media_sal.png", height = 5.00, width = 7.00)

usp.df %>% 
  filter(Líquido > 0) %>% 
  filter(Categoria == "Docente") %>% 
  group_by(date, sexo_final) %>% 
  summarise(media = mean(Líquido)) %>% 
  ggplot(mapping = aes(x = date, y = media, color = sexo_final)) +
  geom_point() +
  geom_line() +
  labs(title = "Média Salarial dos Docentes por Gênero",
       color = "Gênero",
       y = "Média",
       x = "Tempo",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)") +
  scale_color_manual(values = c_diver) +
  theme(legend.position = "bottom")

ggsave("g2_media_sal_gen.png", height = 5.00, width = 7.00)

######### Explorando
library(magrittr)
usp.df %<>%
  mutate(doc_int = ifelse(Jornada == "RDIDP" & Líquido > 0 & is.na(`Função de Estrutura`), "Dedicação Exclusiva", "Não Dedicação Exclusiva"))

usp.df %>% 
  filter(Categoria == "Docente") %>% 
  filter(!is.na(doc_int)) %>% 
  ggplot(mapping = aes(x = sexo_final, fill = doc_int)) +
  geom_bar(position = "fill")

usp.df %>% 
  filter(Líquido > 0) %>% 
  filter(Categoria == "Docente") %>%
  group_by(date, Jornada, sexo_final) %>% 
  summarise(media = mean(Líquido, na.rm = T)) %>% 
  ggplot(mapping =aes(x = date, y = media, color = sexo_final)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Jornada, ncol = 1) +
  theme(legend.position = "bottom")

ggsave("doc_func_sal.png", height = 18.0, width = 7.0)





#Salários médios por unidades da USP
usp.df %>% 
  filter(Líquido > 0) %>% 
  filter(Categoria == "Docente") %>% 
  filter(obs == 38) %>% 
  group_by(unid) %>% 
  summarise(cont = n(), 
            media = mean(Líquido, na.rm = T)) %>%
  filter(cont > 10) %>%
  mutate(media_t = mean(media)) %>% 
  ggplot(mapping = aes(x = reorder(unid, media), y = media)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Médias Salarias dos Docentes por Unidade (01/11/2017)",
       subtitle = "Unidades com mais de 10 docentes",
       x = "Unidade",
       y = "Média",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)") +
  geom_hline(yintercept = 10951.68)


ggsave("g3_media_unid.png", height = 10.00, width = 7.00)

#Diferença Proporcional entre as médias de salários masculinos e femininos
usp.df %>% 
  filter(Líquido > 0) %>% 
  filter(Categoria == "Docente") %>% 
  filter(obs == 38) %>% 
  group_by(unid, sexo_final) %>% 
  summarise(cont = n(), 
            media = mean(Líquido, na.rm = T)) %>%
  group_by(unid)
  filter(cont > 10) 

 
  select(-cont) %>% 
  spread(key = "sexo_final", value = "media") %>%
  mutate(diff = Feminino / Masculino) %>% 
  filter(!is.na(diff)) %>% 
  gather(Feminino, Masculino, key = "sexo_final", value = "media") %>% 
  ggplot(mapping = aes(x = reorder(unid, diff), y = media, fill = sexo_final)) +
  geom_bar(stat = "identity", position = "fill") + 
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  labs(title = "Diferença Percentual no Salário dos Docentes por Gênero (01/11/2017)",
       subtitle = "Unidades com mais de 10 docentes",
       x = "Unidade",
       y = "Variação",
       fill = "Gênero",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)") +
  scale_fill_manual(values = c_diver) +
  theme(legend.position = "bottom")

ggsave("g4_media_unid_gen.png", height = 10.0, width = 7.00)

############################################################################################
#### 2.2. Análise das proporções de homens e mulheres nas unidades da USP
#### PS: Lembrar que esse dado foi coletado por meio do nomes presentes nas folhas de departamento
#### Talvez o mais correto seja dizer que são nomes femininos e masculinos

usp.df %>% 
  filter(!is.na(Função)) %>% 
  filter(Categoria == "Docente") %>% 
  group_by(date, Função) %>% 
  summarise(quanti = n()) %>% 
  ggplot(mapping = aes(x = date, y = quanti, color = Função)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c_quali) +
  labs(title = "Função dos Docentes na USP",
       x = "Tempo",
       y = "Quantidade",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)") +
  theme(legend.position = "bottom")

ggsave("g5_doc_func.png", height = 5.00, width = 7.00)

usp.df %>% 
  filter(!is.na(Função)) %>% 
  filter(Categoria == "Docente") %>% 
  group_by(date, Função, sexo_final) %>% 
  summarise(quanti = n()) %>% 
  ggplot(mapping = aes(x = date, y = quanti, color = Função)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c_quali) +
  labs(title = "Função dos Docentes por Gênero",
       x = "Tempo",
       y = "Quantidade",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)") +
  facet_wrap(~sexo_final) +
  theme(legend.position = "bottom")

ggsave("g6_doc_func_gen.png", height = 5.00, width = 7.00)

doc10 <- usp.df %>%
  filter(obs == 38) %>% 
  filter(Categoria == "Docente") %>% 
  count(unid) %>% 
  arrange(n) %>% 
  filter(n > 10)

usp.df %>%
  filter(obs == 38) %>% 
  filter(Categoria == "Docente") %>% 
  count(unid) %>% 
  filter(n > 10) %>% 
  ggplot(mapping = aes(x = reorder(unid, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Quantidade de Docentes por Unidades (01/11/2017)",
       subtitle = "Unidades com mais de 10 docentes",
       x = "Unidade", 
       y = "Quantidade",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)")

ggsave("g7_doc_unid.png", height = 10.0, width = 7.00)

usp.df %>%
  filter(obs == 38) %>% 
  filter(!is.na(Função)) %>% 
  filter(Categoria == "Docente") %>% 
  filter(unid %in% doc10$unid) %>% 
  group_by(unid, Função) %>% 
  summarise(n = n()) %>% 
  spread(key = Função, value = n) %>% 
  mutate(total = sum(Assistente,
                     `Auxiliar de Ensino`,
                     `Professor Associado`,
                     `Professor Colaborador`,
                     `Professor Contratado`,
                     `Professor Doutor`,
                     `Professor Titular`, na.rm = T),
         prop_titu = `Professor Titular`/total,
         prop_dout = `Professor Doutor`/total,
         prop_fake = sum((1 * `Auxiliar de Ensino`), (2 * `Professor Associado`), (3 * `Professor Colaborador`), (4 * `Professor Contratado`), (5 * `Professor Doutor`), (13 * `Professor Titular`), na.rm = T) / total) %>% 
  gather(Assistente,
         `Auxiliar de Ensino`,
         `Professor Associado`,
         `Professor Colaborador`,
         `Professor Contratado`,
         `Professor Doutor`,
         `Professor Titular`, key = "Função", value = "n") %>% 
  ggplot(mapping = aes(x = reorder(unid, prop_titu), y = n, fill = Função)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  labs(title = "Função dos Docentes por Unidade (01/11/2017)",
       subtitle = "Unidades com mais de 10 docentes",
       x = "Unidade",
       y = "Proporção",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)") +
  scale_fill_manual(values = c_quali) +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0.5)

ggsave("g8_doc_func_unid.png", height = 10.0, width = 7.00)

usp.df$Jornada <- ordered(usp.df$Jornada, levels = c("RDIDP", "RTC", "12 horas", "RTP"))

usp.df %>%
  filter(obs == 38) %>% 
  filter(!is.na(Função)) %>% 
  filter(Categoria == "Docente") %>% 
  filter(unid %in% doc10$unid) %>% 
  group_by(unid, Jornada) %>% 
  summarise(n = n()) %>% 
  spread(key = Jornada, value = n) %>% 
  mutate(total = sum(`12 horas`,RDIDP,RTC,RTP, na.rm = T),
         prop_rdidp = RDIDP / total) %>% 
  gather(`12 horas`, RDIDP, RTC,RTP, key = "Jornada", value = "n") %>% 
  mutate(ordem = ifelse(Jornada == "RDIDP", 4,
                        ifelse(Jornada == "RTC", 3,
                               ifelse(Jornada == "RTP", 2,
                                      ifelse(Jornada == "12 horas", 1, NA)
                                      )
                               )
                        )
         ) %>% 
  ggplot(mapping = aes(x = reorder(unid, prop_rdidp), y = n, fill = Jornada, group = ordem)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  labs(title = "Jornada dos Docentes por Unidade (01/11/2017)",
       subtitle = "Unidades com mais de 10 docentes",
       x = "Unidade",
       y = "Porporção",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c_quali)

ggsave("g9_doc_jornada.png", height = 10.0, width = 7.0)
  



#Criando um banco de dados com as proporções de homens e mulheres para cada unidade
# Isso facilita a criação dos gráficos
usp_gen_prop <- usp.df %>% 
  filter(Categoria == "Docente") %>% 
  filter(date == "2017/10/01") %>% #Selecionando a última observação disponível
  group_by(unid, sexo_final) %>% 
  summarise(cont = n()) %>% 
  spread(key = sexo_final, value = cont) %>% #Transformando o banco para facilitar os cálculos
  mutate(total = sum(Feminino, Masculino, na.rm = T),
         prop_m = Feminino/total,
         prop_h = Masculino/total) %>% 
  gather(Feminino, Masculino, key = "sexo_final", value = "cont")

#Visualização da proporção de mulheres nas unidades da USP com mais de 10  docentes
usp_gen_prop %>% 
  filter(total > 10) %>% 
  ggplot(mapping = aes(x = reorder(unid, prop_h), y = cont, fill = sexo_final)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  labs(title = "Proporção de Homens e Mulheres por Unidade (01/11/2017)",
       y = "Proporção",
       x = "Unidade da USP", 
       fill = "Gênero",
       subtitle = "Unidades com mais de 10 docentes",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)") +
  scale_fill_manual(values = c_diver) +
  theme(legend.position = "bottom")

ggsave("g10_graf_geral.png", height = 10.0, width = 7.0)

usp_gen_prop %>% 
  filter(total > 10 & total <= 100) %>% 
  ggplot(mapping = aes(x = reorder(unid, prop_h), y = cont, fill = sexo_final)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  labs(title = "Proporção de Homens e Mulheres por Unidade",
       y = "Proporção",
       x = "Unidade da USP", 
       fill = "Sexo",
       subtitle = "Unidades com mais de 10 e menos de 100 docentes") +
  scale_fill_manual(values = c_diver)

usp_gen_prop %>% 
  filter(total > 100 & total <= 200) %>% 
  ggplot(mapping = aes(x = reorder(unid, prop_h), y = cont, fill = sexo_final)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  labs(title = "Proporção de Homens e Mulheres Docentes na USP",
       y = "Proporção",
       x = "Unidade da USP", 
       fill = "Sexo",
       subtitle = "Unidades com mais de 100 e menos de 200 docentes",
       caption = "Autor: Rafael Coelho") +
  scale_fill_manual(values = c_diver)

usp_gen_prop %>% 
  filter(total > 200) %>% 
  ggplot(mapping = aes(x = reorder(unid, prop_h), y = cont, fill = sexo_final)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  labs(title = "Proporção de Homens e Mulheres Docentes na USP",
       y = "Proporção",
       x = "Unidade da USP", 
       fill = "Sexo",
       subtitle = "Unidades com mais de 200 docentes",
       caption = "Autor: Rafael Coelho") +
  scale_fill_manual(values = c_diver)

doc_fflch <- usp.df %>% 
  filter(Categoria == "Docente") %>%
  filter(unid == "FFLCH") %>% 
  filter(date == "2017/10/01") %>% #Selecionando a última observação disponível
  group_by(depto, sexo_final) %>% 
  summarise(cont = n()) %>% 
  spread(key = sexo_final, value = cont) %>% #Transformando o banco para facilitar os cálculos
  mutate(total = sum(Feminino, Masculino, na.rm = T),
         prop_m = Feminino/total,
         prop_h = Masculino/total,
         curso  = ifelse(depto %in% c("Antropologia",
                                      "Ciência Política",
                                      "Sociologia"), "Ciências Sociais",
                         ifelse(depto == "História", "História",
                                ifelse(depto == "Geografia", "Geografia",
                                       ifelse(depto == "Filosofia", "Filosofia",
                                              ifelse(depto %in% c("Letras Clássicas e Vernáculas",
                                                                  "Letras Modenas",
                                                                  "Letras Orientais",
                                                                  "Lingüística",
                                                                  "Teoria Literária e Literatura Comparada"), "Letras",
                                                     ifelse(depto == "Diretoria Faculdade de Filosofia, Letras e Ciências Humanas", "Diretoria", NA)
                                              )
                                       )
                                )
                         ))) %>% 
  gather(Feminino, Masculino, key = "sexo_final", value = "cont")


doc_fflch %>% 
  ungroup() %>% 
  mutate(depto = ifelse(depto == "Diretoria Faculdade de Filosofia, Letras e Ciências Humanas", "Diretoria", depto)) %>% 
  ggplot(mapping = aes(x = reorder(depto,prop_h), y = cont, fill = sexo_final)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_hline(yintercept = 0.5) +
  coord_flip() +
  labs(title = "Proporção de Homens e Mulheres Docentes FFLCH",
       y = "Proporção",
       x = "Unidade da USP", 
       fill = "Gênero",
       subtitle = "(01/11/2017)",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)") +
  scale_fill_manual(values = c_diver) +
  theme(legend.position = "bottom")

ggsave("g11_doc_fflch_gen.png", height = 10.0, width = 7.0)

############################################################################################
#### 2.2. Investigando a variação na quantidade de docentes por unidade da USP



#Cria um data_frame com a variação por unidade ao longo do tempo da quantidade
# de docentes. Cria duas variáveiz:
# diff = diferença na quantidade de docentes em relação ao mês anteior
# diff_pri = diferença na quantidade de docentes em relação ao primeiro mês observado
diff_doc_unid <- usp.df %>% 
  filter(Categoria == "Docente") %>% 
  group_by(date, unid) %>% 
  summarise(cont = n()) %>% 
  group_by(unid) %>% 
  mutate(lag      = dplyr::lag(cont),
         pri      = first(cont),
         diff     = (cont - lag)/lag * 100,
         diff_pri = (cont - pri)/pri * 100)


#Visualização da variação de docentes em relação à primeira observação
# para variações maiores (no último mês) do que 10% para mais ou para menos
diff_doc_unid %>%
  group_by(unid) %>% 
  filter(cont > 20) %>% 
  filter(last(diff_pri) > 10 | - 10 > last(diff_pri)) %>% 
  ggplot(mapping = aes(x = date, y = diff_pri, color = unid)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  scale_fill_brewer(type = "qual") +
  labs(y = "Variação (%)",
       x = "", color = "Unidade",
       title = "Maiores Variações de Docentes na Unidade",
       subtitle = "Unidades com mais de 20 docentes",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c_quali)

ggsave("g12_doc_var.png", height = 5.00, width = 7.00)


#Criando um data_frame para a variação na quantidade de docentes
diff_doc_FFLCH <- usp.df %>%
  filter(Categoria == "Docente") %>% 
  filter(unid == "FFLCH") %>% 
  group_by(date,depto) %>% 
  summarise(cont = n()) %>%
  group_by(depto) %>% 
  mutate(lag      = dplyr::lag(cont),
         pri      = first(cont),
         diff     = (cont - lag)/lag * 100,
         diff_pri = (cont - pri)/pri * 100,
         curso    = ifelse(depto %in% c("Antropologia",
                                        "Ciência Política",
                                        "Sociologia"), "Ciências Sociais",
                           ifelse(depto == "História", "História",
                                  ifelse(depto == "Geografia", "Geografia",
                                         ifelse(depto == "Filosofia", "Filosofia",
                                                ifelse(depto %in% c("Letras Clássicas e Vernáculas",
                                                                    "Letras Modenas",
                                                                    "Letras Orientais",
                                                                    "Lingüística",
                                                                    "Teoria Literária e Literatura Comparada"), "Letras",
                                                       ifelse(depto == "Diretoria Faculdade de Filosofia, Letras e Ciências Humanas", "Diretoria", NA)
                                                       )
                                                )
                                         )
                                  )
                           )
         )

#Visualização da variação da quantidade de docentetes por departameto da FFLCH
diff_doc_FFLCH %>% 
  filter(curso %in% c("Ciências Sociais", "Letras", "Geografia", "História", "Filosofia")) %>% 
  ggplot(mapping = aes(x = date, y = diff_pri, color = depto)) +
  geom_point() +
  geom_line() +
  facet_wrap(~curso) +
  labs(y = "Variação (%)", x = "", title = "Variação de Docentes na FFLCH", color = "Departamento")

#Tempo Usp

usp.df %>% 
  filter(Categoria == "Docente") %>% 
  filter(obs == 38) %>% 
  group_by(unid) %>% 
  summarise(media_t = mean(`Tempo USP`),
            n = n()) %>% 
  filter(n > 10) %>% 
  ggplot(mapping = aes(x = reorder(unid, media_t), y = media_t)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Média de Tempo",
       x = "Unidade",
       title = "Média de Tempo na Usp (01/11/2017)",
       subtitle = "Unidades com mais de 10 unidades",
       caption = "Fonte: Portal da Transparência - Sistemas USP (Elaboração Própria)")

ggsave("g13_doc_tempo.png", height = 10.0, width = 7.0)

usp.df %>% 
  filter(Categoria == "Docente") %>% 
  filter(Líquido < 50000) %>% 
  ggplot(mapping = aes(x = Líquido, fill = sexo_final)) +
  geom_density() +
  facet_wrap(~sexo_final)

usp.df %>% 
  filter(`Salário Mensal` == 0) %>% 
  group_by(date) %>% 
  summarise(media = mean(Líquido)) %>% 
  ggplot(mapping = aes(x = date, y = media)) +
  geom_bar(stat = "identity")

usp.df %>% 
  filter(`Salário Mensal` == 0) %>%
  filter(Líquido > 50000) %>% 
  arrange(desc(Líquido)) %>% 
  ggplot(mapping =aes(x = `Tempo USP`, y = log10(Líquido))) +
  geom_point(alpha = 0.2)




usp.df %>% 
  filter(is.na(doc_int)) %>% 
  View()
