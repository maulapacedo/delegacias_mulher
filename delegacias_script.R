###### DELEGACIAS DA MULHER DO BRASIL ######
###### 08/07/2022 

#Carregar pacotes
pacman::p_load(readxl, dplyr, lubridate, tidyverse, kableExtra, rgdal, 
               devtools, ggplot2, sf)


###### BASE DE DADOS Az-Minas #######
#Abrir Dados
delegacias <- read_excel("~/TCC/bases/levantamento_delegacias.xlsx")

#Estrutura dados  
summary(delegacias)
str(delegacias)

#Pre-processamento
delegacias$ano_criacao[which(delegacias$ano_criacao == "-")] <- 0
delegacias[delegacias == 0] <- NA

delegacias$ano_criacao <- as.Date(as.character(delegacias$ano_criacao), format = "%Y")
delegacias$ano_criacao <- format(delegacias$ano_criacao, format = "%Y")


#Filtrar para apenas delegacias da Mulher 
delegacias_mulher <- delegacias |> 
  select(-c("telefone1", "telefone2", "e-mail", "observacao", "telefone_correto",
            "horario_correto", "horario_inicio", "horario_fim", "natureza", "projeto",
            "complemento", "obs_pandemia", "url", "endereco_correto", "existe_delegacia",
            "delegacia_mulher")) |> 
  filter(categoria == "Delegacia da Mulher")

#Renomear variavel municipios
delegacias_mulher <- delegacias_mulher |> 
  rename(name_muni = municipio)


municipios_merge <- merge(delegacias, municipios, by = "name_muni")

#Analise dos Dados 
table(delegacias$uf)
#-------------------------------------------------------------------------------#
##### ANALISE PERCENTUAL #####
#Total
delegacias <- X_2020_10_14_Levantamento_Delegacias_da_Mulher_dadosbrutos_dados_brutos

delegacias_1 <- delegacias %>% 
  group_by(uf) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n/sum(n)* 100) %>% 
  rename(abbrev_state = uf)

#

delegacias_2 <- delegacias_2 %>% 
  select(abbrev_state, n, percent, code_muni)

##### MAPA DELEGACIAS ######

#Carregar mapa 
devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr)

#Carregar shapefiles de todos os municipios do Brasil
municipios <- read_municipality(code_muni = "all", year = 2020)
class(municipios)
municipios

ggplot(municipios) +
  geom_sf(aes(fill = name_state)) +
  labs(fill = "Estado")






