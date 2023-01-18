#--------------------------#
# Project name: Delegacias da Mulher
# Purpose of the script: Baixar dados do SIM-Datasus
# Author: Antonio Vinicius
# Created on: Fri Apr  8 09:27:32 2022
#--------------------------#
# Insert notes:
# 1.
# 2.
#--------------------------#

# Opcoes
rm(list = ls())


# Pacotes
pacman::p_load(dplyr, tidyr, stringr, lubridate, microdatasus)


# Baixar microdados Datasus -------------------------------------------------------------------

# Vetor com CID-10 de crimes violentos (http://www.ipea.gov.br/atlasviolencia/quem/5/glossario)
mortes_violentas <- c(paste0("X", 85:99), paste0("Y0", 0:9), paste0("Y", 22:24))
morte_arma_fogo <- c(paste0("X", 93:95), paste0("Y", 22:24))
morte_agressao <- c(paste0("X", 85:92), paste0("X", 96:99), paste0("Y0", 0:9))

# Placebo (obitos por outras doencas)
# Algumas doenças infecciosas e parasitarias
morte_doenca_infecciosa <- c(paste0("A0", 0:9), paste0("A", 10:99), 
                             paste0("B0", 0:9), paste0("B", 10:99))

# Doencas do aparelho respiratorio, circulatorio e digestivo
morte_doenca_nao_infecciosa <- c(paste0("I0", 0:9), paste0("I", 10:99),
                                 paste0("J0", 0:9), paste0("J", 10:99),
                                 paste0("K0", 0:9), paste0("K", 10:99))

# Acidentes de transito
morte_acid_transito <- c(paste0("V0", 0:9), paste0("V", 10:99))



# Vetor com codigos das UFs
ufs <- c("AC", "AL", "AP", "AM","BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", 
         "PB", "PR","PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")


# Initial time
a <- Sys.time()

# Estado atual
for (i in seq_along(ufs)){

# Baixar dados (filtrar dados de 1996 a 2020)
  obitos_sample <- fetch_datasus(
    year_start = 1996, year_end = 2020, month_start = 1,
    month_end = 12, uf = ufs[i], information_system = "SIM-DO"
    ) 


# Calcular o total de obitos por mortes violentas por municipio 
  obitos_data <- obitos_sample |> 
    janitor::clean_names() |> 
    mutate(
      dtobito = dmy(dtobito),
      ano = year(dtobito),
      codmunres = str_sub(as.character(codmunres), 1, 6),
      codmunocor = str_sub(as.character(codmunocor), 1, 6),
      codmuncode = case_when(
        str_sub(as.character(codmunocor), 1,2) == "11" ~ "RO",
        str_sub(as.character(codmunocor), 1,2) == "12" ~ "AC",
        str_sub(as.character(codmunocor), 1,2) == "13" ~ "AM",
        str_sub(as.character(codmunocor), 1,2) == "14" ~ "RR",
        str_sub(as.character(codmunocor), 1,2) == "15" ~ "PA",
        str_sub(as.character(codmunocor), 1,2) == "16" ~ "AP",
        str_sub(as.character(codmunocor), 1,2) == "17" ~ "TO",
        str_sub(as.character(codmunocor), 1,2) == "21" ~ "MA",
        str_sub(as.character(codmunocor), 1,2) == "22" ~ "PI",
        str_sub(as.character(codmunocor), 1,2) == "23" ~ "CE",
        str_sub(as.character(codmunocor), 1,2) == "24" ~ "RN",
        str_sub(as.character(codmunocor), 1,2) == "25" ~ "PB",
        str_sub(as.character(codmunocor), 1,2) == "26" ~ "PE",
        str_sub(as.character(codmunocor), 1,2) == "27" ~ "AL",
        str_sub(as.character(codmunocor), 1,2) == "28" ~ "SE",
        str_sub(as.character(codmunocor), 1,2) == "29" ~ "BA",
        str_sub(as.character(codmunocor), 1,2) == "31" ~ "MG",
        str_sub(as.character(codmunocor), 1,2) == "32" ~ "ES",
        str_sub(as.character(codmunocor), 1,2) == "33" ~ "RJ",
        str_sub(as.character(codmunocor), 1,2) == "35" ~ "SP",
        str_sub(as.character(codmunocor), 1,2) == "41" ~ "PR",
        str_sub(as.character(codmunocor), 1,2) == "42" ~ "SC",
        str_sub(as.character(codmunocor), 1,2) == "43" ~ "RS",
        str_sub(as.character(codmunocor), 1,2) == "50" ~ "MS",
        str_sub(as.character(codmunocor), 1,2) == "51" ~ "MT",
        str_sub(as.character(codmunocor), 1,2) == "52" ~ "GO",
        str_sub(as.character(codmunocor), 1,2) == "53" ~ "DF"),
      homem = ifelse(sexo == 1, 1, 0),
      mulher = ifelse(sexo == 2, 1, 0)) |> 
    filter(
      codmuncode == ufs[i], 
      str_sub(causabas, 1, 3) %in% c(mortes_violentas, morte_doenca_infecciosa,
                                     morte_doenca_nao_infecciosa, morte_acid_transito)
      ) |> 
    group_by(ano, codmunocor, codmuncode) |> 
    summarise(
      # Total de obitos (mortes violentas)
      n_obitos = sum(str_sub(causabas, 1, 3) %in% mortes_violentas),
      n_obitos_arma_fogo = sum(str_sub(causabas, 1, 3) %in% morte_arma_fogo),
      n_obitos_agressao = sum(str_sub(causabas, 1, 3) %in% morte_agressao),
    
      # Mortes de homens
      n_obitos_homem = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & homem == 1),
      n_obitos_arma_fogo_homem = sum(str_sub(causabas, 1, 3) %in% morte_arma_fogo & homem == 1),
      n_obitos_agressao_homem = sum(str_sub(causabas, 1, 3) %in% morte_agressao & homem == 1),
    
      # Mortes de mulheres
      n_obitos_mulher = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & mulher == 1),
      n_obitos_arma_fogo_mulher = sum(str_sub(causabas, 1, 3) %in% morte_arma_fogo & mulher == 1),
      n_obitos_agressao_mulher = sum(str_sub(causabas, 1, 3) %in% morte_agressao & mulher == 1),
      
      # Raca (mulheres)
      n_obitos_mulher_preta = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & racacor %in% c("2", "4", "5") & mulher == 1),
      n_obitos_mulher_branca = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & racacor %in% c("1", "3") & mulher == 1),
      
      # Estado civil (mulheres)
      n_obitos_mulher_solteira = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & estciv == "1" & mulher == 1),
      n_obitos_mulher_casada = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & estciv == "2" & mulher == 1),
      n_obitos_mulher_viuva = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & estciv == "3" & mulher == 1),
      n_obitos_mulher_separada = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & estciv == "4" & mulher == 1),
      
      # Escolaridade (mulheres)
      n_obitos_mulher_esc_baixa = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & esc %in% c("1", "2", "3") & mulher == 1),
      n_obitos_mulher_esc_media = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & esc %in% c("4", "8") & mulher == 1),
      n_obitos_mulher_esc_alta = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & esc %in% c("5") & mulher == 1),
      
      # Ocupacao
      n_obitos_mulher_cbo_alta = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & str_sub(ocup, 1, 1) %in% c("0", "1", "2", "3") & mulher == 1),
      n_obitos_mulher_cbo_media = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & str_sub(ocup, 1, 1) %in% c("4", "5", "6", "7", "8") & mulher == 1),
      n_obitos_mulher_cbo_baixa = sum(str_sub(causabas, 1, 3) %in% mortes_violentas & str_sub(ocup, 1, 1) %in% c("9") & mulher == 1),
      
      #Placebo
      n_obitos_doenca_infecciosa_mulher = sum(str_sub(causabas, 1, 3) %in% morte_doenca_infecciosa & mulher == 1),
      n_obitos_doenca_nao_infecciosa_mulher = sum(str_sub(causabas, 1, 3) %in% morte_doenca_nao_infecciosa & mulher == 1),
      n_obitos_acid_transito_mulher = sum(str_sub(causabas, 1, 3) %in% morte_acid_transito & mulher == 1)
      ) |> 
    as_tibble()


  # Salvar dados
  readr::write_rds(
    obitos_data,
    paste0("Dados/DATASUS/SIM/obitos_data", ufs[i], ".rds"),               
    compress = "gz")

  print(ufs[i])

}

# Final time
b <- Sys.time()


# Elapsed time
elapsed_time <- b - a

# Ler e agrupar dados

# Listar arquivos em um diretorio
files_list <- list.files("Dados/DATASUS/SIM", pattern = "^obitos_data")

# Criar objeto
dados_sim <- data.frame()

for (file in files_list){

  temp <- readr::read_rds(paste0("Dados/DATASUS/SIM/", file))
  dados_sim <- bind_rows(dados_sim, temp)
  print(paste0(file))

}

# out <- dados_sim |> 
#   filter(!is.na(ano)) |> 
#   complete(ano, nesting(codmunocor, codmuncode), 
#            fill = list(
#              n_obitos = 0,
#              n_obitos_arma_fogo = 0,
#              n_obitos_agressao = 0,
#              n_obitos_homem = 0,
#              n_obitos_arma_fogo_homem = 0,
#              n_obitos_agressao_homem = 0,  
#              n_obitos_mulher = 0,
#              n_obitos_arma_fogo_mulher = 0,
#              n_obitos_agressao_mulher = 0 
#              )
#            )

# Completar linhas faltantes
dados_sim <- dados_sim |> 
  complete(
    ano = 1996:2020, 
    nesting(codmunocor, codmuncode)
  ) |> 
  mutate(
    across(where(is.numeric), ~replace_na(.x, 0))
  )


# Salvar dados
readr::write_rds(
  dados_sim,
  paste0("Dados/DATASUS/SIM/dados_sim.rds"),               
  compress = "gz")


