#--------------------------#
# Project name: Delegacias da Mulher
# Purpose of the script: Construir base de dados final
# Author: Antonio Vinicius
# Created on: Mon Aug  8 16:53:10 2022
#--------------------------#
# Insert notes:
# 1.
# 2.
#--------------------------#

# Options
rm(list = ls())

# Load packages
pacman::p_load(dplyr, tidyr, readr, stringr, 
               ggplot2, lubridate, sf, geobr)




# Dados das DMs -------------------------------------------------------------------------

dm_data <- readxl::read_excel("Dados/Delegacias/levantamento_delegacias.xlsx") |> 
  mutate(
    # Converter para numerico
    ano_criacao = as.numeric(str_replace(ano_criacao, "-", "")),
    numero = as.numeric(str_replace(ano_criacao, "s/n", "")),
    
    # Converter para maiusculas
    across(
      c(nome, categoria, tipo_logradouro, nome_logradouro, bairro, municipio),
      ~str_to_upper(.x)
    ),
    
    # Logradouro
    logradouro = paste(tipo_logradouro, nome_logradouro),
    endereco = paste(logradouro, numero, sep = ", "),
    
    # Reparar nome dos municipios
    municipio = case_when(
      municipio == "TAGUATINGA" ~ "BRASÍLIA",
      municipio == "VALPARAÍSO" ~ "VALPARAÍSO DE GOIÁS",
      municipio == "CURUPURU" ~ "CURURUPU",
      municipio == "ABAETE" ~ "ABAETÉ",
      municipio == "IBIRITE" ~ "IBIRITÉ",
      municipio == "MARABA" ~ "MARABÁ",
      municipio == "GUABIRA" ~ "GUARABIRA",
      municipio == "SANTO AMARO" ~ "RECIFE",
      municipio == "PARQUE PIAUÍ" ~ "TERESINA",
      municipio == "RESENTE" ~ "RESENDE",
      municipio == "GUARAJÁ-MIRIM" ~ "GUAJARÁ-MIRIM",
      municipio == "ESTÂNCIA VELHA" ~ "ESTÂNCIA",
      municipio == "GUAIBA" ~ "GUAÍBA",
      municipio == "SANTANA DO LIVRAMENTO" ~ "SANT'ANA DO LIVRAMENTO",
      municipio == "JARAGUÁ DO SU" ~ "JARAGUÁ DO SUL",
      municipio == "GUARAJÁ MIRIM" ~ "GUAJARÁ-MIRIM",
      municipio == "EMBÚ" ~ "EMBU DAS ARTES",
      municipio == "EMBU" ~ "EMBU DAS ARTES",
      municipio == "FERNANDOPOLIS" ~ "FERNANDÓPOLIS",
      municipio == "COLINAS" ~ "COLINAS DO TOCANTINS",
      TRUE ~ as.character(municipio)
    ),
    uf =  case_when(
      municipio == "ARIQUEMES" ~ "RO",
      TRUE ~ as.character(uf)
    ),
    
    # Informacoes faltantes do ano de criacao, imputamos 0
    ano_criacao = if_else(is.na(ano_criacao), 0, ano_criacao)
  ) |>  
  # Selecionar variaveis
  select(
    nome_delegacia = nome, sigla, categoria, endereco, bairro, municipio,
    uf, ano_criacao, horario_inicio, horario_fim, latitude, longitude
  ) 




# Inserir codigo dos municipios 
mun_br <- geobr::read_municipality("all", year = 2020)
st_geometry(mun_br) <- NULL


mun_br <- mun_br |> 
  mutate(
    across(c(name_muni, name_region), ~str_to_upper(.x)),
    uf = abbrev_state
  ) |> 
  select(
    -c(uf, name_state)
  )


# Inserir codigo dos municipios
dm_data <- dm_data |> 
  left_join(
    mun_br, by = c("municipio" = "name_muni", "uf" = "abbrev_state")
  ) |> 
  rename(
    code_mun = code_muni,
    code_uf = code_state, 
    code_regiao = code_region, 
    nome_regiao = name_region
  )


code_municipios <- dm_data |> 
  select(code_mun, lon = longitude, lat = latitude) |> 
  unique() |> 
  drop_na()

temp <- NULL

for(i in 307:nrow(code_municipios)){

  sc <- geobr::read_census_tract(code_tract = pull(code_municipios[i,1]), year = 2010)
  
  aux <- sf::st_intersection(
    st_as_sf(code_municipios[i, 2:3], coords=c("lon","lat"), crs = 4674), 
    sc
  )
  
    temp <- bind_rows(temp, aux)
  
}


output <- temp
output2 <- temp

setor_censo_dm <- bind_rows(output, output2)

# Salvar dados --------------------------------------------------------------------------------

readr::write_rds(
  setor_censo_dm,
  "Dados/setor_censo_dm.rds",
  compress = "gz"
)




# Abrir dados

setor_censo_dm <- readr::read_rds("Dados/setor_censo_dm.rds")



