#--------------------------#
# Project name: Delegacias da Mulher
# Purpose of the script:  Baixar dados do SINAN
# Author: Antonio Vinicius
# Created on: Fri May 27 09:58:50 2022
#--------------------------#
# Insert notes:
# 1.
# 2.
#--------------------------#

# Options
rm(list = ls())

# Pacotes
pacman::p_load(dplyr, tidyr, stringr, lubridate,
               ggplot2, scales, data.table, stringr)



# Abrir dados SINAN ---------------------------------------------------------------------------

# Dados extraidos da pagina Base dos Dados
dados_sinan <- readr::read_csv("Dados/SINAN/microdados_violencia.csv")

# Ler dicionario
dicionario_sinan <- readr::read_csv("Dados/SINAN/dicionario.csv")


# Violência doméstica é todo tipo de violência que é praticada entre os membros que habitam
# um ambiente familiar em comum. Pode acontecer entre pessoas com laços de sangue (como pais
# e filhos), ou unidas de forma civil (como marido e esposa ou genro e sogra).
# A violência doméstica pode ser subdividida em violência física, psicológica, sexual, patrimonial
# e moral. Também é considerada violência doméstica o abuso sexual de uma criança e maus tratos em
# relação a idosos.

# A violência contra a mulher é todo ato que resulte em morte ou lesão física, sexual ou 
# psicológica de mulheres, tanto na esfera pública quanto na privada


#⊢ Base de violencia contra a mulher ----------

violencia_mulher <- dados_sinan |> 
  mutate(
    ano_ocorrencia = year(data_ocorrencia)
  ) |> 
  filter(
    ano_ocorrencia >= 2009, # ano de ocorrencia a partir de 2009
    !is.na(id_municipio_ocorrencia),  #com informacao do municipio de ocorrencia
    !is.na(data_ocorrencia), # com informacao da data de ocorrencia
    sexo_paciente == 0, # sexo feminino
    idade_paciente >= 13, # exclui violencia infantil
    # tipo de violencia
    (
      ocorreu_violencia_fisica == 1 | 
      ocorreu_violencia_psicologica == 1 | 
      ocorreu_tortura == 1 |
      ocorreu_violencia_sexual == 1 |
      ocorreu_trafico_ser_humano == 1 |
      ocorreu_violencia_financeira == 1 |
      #ocorreu_negligencia_abandono == 1 |
      #ocorreu_trabalho_infantil == 1 |
      ocorreu_intervencao_legal == 1 |
      ocorreu_outra_violencia == 1
      )
    ) 
 


# Salvar dados
readr::write_rds(violencia_mulher, 
                 "Dados/SINAN/violencia_mulher.rds",
                 compress = "gz")




# Construindo variaveis -----------------------------------------------------------------------

# Abrir dados
violencia_mulher <- readr::read_rds("Dados/SINAN/violencia_mulher.rds")


#⊢ Violencia contra mulher ----------

viol_mulher <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0
  ) |> 
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |> 
  summarise(
    n_violencia_mulher = n(),
    .groups = "drop"
  ) 


#⊢ Violencia contra mulher reincidente ----------

viol_mulher_reincidente <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0,
    outras_vezes_ocorrencia == 1,
  ) |> 
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |> 
  summarise(
    n_violencia_mulher_reincidente = n(),
    .groups = "drop"
  ) 

#⊢ Idade ----------

viol_idade <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0
  ) |>
  mutate(
    idade_paciente_faixa = case_when(
      idade_paciente %in% c(13:21) ~ "idade_13_21",
      idade_paciente %in% c(22:45) ~ "idade_22_45",
      idade_paciente %in% c(46:65) ~ "idade_46_65",
      idade_paciente > 65 ~ "idade_65_mais"
    )
  ) |> 
  group_by(id_municipio_ocorrencia, ano_ocorrencia, idade_paciente_faixa) |> 
  summarise(
    n_violencia = n(),
    .groups = "drop"
  ) |> 
  pivot_wider(
    names_from = "idade_paciente_faixa",
    values_from = "n_violencia",
    names_prefix = "n_violencia_"
    )


#⊢ Estado civil ----------

viol_estado_civil <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0
  ) |>
  mutate(
    estado_civil = case_when(
      estado_civil_paciente == 1 ~ "solteira",
      estado_civil_paciente == 2 ~ "casada",
      estado_civil_paciente == 3 ~ "viuva",
      estado_civil_paciente == 4 ~ "separada",
      TRUE ~ "estado_civil_desconhecido"
    )
  ) |> 
  group_by(id_municipio_ocorrencia, ano_ocorrencia, estado_civil) |> 
  summarise(
    n_violencia = n(),
    .groups = "drop"
  ) |> 
  pivot_wider(
    names_from = "estado_civil",
    values_from = "n_violencia",
    names_prefix = "n_violencia_"
  )


#⊢ Raca ----------

viol_raca <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0
  ) |>
  mutate(
    raca = case_when(
      raca_paciente %in% c(1, 3) ~ "branca",
      raca_paciente %in% c(2, 4, 5) ~ "preta_parda",
      TRUE ~ "raca_nd"
    )
  ) |> 
  group_by(id_municipio_ocorrencia, ano_ocorrencia, raca) |> 
  summarise(
    n_violencia_raca = n(),
    .groups = "drop"
  ) |> 
  pivot_wider(
    names_from = "raca",
    values_from = "n_violencia_raca",
    names_prefix = "n_violencia_"
  )


#⊢ Educacao ----------

viol_educacao <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0
  ) |>
  mutate(
    escolaridade = case_when(
      escolaridade_paciente %in% c(1:4) ~ "educ_baixa",
      escolaridade_paciente %in% c(5:6) ~ "educ_media",
      escolaridade_paciente %in% c(7:8) ~ "educ_alta",
      TRUE ~ "educ_nd"
    )
  ) |> 
  group_by(id_municipio_ocorrencia, ano_ocorrencia, escolaridade) |> 
  summarise(
    n_violencia_educacao = n(),
    .groups = "drop"
  ) |> 
  pivot_wider(
    names_from = "escolaridade",
    values_from = "n_violencia_educacao",
    names_prefix = "n_violencia_"
  )


#⊢ Ocupacao ----------

viol_ocupacao <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0
  ) |>
  mutate(
    ocupacao = case_when(
      ocupacao_paciente %in% c(999991) ~ "estudante",
      ocupacao_paciente %in% c(999992, 999994) ~ "desempregada",
      str_sub(ocupacao_paciente, 1, 1) %in% c("0", "1", "2", "3") ~ "cbo_alta",
      str_sub(ocupacao_paciente, 1, 1) %in% c("4", "5", "6", "7", "8") ~ "cbo_media",
      TRUE ~ "outros_cbos"
    )
  ) |> 
  group_by(id_municipio_ocorrencia, ano_ocorrencia, ocupacao) |> 
  summarise(
    n_violencia = n(),
    .groups = "drop"
  ) |> 
  pivot_wider(
    names_from = "ocupacao",
    values_from = "n_violencia",
    names_prefix = "n_violencia_"
  )


#⊢ Tipo de violencia ----------

#* Violencia fisica -----
viol_fisica <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0,
    ocorreu_violencia_fisica == 1
  ) |>
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
  summarise(
    n_violencia_fisica = n(),
    .groups = "drop"
  )


#* Violencia psicologica -----
viol_psicologica <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0,
    ocorreu_violencia_psicologica == 1
  ) |>
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
  summarise(
    n_violencia_psicologica = n(),
    .groups = "drop"
  )


#* Violencia tortura -----
viol_tortura <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0,
    ocorreu_tortura == 1
  ) |>
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
  summarise(
    n_violencia_tortura = n(),
    .groups = "drop"
  )


#* Violencia sexual -----
viol_sexual <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0,
    ocorreu_violencia_sexual == 1
  ) |>
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
  summarise(
    n_violencia_sexual = n(),
    .groups = "drop"
  )


#* Trafico de ser humano -----
viol_trafico_humano <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0,
    ocorreu_trafico_ser_humano == 1
  ) |>
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
  summarise(
    n_violencia_trafico_humano = n(),
    .groups = "drop"
  )


#* Violencia financeira -----
viol_financeira <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0,
    ocorreu_violencia_financeira == 1
  ) |>
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
  summarise(
    n_violencia_financeira = n(),
    .groups = "drop"
  )


# Negligencia ou abandono
# viol_abandono <- violencia_mulher |> 
#   filter(
#     lesao_autoprovocada == 0,
#     ocorreu_negligencia_abandono == 1
#   ) |>
#   group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
#   summarise(
#     n_violencia_abandono = n(),
#     .groups = "drop"
#   )
  

# Trabalho infantil
# viol_trab_infantil <- violencia_mulher |> 
#   filter(
#     lesao_autoprovocada == 0,
#     ocorreu_trabalho_infantil == 1
#   ) |>
#   group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
#   summarise(
#     n_violencia_trab_infantil = n(),
#     .groups = "drop"
#   )


#* Intervencao legal -----
#viol_intervencao_legal <- violencia_mulher |> 
#  filter(
#    lesao_autoprovocada == 0,
#    ocorreu_intervencao_legal == 1
#  ) |>
#  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
#  summarise(
#    n_violencia_intervencao_legal = n(),
#    .groups = "drop"
#  )


#* Outras violencias -----
viol_outra <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0,
    ocorreu_outra_violencia == 1
  ) |>
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
  summarise(
    n_violencia_outra = n(),
    .groups = "drop"
  )


#* Violencia auto-provocada -----
viol_auto_provocada <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 1
  ) |>
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
  summarise(
    n_violencia_auto_provocada = n(),
    .groups = "drop"
  )



#* Intimate partner violence (IPV) -----
viol_parceiro_intimo <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0,
    # autor da violencia
     (
       autor_conjugue == 1 |
       autor_ex_conjugue == 1 |
       autor_namorado_a == 1 |
       autor_ex_namorado_a == 1 
       )
  ) |>
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
  summarise(
    n_violencia_parceiro_intimo = n(),
    .groups = "drop"
  )

#* Violencia domestica -----
viol_domestica <- violencia_mulher |> 
  filter(
    lesao_autoprovocada == 0,
    # autor da violencia
    (
      autor_pai == 1 |
      autor_mae == 1 |
      autor_padrasto == 1 |
      autor_madrasta == 1 |
      autor_conjugue == 1 |
      autor_ex_conjugue == 1 |
      autor_namorado_a == 1 |
      autor_ex_namorado_a == 1 |
      autor_filho_a == 1 |
      autor_irmao == 1 |
      autor_cuidador == 1
      )
  ) |>
  group_by(id_municipio_ocorrencia, ano_ocorrencia) |>
  summarise(
    n_violencia_domestica = n(),
    .groups = "drop"
  )


# Merge dos dados -----------------------------------------------------------------------------

dados_sinan <- viol_mulher |> 
  full_join(viol_mulher_reincidente) |> 
  full_join(viol_idade) |> 
  full_join(viol_estado_civil) |> 
  full_join(viol_raca) |> 
  full_join(viol_ocupacao) |> 
  full_join(viol_educacao) |> 
  full_join(viol_fisica) |> 
  full_join(viol_psicologica) |>
  full_join(viol_tortura) |>
  full_join(viol_sexual) |>
  full_join(viol_trafico_humano) |>
  full_join(viol_financeira) |>
  #full_join(viol_abandono) |>
  #full_join(viol_trab_infantil) |>
  #full_join(viol_intervencao_legal) |>
  full_join(viol_outra) |>
  full_join(viol_auto_provocada) |>
  full_join(viol_parceiro_intimo) |>
  full_join(viol_domestica) |> 
  complete(
    ano_ocorrencia = 2009:2019, 
    nesting(id_municipio_ocorrencia)
    ) |> 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )


# Salvar dados
readr::write_rds(dados_sinan, 
                 "Dados/SINAN/dados_sinan.rds",
                 compress = "gz")

#------------------------------------------------------------------------------


