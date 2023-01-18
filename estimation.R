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
pacman::p_load(dplyr, tidyr, readr, stringr, ggplot2, showtext,
               lubridate, sf, fixest, did, didimputation, stargazer)


# Loading Google fonts (https://fonts.google.com/)
font_family <- "Amiri"
font_add_google(font_family)
showtext_auto()




# 1. Abrir dados ---------------------------------------------------------------------------------



#* Dados do SIM --------------------------------------------------------------------------

dados_sim <- readr::read_rds("Dados/DATASUS/SIM/dados_sim.rds") |> 
  filter(ano <= 2019, ano != 0) |> 
  rename(
    code_mun = codmunocor,
    code_uf = codmuncode
    )
  

glimpse(dados_sim)


#* Dados do SINAN ------------------------------------------------------------------------

dados_sinan <- readr::read_rds("Dados/SINAN/dados_sinan.rds") |> 
  rename(
    ano = ano_ocorrencia,
    code_mun = id_municipio_ocorrencia
    ) |> 
  mutate(
    code_mun = str_sub(code_mun, 1, 6)
  )

glimpse(dados_sinan)



#* Dados das DMs -------------------------------------------------------------------------

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


# Obter numero, ano de criacao da primeira e da ultima delegacia  
ano_criacao_dm <- dm_data |> 
  # Ano da primeira delegacia criada
  select(code_mun, ano_criacao) |> 
  group_by(code_mun) |> 
  distinct() |> 
  slice_min(ano_criacao) |> 
  rename(ano_criacao_min = ano_criacao) |> 
  # Ano da ultima delegacia criada
  left_join(
    dm_data |> 
      select(code_mun, ano_criacao) |> 
      group_by(code_mun) |> 
      distinct() |> 
      slice_max(ano_criacao)|> 
      rename(ano_criacao_max = ano_criacao)
  ) |> 
  # Numero de delegacias
  left_join(
    dm_data |> 
      group_by(code_mun) |> 
      summarise(n_delegacias = n())
  ) |> 
  mutate(
    code_mun = as.character(str_sub(code_mun, 1, 6))
  )



#* Dados da Populacao --------------------------------------------------------------------------

dados_populacao <- readr::read_csv("Dados/IBGE/Populacao/populacao_municipio.csv") |> 
  rename(code_mun = id_municipio) |> 
  mutate(code_mun = as.character(str_sub(code_mun, 1, 6))) |> 
  select(-sigla_uf)

dados_populacao_sexo <- readr::read_csv("Dados/DATASUS/Populacao_SUS/populacao_municipio_sus.csv") |>
  rename(code_mun = id_municipio) |> 
  mutate(code_mun = str_sub(code_mun, 1, 6)) |> 
  unite(
    sexo_grupo_idade, sexo, grupo_idade, remove = TRUE
    ) |> 
  mutate(
    sexo_grupo_idade = str_replace(sexo_grupo_idade, "-", "_"),
    sexo_grupo_idade = str_replace(sexo_grupo_idade, " ", "_")
    ) |> 
  pivot_wider(
    names_from = "sexo_grupo_idade",
    values_from = "populacao"
    ) |> 
  mutate(
    populacao_fem = rowSums(across(starts_with("feminino_"))),
    populacao_masc = rowSums(across(starts_with("masculino_")))
    ) |> 
  select(
    -starts_with("feminino_"), 
    -starts_with("masculino_")
    )



#* Juntar bases de dados -----------------------------------------------------------------------

dados_comp <- dados_sim |> 
  left_join(dados_sinan) |> 
  left_join(dados_populacao) |> 
  left_join(dados_populacao_sexo) |> 
  left_join(ano_criacao_dm) |> 
  select(
    ano:code_uf, populacao:n_delegacias, everything()
  ) |> 
  mutate(
    across(starts_with("n_"), ~.x/populacao_fem * 100000,
                .names = "{sub('n_', 'rate_', col)}"))



#* Salvar dados --------------------------------------------------------------------------------

readr::write_rds(
  dados_comp,
  "Dados/dados_comp.rds",
  compress = "gz"
)





# 2. Estimacao -----------------------------------------------------------------------------------

# Abrir dados

# dados_comp <- readr::read_rds("Dados/dados_comp.rds") |> 
#   filter(
#     is.na(ano_criacao_min) | ano_criacao_min >= 2011
#   ) |> 
#   drop_na(populacao_fem) |> 
#   mutate(
#     treat = ifelse(ano >= ano_criacao_min & !is.na(ano_criacao_min) & ano >= 2011, 1, 0),
#     group_CSA = if_else(is.na(ano_criacao_min), 0, ano_criacao_min), # CSA wants never treated cohort variable to be 0
#     group = if_else(is.na(ano_criacao_min), 10000, ano_criacao_min), # never treated cohort variable 10000 for fixest
#     time_to_treatment = ifelse(group != 10000, ano - group, -1000), # set time to treatment to -1000 for fixest
#     int_uf_ano = paste0(code_uf, ano),
#     ln_rate_obitos_mulher = log(rate_obitos_mulher + 1),
#     code_mun = as.numeric(code_mun)
#     ) 

# Cidades com media populacional de ate 500,000 habitantes

dados_comp <- readr::read_rds("Dados/dados_comp.rds") |> 
  filter(
    is.na(ano_criacao_min) | ano_criacao_min >= 2011
  ) |> 
  drop_na(populacao_fem) |> 
  mutate(
    treat = ifelse(ano >= ano_criacao_min & !is.na(ano_criacao_min) & ano >= 2011, 1, 0),
    group_CSA = if_else(is.na(ano_criacao_min), 0, ano_criacao_min), # CSA wants never treated cohort variable to be 0
    group = if_else(is.na(ano_criacao_min), 10000, ano_criacao_min), # never treated cohort variable 10000 for fixest
    time_to_treatment = ifelse(group != 10000, ano - group, -1000), # set time to treatment to -1000 for fixest
    int_uf_ano = paste0(code_uf, ano),
    ln_rate_obitos_mulher = log(rate_obitos_mulher + 1),
    code_mun = as.numeric(code_mun)
  ) |> 
  group_by(code_mun) |> 
  mutate(populacao_media = mean(populacao, na.rm = T)) |> 
  ungroup() |> 
  filter(populacao_media <= 500000)




#* Estatística descritiva --------------------------------------------------------------------

# All Sample
all_sample <- dados_comp |>  
  filter(between(time_to_treatment, -7, 7)) |> 
 summarise(
    obitos_mulher_mean = mean(rate_obitos_mulher, na.rm = T),
    obitos_mulher_sd = sd(rate_obitos_mulher, na.rm = T),
    
    obitos_arma_fogo_mulher_mean = mean(rate_obitos_arma_fogo_mulher, na.rm = T),
    obitos_arma_fogo_mulher_sd = sd(rate_obitos_arma_fogo_mulher, na.rm = T),

    rate_obitos_agressao_mulher_mean = mean(rate_obitos_agressao_mulher, na.rm = T),
    rate_obitos_agressao_mulher_sd = sd(rate_obitos_agressao_mulher, na.rm = T),
    
    rate_obitos_mulher_branca_mean = mean(rate_obitos_mulher_branca, na.rm = T),
    rate_obitos_mulher_branca_sd = sd(rate_obitos_mulher_branca, na.rm = T),
    
    rate_obitos_mulher_preta_mean = mean(rate_obitos_mulher_preta, na.rm = T),
    rate_obitos_mulher_preta_sd = sd(rate_obitos_mulher_preta, na.rm = T),
    
    rate_obitos_esc_baixa_mean = mean(rate_obitos_mulher_esc_baixa, na.rm = T),
    rate_obitos_esc_baixa_sd = sd(rate_obitos_mulher_esc_baixa, na.rm = T),
    
    rate_obitos_esc_alta_mean = mean(rate_obitos_mulher_esc_alta, na.rm = T),
    rate_obitos_esc_alta_sd = sd(rate_obitos_mulher_esc_alta, na.rm = T),
    
    rate_obitos_doenca_infecciosa_mulher_mean = mean(rate_obitos_doenca_infecciosa_mulher, na.rm = T),
    rate_obitos_doenca_infecciosa_mulher_sd = sd(rate_obitos_doenca_infecciosa_mulher, na.rm = T),
    
    rate_obitos_doenca_nao_infecciosa_mulher_mean = mean(rate_obitos_doenca_nao_infecciosa_mulher, na.rm = T),
    rate_obitos_doenca_nao_infecciosa_mulher_sd = sd(rate_obitos_doenca_nao_infecciosa_mulher, na.rm = T),
    
    rate_obitos_acid_transito_mulher_mean = mean(rate_obitos_acid_transito_mulher, na.rm = T),
    rate_obitos_acid_transito_mulher_sd = sd(rate_obitos_acid_transito_mulher, na.rm = T),
    
    rate_obitos_total_mean = mean(rate_obitos, na.rm = T),
    rate_obitos_total_sd = sd(rate_obitos, na.rm = T),
    
    rate_obitos_arma_fogo_mean = mean(rate_obitos_arma_fogo, na.rm = T),
    rate_obitos_arma_fogo_sd = sd(rate_obitos_arma_fogo, na.rm = T),
    
    sample = "all"
  )


# Pre DEAM
pre_treat <- dados_comp |>  
  filter(between(time_to_treatment, -7, -1)) %>% 
  summarise(
    obitos_mulher_mean = mean(rate_obitos_mulher, na.rm = T),
    obitos_mulher_sd = sd(rate_obitos_mulher, na.rm = T),
    
    obitos_arma_fogo_mulher_mean = mean(rate_obitos_arma_fogo_mulher, na.rm = T),
    obitos_arma_fogo_mulher_sd = sd(rate_obitos_arma_fogo_mulher, na.rm = T),
    
    rate_obitos_agressao_mulher_mean = mean(rate_obitos_agressao_mulher, na.rm = T),
    rate_obitos_agressao_mulher_sd = sd(rate_obitos_agressao_mulher, na.rm = T),
    
    rate_obitos_mulher_branca_mean = mean(rate_obitos_mulher_branca, na.rm = T),
    rate_obitos_mulher_branca_sd = sd(rate_obitos_mulher_branca, na.rm = T),
    
    rate_obitos_mulher_preta_mean = mean(rate_obitos_mulher_preta, na.rm = T),
    rate_obitos_mulher_preta_sd = sd(rate_obitos_mulher_preta, na.rm = T),
    
    rate_obitos_esc_baixa_mean = mean(rate_obitos_mulher_esc_baixa, na.rm = T),
    rate_obitos_esc_baixa_sd = sd(rate_obitos_mulher_esc_baixa, na.rm = T),
    
    rate_obitos_esc_alta_mean = mean(rate_obitos_mulher_esc_alta, na.rm = T),
    rate_obitos_esc_alta_sd = sd(rate_obitos_mulher_esc_alta, na.rm = T),
    
    rate_obitos_doenca_infecciosa_mulher_mean = mean(rate_obitos_doenca_infecciosa_mulher, na.rm = T),
    rate_obitos_doenca_infecciosa_mulher_sd = sd(rate_obitos_doenca_infecciosa_mulher, na.rm = T),
    
    rate_obitos_doenca_nao_infecciosa_mulher_mean = mean(rate_obitos_doenca_nao_infecciosa_mulher, na.rm = T),
    rate_obitos_doenca_nao_infecciosa_mulher_sd = sd(rate_obitos_doenca_nao_infecciosa_mulher, na.rm = T),
    
    rate_obitos_acid_transito_mulher_mean = mean(rate_obitos_acid_transito_mulher, na.rm = T),
    rate_obitos_acid_transito_mulher_sd = sd(rate_obitos_acid_transito_mulher, na.rm = T),
    
    rate_obitos_total_mean = mean(rate_obitos, na.rm = T),
    rate_obitos_total_sd = sd(rate_obitos, na.rm = T),
    
    rate_obitos_arma_fogo_mean = mean(rate_obitos_arma_fogo, na.rm = T),
    rate_obitos_arma_fogo_sd = sd(rate_obitos_arma_fogo, na.rm = T),
    
    sample = "pre"
  )


# Post DEAM
post_treat <- dados_comp |>  
  filter(between(time_to_treatment, 0, 7)) %>% 
  summarise(
    obitos_mulher_mean = mean(rate_obitos_mulher, na.rm = T),
    obitos_mulher_sd = sd(rate_obitos_mulher, na.rm = T),
    
    obitos_arma_fogo_mulher_mean = mean(rate_obitos_arma_fogo_mulher, na.rm = T),
    obitos_arma_fogo_mulher_sd = sd(rate_obitos_arma_fogo_mulher, na.rm = T),
    
    rate_obitos_agressao_mulher_mean = mean(rate_obitos_agressao_mulher, na.rm = T),
    rate_obitos_agressao_mulher_sd = sd(rate_obitos_agressao_mulher, na.rm = T),
    
    rate_obitos_mulher_branca_mean = mean(rate_obitos_mulher_branca, na.rm = T),
    rate_obitos_mulher_branca_sd = sd(rate_obitos_mulher_branca, na.rm = T),
    
    rate_obitos_mulher_preta_mean = mean(rate_obitos_mulher_preta, na.rm = T),
    rate_obitos_mulher_preta_sd = sd(rate_obitos_mulher_preta, na.rm = T),
    
    rate_obitos_esc_baixa_mean = mean(rate_obitos_mulher_esc_baixa, na.rm = T),
    rate_obitos_esc_baixa_sd = sd(rate_obitos_mulher_esc_baixa, na.rm = T),
    
    rate_obitos_esc_alta_mean = mean(rate_obitos_mulher_esc_alta, na.rm = T),
    rate_obitos_esc_alta_sd = sd(rate_obitos_mulher_esc_alta, na.rm = T),
    
    rate_obitos_doenca_infecciosa_mulher_mean = mean(rate_obitos_doenca_infecciosa_mulher, na.rm = T),
    rate_obitos_doenca_infecciosa_mulher_sd = sd(rate_obitos_doenca_infecciosa_mulher, na.rm = T),
    
    rate_obitos_doenca_nao_infecciosa_mulher_mean = mean(rate_obitos_doenca_nao_infecciosa_mulher, na.rm = T),
    rate_obitos_doenca_nao_infecciosa_mulher_sd = sd(rate_obitos_doenca_nao_infecciosa_mulher, na.rm = T),
    
    rate_obitos_acid_transito_mulher_mean = mean(rate_obitos_acid_transito_mulher, na.rm = T),
    rate_obitos_acid_transito_mulher_sd = sd(rate_obitos_acid_transito_mulher, na.rm = T),
    
    rate_obitos_total_mean = mean(rate_obitos, na.rm = T),
    rate_obitos_total_sd = sd(rate_obitos, na.rm = T),
    
    rate_obitos_arma_fogo_mean = mean(rate_obitos_arma_fogo, na.rm = T),
    rate_obitos_arma_fogo_sd = sd(rate_obitos_arma_fogo, na.rm = T),
    
    sample = "post"
  )

statistics <- bind_rows(all_sample, pre_treat, post_treat) |> as.matrix()
stargazer::stargazer(statistics,
                     type = "latex", 
                     style = "all",
                     mean.sd = TRUE)




#* Single DD coefficient -----------------------------------------------------------------------


# Taxa de obito

mod_1 <- feols(log(rate_obitos_mulher + 1) ~ treat | code_mun + ano,
             cluster = ~code_mun, weights = ~populacao,
             data = dados_comp)

summary(mod_1)

mod_2 <- feols(log(rate_obitos_arma_fogo_mulher + 1) ~ treat | code_mun + ano,
               cluster = ~code_mun, weights = ~populacao,
               data = dados_comp)

summary(mod_2)

mod_3 <- feols(log(rate_obitos_agressao_mulher + 1) ~ treat | code_mun + ano,
               cluster = ~code_mun, weights = ~populacao,
               data = dados_comp)

summary(mod_3)

# Gerar tabela
etable(mod_1, mod_2, mod_3,
       keep = "treat", tex = TRUE)


# Raca, escolaridade e estado civil

mod_4 <- feols(log(rate_obitos_mulher_branca + 1) ~ treat | code_mun + ano,
               cluster = ~code_mun, weights = ~populacao,
               data = dados_comp)


mod_5 <- feols(log(rate_obitos_mulher_preta + 1) ~ treat | code_mun + ano,
               cluster = ~code_mun, weights = ~populacao,
               data = dados_comp)


mod_6 <- feols(log(rate_obitos_mulher_esc_alta + 1) ~ treat | code_mun + ano,
               cluster = ~code_mun, weights = ~populacao,
               data = dados_comp)


mod_7 <- feols(log(rate_obitos_mulher_esc_baixa + 1) ~ treat | code_mun + ano,
               cluster = ~code_mun, weights = ~populacao,
               data = dados_comp)


mod_8 <- feols(log(rate_obitos_mulher_casada + 1) ~ treat | code_mun + ano,
      cluster = ~code_mun, weights = ~populacao,
      data = dados_comp)

mod_9 <- feols(log(rate_obitos_mulher_solteira + 1) ~ treat | code_mun + ano,
               cluster = ~code_mun, weights = ~populacao,
               data = dados_comp)



etable(mod_4, mod_5, mod_6, mod_7,
       keep = "treat", tex = TRUE)


# Causas não relacionadas

mod_8 <- feols(log(rate_obitos_doenca_infecciosa_mulher + 1) ~ treat | code_mun + ano,
               cluster = ~code_mun, weights = ~populacao,
               data = subset(dados_comp, 
                             between(time_to_treatment, -6, 6)))


mod_9 <- feols(log(rate_obitos_doenca_nao_infecciosa_mulher + 1) ~ treat | code_mun + ano,
               cluster = ~code_mun, weights = ~populacao,
               data = subset(dados_comp, 
                             between(time_to_treatment, -6, 6)))


mod_10 <- feols(log(rate_obitos_acid_transito_mulher + 1) ~ treat | code_mun + ano,
               cluster = ~code_mun, weights = ~populacao,
               data = subset(dados_comp, 
                             between(time_to_treatment, -6, 6)))

# Gerar tabelas
etable(mod_8, mod_9, mod_10,
       keep = "treat", tex = TRUE)




#* Event study ---------------------------------------------------------------------------------



# # Two-way fixed effect
# twfe <- dados_comp %>% 
#   do(broom::tidy(feols(log(rate_obitos_mulher + 1) ~ + i(time_to_treatment, ref = c(-1, -1000)) | code_mun + int_uf_ano, 
#                        data = ., cluster = ~code_mun), conf.int = TRUE)) %>% 
#   mutate(t =  as.double(str_replace_all(term, c("time_to_treatment::" = "", ":treated" = "")))) %>% 
#   filter(t > -6 & t < 6) %>% 
#   select(t, estimate, conf.low, conf.high) %>% 
#   # add in data for year -1
#   bind_rows(tibble(t = -1, estimate = 0, 
#                    conf.low = 0, conf.high = 0
#   )) %>% 
#   mutate(method = "TWFE")


# # Sun & Abraham method 
# SA <- dados_comp |> 
#   do(broom::tidy(feols(log(rate_violencia_domestica + 1) ~ sunab(group, ano) | code_mun + int_uf_ano, data = .,
#                        cluster = ~code_mun))) |>  
#   mutate(t =  as.double(str_replace(term, "ano::", "")),
#          conf.low = estimate - (qnorm(0.975)*std.error),
#          conf.high = estimate + (qnorm(0.975)*std.error)) |> 
#   filter(t > -6 & t < 6) |> 
#   select(t, estimate, conf.low, conf.high) |> 
#   mutate(method = "Sun & Abraham")
#


# Criar listas vazias para armazenamento
model.twfe <- vector(mode = "list")
model.sa <- vector(mode = "list")
plot.coef <- vector(mode = "list")

# Obter nome das variaveis
names_var <- names(dados_comp) |> 
  str_subset("^rate")

names_var <- names_var[-1]



# Loop para estimacoes
for (i in seq_along(names_var)){
  
  # Criar dados temporarios com variaveis a serem usadas na estimacao
  temp.dta = tibble(y = pull(dados_comp[, names_var[i]]), dados_comp[, (!colnames(dados_comp) %in% names_var[i])])
  
  # Rodar o modelo TWE
  model.twfe[[i]] <- broom::tidy(
    feols(log(y + 1) ~ + i(time_to_treatment, ref = c(-1, -1000)) | code_mun + ano, 
                 data = temp.dta, weights = ~populacao, cluster = ~code_mun), conf.int = TRUE
  ) |> 
    mutate(t =  as.double(str_replace_all(term, c("time_to_treatment::" = "", ":treated" = "")))) %>% 
    filter(t > -7 & t < 7) %>% 
    select(t, estimate, conf.low, conf.high) %>% 
    # add in data for year -1
    bind_rows(tibble(t = -1, estimate = 0, 
                     conf.low = 0, conf.high = 0
    )) %>% 
    mutate(method = "TWFE")
  
  
  # Rodar o modelo de Sun & Abraham
  model.sa[[i]] <- broom::tidy(
    feols(log(y + 1) ~ + sunab(group, ano) | code_mun + ano, 
          data = temp.dta, weights = ~populacao, cluster = ~code_mun)
  ) |> 
    mutate(t =  as.double(str_replace(term, "ano::", "")),
           conf.low = estimate - (qnorm(0.975)*std.error),
           conf.high = estimate + (qnorm(0.975)*std.error)) |> 
    filter(t > -7 & t < 7) |> 
    select(t, estimate, conf.low, conf.high) |> 
    mutate(method = "Sun & Abraham")
  
  # Juntar coeficientes
  coefs <- bind_rows(
    model.twfe[[i]],
    model.sa[[i]]
  ) 
  
  # Plotar coeficientes
  plot.coef[[i]] <- coefs |> 
    ggplot(aes(x = t, y = estimate, color = method)) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "#ce6d8b", size = .5, alpha = 0.75) + 
    #geom_vline(xintercept = -0.5, linetype = "dashed", size = .5) +
    annotate("rect", xmin = -.65, xmax = -.45, ymin = -Inf, ymax = Inf,
             alpha = .2,fill = "#4056f4") +
    geom_point(aes(x = t, y = estimate), position = position_dodge2(width = 0.5), size = 2.5) +
    geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), 
                   position = position_dodge2(width = 0.5), size = 1) +
    scale_color_manual(values = c("#58504a", "#4d9078")) +
    ggtitle("") +
    scale_x_continuous(
      breaks = seq(-6, 6, 1)
    ) +
    xlab("Ano relativo para abertura de delegacias") + 
    ylab("Coeficiente") +
    theme_bw() +
    theme(text = element_text(family = font_family)) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(size = 30, colour = "black"),
          axis.text.y = element_text(size = 30, colour = "black"),
          axis.title.x = element_text(size = 30, face = "bold"),
          axis.title.y = element_text(size = 30, face = "bold"),
          legend.text = element_text(size = 24),
          plot.title = element_text(size = 30, face = "bold"),
          panel.grid.minor = element_line(colour = "white")
          )
    
  # Salvar plots
  ggsave(
    filename = paste0(str_to_lower(names_var[i]), ".pdf"),
    plot = plot.coef[[i]], 
    device = cairo_pdf,
    path = "Figuras/Estimacao_fem/small",
    width = 14, height = 8.5, units = "in"
  )
  
}


#* Robustez ---------------------------------------------------------------------------------

# Eliminar estados mais populosos

code_uf_excluded <- c("SP", "MG", "RJ", 
                      "BA", "PR", "RS",
                      "PE", "CE")


list_excluded <- vector(mode = "list", length = length(code_uf_excluded)) 

for (i in seq_along(code_uf_excluded)){
  
  list_excluded[[i]] <- feols(log(rate_obitos_mulher + 1) ~ treat | code_mun + ano,
                              cluster = ~code_mun, weights = ~populacao,
                              data = subset(dados_comp, code_uf != code_uf_excluded[i])) |> 
    broom::tidy() |> 
    mutate(excluded = code_uf_excluded[i])
  
}

excluded_uf_plot <- bind_rows(list_excluded) |> 
  filter(term == "treat") |> 
  mutate(lower_95 = estimate - std.error * -qnorm((1-0.95)/2),
         upper_95 = estimate + std.error * -qnorm((1-0.95)/2),
         lower_90 = estimate - std.error * -qnorm((1-0.90)/2),
         upper_90 = estimate + std.error * -qnorm((1-0.90)/2)) |> 
  ggplot() +
  geom_hline(yintercept = 0, colour = "#ef6548", lty = 2, lwd = .8) +
  geom_point(aes(y = estimate, x = excluded), lwd = 3) +
  geom_errorbar(aes(x = excluded, ymin = lower_90, ymax = upper_90*1.10), ###
                col = "#252525", size = 1, width = .2) +
  # geom_vline(xintercept = true_est$estimate, colour = "#ef6548", lty = 2, lwd = 1) +
  #scale_x_continuous(labels = scaleFUN) +
  theme(panel.spacing = unit(2, "lines")) +
  theme_bw() +
  theme(strip.background = element_rect(color = "white", fill = "white", size = 1.5)) +
  theme(strip.text.x = element_text(size = 24, color = "black", face = "bold")) +
  theme(text = element_text(size = 16, family = "Palatino Linotype")) +
  theme(axis.text.x = element_text(size = 24, colour = "black"),
        axis.text.y = element_text(size = 24, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 24, 
                                    margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(face = "bold", size = 24, 
                                    margin = unit(c(0, 3, 0, 0), "mm")),
        plot.title = element_text(size = 32, face = "bold"),
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 26),
        panel.grid.major  = element_line(color = "#f0f0f0"),
        panel.grid.minor  = element_line(color = "#f0f0f0"),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  xlab("Excluded cities") + ylab("")

excluded_cities_plot

# Save plot
ggsave(file = "excluded_cities_plot_ols_fem.pdf", 
       plot = excluded_cities_plot, device = cairo_pdf, 
       path = "Figures/", dpi = 320,
       width = 12.0 , height = 7.5, units = "in")

