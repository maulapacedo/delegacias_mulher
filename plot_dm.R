#--------------------------#
# Project name: Delegacias da Mulher
# Purpose of the script: Informacoes das Delegacias da Mulher
# Author: Antonio Vinicius
# Created on: Tue Jul 26 08:34:28 2022
#--------------------------#
# Insert notes:
# 1.
# 2.
#--------------------------#

# Options
rm(list = ls())


# Load packages -----------------------------------------------------------

pacman::p_load(dplyr, geobr, ggplot2, readxl, tidyr,
               showtext, stringr)

# Loading Google fonts (https://fonts.google.com/)
font_add_google("Signika Negative")
showtext_auto()



# Load data DM's ---------------------------------------------------------------

dm_data <- readxl::read_excel("Dados/Delegacias/levantamento_delegacias.xlsx") |> 
  mutate(
    ano_criacao = stringr::str_replace(ano_criacao, "-", ""),
    ano_criacao = as.numeric(ano_criacao),
    municipio = stringr::str_to_upper(municipio), 
    ano_criacao = as.Date(as.character(ano_criacao), format = "%Y"),
    ano_criacao = format(ano_criacao, format = "%Y")
  ) |> 
  #Retirar variaveis desnecessarias 
  select(-c("telefone1", "telefone2", "e-mail", "observacao", "telefone_correto",
            "horario_correto", "natureza", "projeto",
            "complemento", "obs_pandemia", "url", "endereco_correto", "existe_delegacia",
            "delegacia_mulher", "funcionamento_pandemia", "24horas", "presencial", 
            "online", "dias_funcionamento")) |> 
  #Corrigir nomes municipios 
  mutate(
   municipio = case_when(
    municipio == "VALPARAÍSO" ~ "VALPARAÍSO DE GOIÁS",
    municipio == "TAGUATINGA" ~ "BRASÍLIA", 
    municipio == "CURUPURU" ~ "CURURUPU",
    municipio == "ABAETE" ~ "ABAETÉ",
    municipio == "MARABA" ~ "MARABÁ",
    municipio == "IBIRITE" ~ "IBIRITÉ",
    municipio == "GUABIRA" ~ "GUARABIRA",
    municipio == "SANTO AMARO" ~ "RECIFE",
    municipio == "PARQUE PIAUÍ" ~ "TERESINA",
    municipio == "RESENTE" ~ "RESENDE",
    municipio == "GUARAJÁ MIRIM" ~ "GUAJARÁ-MIRIM",
    municipio == "ESTÂNCIA VELHA" ~ "ESTÂNCIA VELHA", #nao consegui
    municipio == "GUAIBA" ~ "GUAÍBA",
    municipio == "SANTANA DO LIVRAMENTO" ~ "SANT'ANA DO LIVRAMENTO",
    municipio == "EMBU" ~ "EMBU DAS ARTES",
    municipio == "FERNANDOPOLIS" ~ "FERNANDÓPOLIS",
    municipio == "JARAGUÁ DO SU" ~ "JARAGUÁ DO SUL",
    municipio == "COLINAS" ~ "COLINAS DO TOCANTINS",
    municipio == "ARIQUEMES" ~ "ARIQUEMES", 
    TRUE ~ municipio
    )
  )

#Data structure
glimpse(dm_data)  


# Data municipalities IBGE -----------------------------------------------------

#Load data
municipios_brasil <- geobr::read_municipality("all", 
                                              year = 2020)

municipios_brasil <- municipios_brasil |> 
  mutate(
    municipio = stringr::str_to_upper(name_muni), 
    uf = abbrev_state
  )

#Data structure
glimpse(municipios_brasil)

#Save data
readr::write_rds(municipios_brasil, 
                 "Dados/IBGE/municipios_brasil.rds",
                 compress = "gz")


# Full Join ---------------------------------------------------------------

#Full join DM's and municipalities

dm_data_fulljoin <- full_join(x = dm_data,
                              y = municipios_brasil,
                              by = c("municipio" = "municipio", "uf" = "uf"))

#Filtrar apenas para as delegacias 
dm_data_2 <- dm_data_fulljoin |> 
  filter(is.na(categoria) == FALSE) 
  
glimpse(dm_data_2)
#Somar a quantidade de municipios NA
sum(is.na(dm_data_2$name_muni))

anos_criacao <- dm_data |> 
                 filter(!is.na(ano_criacao)) |>  
                 group_by(code_muni) |> 
                 summarise(n_delegacias = n()) |> 
                 ungroup() |> 
                 filter(!is.na(ano_criacao)) |>
                 group_by(ano_criacao) |> 
                 summarise(min(ano_criacao))
    

# Evolucao das DMs ----------------------------------------------------------------------------

# Numero de delegacias criadas por ano
dm_criacao_ano <- dm_data |>
  group_by(ano_criacao) |> 
  summarise(n_dm = n()) |>  
  ggplot() +
  geom_col(
    aes(x = ano_criacao, y = n_dm),
    fill = "#7F7EFF", col = "white"
  ) +
  scale_x_continuous(
    breaks = seq(1985, 2020, 5)
  ) +
  xlab("Ano de criação") + 
  ylab("Número de delegacias criadas") +
  theme_minimal() +
  theme(text = element_text(family = "Signika Negative")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 30, colour = "black"),
        axis.text.y = element_text(size = 30, colour = "black"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 24))


ggsave(
  filename = "dm_criacao_ano.pdf",
  plot = dm_criacao_ano, 
  device = cairo_pdf,
  path = "Figuras/",
  width = 14, height = 8.5, units = "in"
)

# Numero acumulado de delegacias

dm_criacao_acumulado <- dm_data |>
  group_by(ano_criacao) |> 
  summarise(
    n_dm = n()
    ) |>
  ungroup() |> 
  complete(
    ano_criacao = 1985:2020
    ) |> 
  mutate(
    n_dm = ifelse(is.na(n_dm), 0, n_dm),
    dm_acumulada = cumsum(n_dm)
    ) |> 
  ggplot() +
  geom_col(
    aes(x = ano_criacao, y = dm_acumulada),
    fill = "#7F7EFF", col = "white"
  ) +
  scale_x_continuous(
    breaks = seq(1985, 2020, 5)
  ) +
  xlab("Ano de criação") + 
  ylab("Número acumulado de delegacias") +
  theme_minimal() +
  theme(text = element_text(family = "Signika Negative")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 30, colour = "black"),
        axis.text.y = element_text(size = 30, colour = "black"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 24))

ggsave(
  filename = "dm_criacao_acumulado.pdf",
  plot = dm_criacao_acumulado, 
  device = cairo_pdf,
  path = "Figuras/",
  width = 14, height = 8.5, units = "in"
)




# Mapas ---------------------------------------------------------------------------------------

mapa_br <- geobr::read_state(year = 2020)

mapa_dm <- mapa_br |> 
  ggplot() + 
  geom_sf(
    fill = "#F0F6F6", col = "#4E4B5C"
  ) +
  geom_point(
    data = dm_data,
    aes(x = longitude, y = latitude),
    col = "#E27396"
  ) + 
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_minimal() +
  theme(text = element_text(family = "Signika Negative")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 30, colour = "black"),
        axis.text.y = element_text(size = 30, colour = "black"),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.title.y = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 24))

ggsave(
  filename = "mapa_dm.png",
  plot = mapa_dm, 
  device = cairo_pdf,
  path = "Figuras/",
  width = 14, height = 8.5, units = "in"
)









