#### Análise questionário Thiago ####
# Data: 08/08/2025
# Autor: Carlos A. Zarzar
# carlos_zarzar@outolook.com
#-------------------------------------------------------------------------------
# Objetivo: Análise do questionário
# 5 entrevistado
#===============================================================================
# Preâmbulo
library(readxl) # pacote importar .xlsx

# Limpando área de trabalho
rm(list = ls())

# Importando dados
df <- readxl::read_xlsx('data.xlsx', sheet = 'data')
df

# Respostas unicas
resp <- unique(df$resposta)

# Transofrmar sim, não , NA
library(dplyr)
library(stringr)

df <- df %>%
  mutate(resposta_padronizada = case_when(
    str_detect(resposta, "^Sim|^Há dos lotes|^Há a emissão") ~ "sim",
    str_detect(resposta, "^Não|^Nãp") ~ "não",
    TRUE ~ NA_character_
  ))

# Para verificar o resultado
table(df$resposta_padronizada, useNA = "ifany")
unique(df$resposta_padronizada)
