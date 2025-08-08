# Script para gerar relatório de transformação de dados
# Autor: Seu Nome
# Data: Sys.Date()

library(dplyr)
library(stringr)
library(knitr)

# Função para gerar relatório de transformação
gerar_relatorio_transformacao <- function(df, coluna_original, coluna_nova = "resposta_padronizada") {

  cat("=== RELATÓRIO DE TRANSFORMAÇÃO DE DADOS ===\n")
  cat("Data:", as.character(Sys.Date()), "\n")
  cat("Coluna analisada:", coluna_original, "\n\n")

  # 1. Análise dos dados originais
  cat("1. DADOS ORIGINAIS\n")
  cat("------------------\n")
  respostas_unicas <- unique(df[[coluna_original]])
  cat("Total de registros:", nrow(df), "\n")
  cat("Respostas únicas:", length(respostas_unicas), "\n\n")

  # Frequência das respostas originais
  freq_original <- table(df[[coluna_original]])
  cat("Frequência das respostas originais:\n")
  print(freq_original)
  cat("\n")

  # 2. Transformação aplicada
  cat("2. TRANSFORMAÇÃO APLICADA\n")
  cat("-------------------------\n")

  # Aplicar transformação se a coluna não existir
  if(!coluna_nova %in% names(df)) {
    df <- df %>%
      mutate(!!coluna_nova := case_when(
        str_detect(.data[[coluna_original]], "^Sim|^Há dos lotes|^Há a emissão") ~ "sim",
        str_detect(.data[[coluna_original]], "^Não|^Nãp") ~ "não",
        TRUE ~ NA_character_
      ))
  }

  # Regras aplicadas
  cat("Regras de transformação:\n")
  cat("- 'sim': Respostas iniciadas com 'Sim' + casos específicos ('Há dos lotes', 'Há a emissão')\n")
  cat("- 'não': Respostas iniciadas com 'Não' ou 'Nãp'\n")
  cat("- NA: Demais casos\n\n")

  # 3. Resultados
  cat("3. RESULTADOS DA TRANSFORMAÇÃO\n")
  cat("------------------------------\n")

  freq_transformada <- table(df[[coluna_nova]], useNA = "ifany")
  cat("Frequência das respostas transformadas:\n")
  print(freq_transformada)
  cat("\n")

  # Percentuais
  total <- nrow(df)
  percentuais <- round(freq_transformada/total*100, 2)
  cat("Percentuais:\n")
  print(percentuais)
  cat("\n")

  # 4. Mapeamento detalhado
  cat("4. MAPEAMENTO DETALHADO\n")
  cat("-----------------------\n")

  mapeamento <- df %>%
    count(.data[[coluna_original]], .data[[coluna_nova]]) %>%
    arrange(.data[[coluna_nova]], .data[[coluna_original]])

  print(mapeamento)
  cat("\n")

  # 5. Casos que viraram NA
  cat("5. CASOS TRANSFORMADOS EM NA\n")
  cat("-----------------------------\n")

  casos_na <- df %>%
    filter(is.na(.data[[coluna_nova]])) %>%
    count(.data[[coluna_original]], sort = TRUE)

  if(nrow(casos_na) > 0) {
    print(casos_na)
  } else {
    cat("Nenhum caso foi transformado em NA.\n")
  }
  cat("\n")

  # 6. Resumo final
  cat("6. RESUMO FINAL\n")
  cat("---------------\n")

  n_sim <- sum(df[[coluna_nova]] == "sim", na.rm = TRUE)
  n_nao <- sum(df[[coluna_nova]] == "não", na.rm = TRUE)
  n_na <- sum(is.na(df[[coluna_nova]]))
  taxa_padronizacao <- round((n_sim + n_nao) / total * 100, 2)

  cat("- Total de registros:", total, "\n")
  cat("- Respostas 'sim':", n_sim, paste0("(", round(n_sim/total*100, 2), "%)"), "\n")
  cat("- Respostas 'não':", n_nao, paste0("(", round(n_nao/total*100, 2), "%)"), "\n")
  cat("- Valores NA:", n_na, paste0("(", round(n_na/total*100, 2), "%)"), "\n")
  cat("- Taxa de padronização bem-sucedida:", taxa_padronizacao, "%\n")
  cat("- Redução de categorias: de", length(respostas_unicas), "para 3\n\n")

  cat("=== FIM DO RELATÓRIO ===\n")

  return(df)
}

# Função para salvar relatório em arquivo
salvar_relatorio <- function(df, coluna_original, nome_arquivo = "relatorio_transformacao.txt") {

  # Capturar a saída da função de relatório
  sink(nome_arquivo)
  resultado <- gerar_relatorio_transformacao(df, coluna_original)
  sink()

  cat("Relatório salvo em:", nome_arquivo, "\n")
  return(resultado)
}

# Exemplo de uso:
# ================

# 1. Gerar relatório no console
# df_atualizado <- gerar_relatorio_transformacao(df, "resposta")

# 2. Salvar relatório em arquivo
# df_atualizado <- salvar_relatorio(df, "resposta", "meu_relatorio.txt")

# 3. Gerar relatório HTML usando R Markdown
# rmarkdown::render("relatorio_transformacao.Rmd", output_file = "relatorio_final.html")

# 4. Análise rápida das transformações
analise_rapida <- function(df, coluna_original, coluna_nova = "resposta_padronizada") {

  # Tabela cruzada
  tabela_cruzada <- table(df[[coluna_original]], df[[coluna_nova]], useNA = "ifany")

  cat("ANÁLISE RÁPIDA:\n")
  cat("===============\n")
  print(tabela_cruzada)
  cat("\n")

  # Casos problemáticos (se houver)
  problemas <- df %>%
    group_by(.data[[coluna_original]], .data[[coluna_nova]]) %>%
    summarise(n = n(), .groups = 'drop') %>%
    filter(.data[[coluna_original]] != .data[[coluna_nova]] |
             is.na(.data[[coluna_nova]]))

  if(nrow(problemas) > 0) {
    cat("Transformações realizadas:\n")
    print(problemas)
  }
}

# Exemplo: analise_rapida(df, "resposta")
