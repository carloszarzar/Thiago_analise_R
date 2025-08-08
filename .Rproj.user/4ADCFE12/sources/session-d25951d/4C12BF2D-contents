# ====================================================================
# ANÁLISE ESTATÍSTICA - QUESTIONÁRIO DE AQUICULTURA
# Análise de dados de questionário aplicado a 5 propriedades
# 4 grupos de perguntas: G1, G2, G3, G4
# ====================================================================

# Carregando bibliotecas necessárias
library(tidyverse)
library(ggplot2)
library(plotly)
library(corrplot)
library(pheatmap)
library(knitr)
library(gridExtra)
library(RColorBrewer)
library(scales)

# ====================================================================
# 1. CARREGAMENTO E PREPARAÇÃO DOS DADOS
# ====================================================================

# Assumindo que seus dados estão em um dataframe chamado 'df'
# df <- read.csv("seu_arquivo.csv") # descomente se necessário

# Verificação inicial dos dados
cat("=== ESTRUTURA DOS DADOS ===\n")
str(df)
cat("\n=== RESUMO DOS DADOS ===\n")
summary(df)

# Verificando valores únicos nas colunas principais
cat("\n=== ENTREVISTADOS ===\n")
table(df$entrevistado)
cat("\n=== GRUPOS ===\n")
table(df$grupo)
cat("\n=== RESPOSTAS PADRONIZADAS ===\n")
table(df$resposta_padronizada, useNA = "always")

# ====================================================================
# 2. ANÁLISE DESCRITIVA GERAL
# ====================================================================

# Criando mapeamento dos nomes das propriedades
propriedades <- c("1" = "Propriedade A", "2" = "Propriedade B",
                  "3" = "Propriedade C", "4" = "Propriedade D",
                  "5" = "Propriedade E")

# Adicionando coluna com nome das propriedades
df$propriedade <- factor(propriedades[as.character(df$entrevistado)],
                         levels = c("Propriedade A", "Propriedade B",
                                    "Propriedade C", "Propriedade D", "Propriedade E"))

# Contagem de perguntas por grupo
perguntas_por_grupo <- df %>%
  group_by(grupo) %>%
  summarise(
    n_perguntas = n_distinct(pergunta),
    n_respostas = n(),
    .groups = 'drop'
  )

cat("\n=== PERGUNTAS POR GRUPO ===\n")
print(perguntas_por_grupo)

# ====================================================================
# 3. ANÁLISE DE FREQUÊNCIAS
# ====================================================================

# Frequência geral de respostas
freq_geral <- df %>%
  count(resposta_padronizada) %>%
  mutate(
    percentual = round(n/sum(n)*100, 2),
    resposta_padronizada = factor(resposta_padronizada, levels = c("sim", "não", "NA"))
  ) %>%
  arrange(resposta_padronizada)

cat("\n=== FREQUÊNCIA GERAL DE RESPOSTAS ===\n")
print(freq_geral)

# Frequência por grupo
freq_por_grupo <- df %>%
  group_by(grupo, resposta_padronizada) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(grupo) %>%
  mutate(
    total = sum(n),
    percentual = round(n/total*100, 2)
  ) %>%
  arrange(grupo, resposta_padronizada)

cat("\n=== FREQUÊNCIA POR GRUPO ===\n")
print(freq_por_grupo)

# Frequência por propriedade
freq_por_propriedade <- df %>%
  group_by(propriedade, resposta_padronizada) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(propriedade) %>%
  mutate(
    total = sum(n),
    percentual = round(n/total*100, 2)
  ) %>%
  arrange(propriedade, resposta_padronizada)

cat("\n=== FREQUÊNCIA POR PROPRIEDADE ===\n")
print(freq_por_propriedade)

# ====================================================================
# 4. ANÁLISE COMPARATIVA ENTRE PROPRIEDADES
# ====================================================================

# Matriz de proporções de "sim" por propriedade e grupo
matriz_sim <- df %>%
  filter(resposta_padronizada != "NA") %>%
  group_by(propriedade, grupo) %>%
  summarise(
    total = n(),
    sim = sum(resposta_padronizada == "sim"),
    prop_sim = round(sim/total*100, 2),
    .groups = 'drop'
  ) %>%
  select(propriedade, grupo, prop_sim) %>%
  pivot_wider(names_from = grupo, values_from = prop_sim)

cat("\n=== PROPORÇÃO DE RESPOSTAS 'SIM' POR PROPRIEDADE E GRUPO (%) ===\n")
print(matriz_sim)

# Ranking das propriedades por conformidade geral
ranking_propriedades <- df %>%
  filter(resposta_padronizada != "NA") %>%
  group_by(propriedade) %>%
  summarise(
    total = n(),
    sim = sum(resposta_padronizada == "sim"),
    prop_sim = round(sim/total*100, 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(prop_sim))

cat("\n=== RANKING DE PROPRIEDADES POR CONFORMIDADE GERAL ===\n")
print(ranking_propriedades)

# ====================================================================
# 5. VISUALIZAÇÕES
# ====================================================================

# 5.1 Gráfico de barras - Frequência geral
p1 <- ggplot(freq_geral, aes(x = resposta_padronizada, y = n, fill = resposta_padronizada)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", percentual, "%)")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("sim" = "#2E8B57", "não" = "#DC143C", "NA" = "#808080")) +
  labs(
    title = "Distribuição Geral das Respostas",
    x = "Resposta",
    y = "Frequência",
    fill = "Resposta"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )

# 5.2 Gráfico de barras agrupadas - Por grupo
freq_grupo_plot <- df %>%
  count(grupo, resposta_padronizada) %>%
  group_by(grupo) %>%
  mutate(percentual = round(n/sum(n)*100, 1))

p2 <- ggplot(freq_grupo_plot, aes(x = grupo, y = n, fill = resposta_padronizada)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(percentual, "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("sim" = "#2E8B57", "não" = "#DC143C", "NA" = "#808080")) +
  labs(
    title = "Distribuição de Respostas por Grupo de Perguntas",
    x = "Grupo",
    y = "Frequência",
    fill = "Resposta"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# 5.3 Heatmap - Proporção de "sim" por propriedade e grupo
matriz_heatmap <- df %>%
  filter(resposta_padronizada != "NA") %>%
  group_by(entrevistado, grupo) %>%
  summarise(prop_sim = mean(resposta_padronizada == "sim") * 100, .groups = 'drop') %>%
  pivot_wider(names_from = grupo, values_from = prop_sim) %>%
  column_to_rownames("entrevistado") %>%
  as.matrix()

rownames(matriz_heatmap) <- paste("Propriedade", LETTERS[1:5])

p3 <- pheatmap::pheatmap(matriz_heatmap,
               cluster_rows = FALSE,
               cluster_cols = FALSE,
               display_numbers = TRUE,
               number_format = "%.1f",
               color = colorRampPalette(c("#DC143C", "#FFFF99", "#2E8B57"))(100),
               main = "Heatmap: % de Respostas 'Sim' por Propriedade e Grupo",
               fontsize_number = 10,
               angle_col = 0)

# 5.4 Gráfico de barras - Por propriedade
p4 <- ggplot(ranking_propriedades, aes(x = reorder(propriedade, prop_sim), y = prop_sim, fill = propriedade)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(prop_sim, "%")), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Ranking de Conformidade por Propriedade",
    x = "Propriedade",
    y = "Percentual de Respostas 'Sim' (%)",
    fill = "Propriedade"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  ylim(0, 100)

# ====================================================================
# 6. ANÁLISE ESTATÍSTICA INFERENCIAL
# ====================================================================

# 6.1 Teste Qui-quadrado - Independência entre grupos e respostas
tabela_contingencia <- table(df$grupo, df$resposta_padronizada)
cat("\n=== TABELA DE CONTINGÊNCIA: GRUPO x RESPOSTA ===\n")
print(tabela_contingencia)

# Teste qui-quadrado
teste_qui <- chisq.test(tabela_contingencia)
cat("\n=== TESTE QUI-QUADRADO: GRUPO x RESPOSTA ===\n")
print(teste_qui) # independência

# 6.2 Teste Qui-quadrado - Independência entre propriedades e respostas
tabela_prop <- table(df$entrevistado, df$resposta_padronizada)
teste_qui_prop <- chisq.test(tabela_prop)
cat("\n=== TESTE QUI-QUADRADO: PROPRIEDADE x RESPOSTA ===\n")
print(teste_qui_prop) # Não independência

# Interpretação: Se o valor do qui-quadrado calculado for maior que o valor crítico, rejeitamos a hipótese de independência, indicando uma associação entre as variáveis
# Ou se o p-valor for menor que o nível de significância 0.05

# ====================================================================
# 7. ANÁLISE DETALHADA POR GRUPO
# ====================================================================

# Função para analisar cada grupo individualmente
analisar_grupo <- function(grupo_name) {
  cat(paste("\n=== ANÁLISE DETALHADA:", grupo_name, "===\n"))

  dados_grupo <- df %>% filter(grupo == grupo_name)

  # Estatísticas básicas
  freq_grupo <- dados_grupo %>%
    count(resposta_padronizada) %>%
    mutate(percentual = round(n/sum(n)*100, 2))

  cat("Distribuição de respostas:\n")
  print(freq_grupo)

  # Por propriedade
  freq_prop_grupo <- dados_grupo %>%
    group_by(propriedade) %>%
    summarise(
      total = n(),
      sim = sum(resposta_padronizada == "sim", na.rm = TRUE),
      nao = sum(resposta_padronizada == "não", na.rm = TRUE),
      na_count = sum(is.na(resposta_padronizada) | resposta_padronizada == "NA"),
      prop_sim = round(sim/(sim + nao)*100, 2),
      .groups = 'drop'
    )

  cat("\nPor propriedade:\n")
  print(freq_prop_grupo)

  return(freq_prop_grupo)
}

# Análise de cada grupo
grupos <- c("G1", "G2", "G3", "G4")
resultados_grupos <- map(grupos, analisar_grupo)
names(resultados_grupos) <- grupos

# ====================================================================
# 8. IDENTIFICAÇÃO DE PADRÕES E OUTLIERS
# ====================================================================

# Identificar propriedades com desempenho muito diferente da média
analise_outliers <- df %>%
  filter(resposta_padronizada != "NA") %>%
  group_by(entrevistado, grupo) %>%
  summarise(prop_sim = mean(resposta_padronizada == "sim") * 100, .groups = 'drop') %>%
  group_by(grupo) %>%
  mutate(
    media_grupo = mean(prop_sim),
    desvio_grupo = sd(prop_sim),
    z_score = (prop_sim - media_grupo) / desvio_grupo,
    outlier = abs(z_score) > 1.5
  )

outliers_detectados <- analise_outliers %>%
  filter(outlier == TRUE) %>%
  arrange(grupo, desc(abs(z_score)))

cat("\n=== OUTLIERS DETECTADOS (Z-score > 1.5) ===\n")
if(nrow(outliers_detectados) > 0) {
  print(outliers_detectados)
} else {
  cat("Nenhum outlier significativo detectado.\n")
}

# ====================================================================
# 9. RELATÓRIO EXECUTIVO
# ====================================================================

cat("\n" ,"="*70, "\n")
cat("                    RELATÓRIO EXECUTIVO\n")
cat("="*70, "\n")

cat("\n1. VISÃO GERAL:\n")
cat("   - Total de respostas analisadas:", nrow(df), "\n")
cat("   - Propriedades avaliadas: 5\n")
cat("   - Grupos de perguntas: 4\n")
cat("   - Total de perguntas:", sum(perguntas_por_grupo$n_perguntas), "\n")

cat("\n2. CONFORMIDADE GERAL:\n")
prop_sim_geral <- round(sum(df$resposta_padronizada == "sim", na.rm = TRUE) /
                          sum(df$resposta_padronizada %in% c("sim", "não")) * 100, 2)
cat("   - Percentual geral de conformidade (Sim):", prop_sim_geral, "%\n")

cat("\n3. RANKING DE PROPRIEDADES:\n")
for(i in 1:nrow(ranking_propriedades)) {
  cat("   ", i, "°", ranking_propriedades$propriedade[i], ":",
      ranking_propriedades$prop_sim[i], "%\n")
}

cat("\n4. GRUPOS COM MAIOR CONFORMIDADE:\n")
ranking_grupos <- freq_por_grupo %>%
  filter(resposta_padronizada == "sim") %>%
  arrange(desc(percentual)) %>%
  select(grupo, percentual)

for(i in 1:nrow(ranking_grupos)) {
  cat("   ", i, "°", ranking_grupos$grupo[i], ":", ranking_grupos$percentual[i], "%\n")
}

# Exibindo os gráficos
print(p1)
print(p2)
print(p4)

cat("\n=== ANÁLISE CONCLUÍDA ===\n")
cat("Para visualizar o heatmap, execute: print(p3)\n")
cat("Para salvar os resultados, descomente as linhas de exportação no final do script.\n")

# ====================================================================
# 10. EXPORTAÇÃO DE RESULTADOS (OPCIONAL)
# ====================================================================

# Descomente as linhas abaixo para salvar os resultados:

# # Salvando gráficos
# ggsave("distribuicao_geral.png", p1, width = 10, height = 6, dpi = 300)
# ggsave("distribuicao_por_grupo.png", p2, width = 12, height = 6, dpi = 300)
# ggsave("ranking_propriedades.png", p4, width = 10, height = 6, dpi = 300)
#
# # Salvando tabelas
# write.csv(freq_geral, "frequencia_geral.csv", row.names = FALSE)
# write.csv(freq_por_grupo, "frequencia_por_grupo.csv", row.names = FALSE)
# write.csv(ranking_propriedades, "ranking_propriedades.csv", row.names = FALSE)
# write.csv(matriz_sim, "matriz_conformidade.csv", row.names = FALSE)
