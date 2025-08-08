# ===============================================================================
# ANÁLISE ESTATÍSTICA - QUESTIONÁRIO DE BIOSSEGURANÇA EM AQUICULTURA
# ===============================================================================

# Carregando bibliotecas necessárias
library(tidyverse)
library(corrplot)
library(pheatmap)
library(gridExtra)
library(knitr)
library(kableExtra)

# ===============================================================================
# 1. CARREGAMENTO E PREPARAÇÃO DOS DADOS
# ===============================================================================

# Assumindo que seus dados estão no dataframe 'df'
# Se não estiverem carregados, descomente e ajuste a linha abaixo:
# df <- read.csv("seu_arquivo.csv")

# Verificar estrutura dos dados
print("Estrutura dos dados:")
str(df)
print("\nPrimeiras linhas:")
head(df, 10)

# Verificar valores únicos em cada coluna
print("\nValores únicos por coluna:")
sapply(df, function(x) unique(x))

# ===============================================================================
# 2. ANÁLISE DESCRITIVA GERAL
# ===============================================================================

# Resumo geral dos dados
print("\n=== RESUMO GERAL DOS DADOS ===")
print(paste("Total de observações:", nrow(df)))
print(paste("Número de entrevistados:", length(unique(df$entrevistado))))
print(paste("Número de grupos:", length(unique(df$grupo))))

# Contagem de perguntas por grupo
perguntas_por_grupo <- df %>%
  group_by(grupo) %>%
  summarise(
    total_perguntas = n_distinct(pergunta),
    total_respostas = n(),
    .groups = 'drop'
  )

print("\nPerguntas por grupo:")
print(perguntas_por_grupo)

# Distribuição de respostas padronizadas
distribuicao_respostas <- df %>%
  count(resposta_padronizada, name = "frequencia") %>%
  mutate(percentual = round(frequencia/sum(frequencia)*100, 2)) %>%
  arrange(desc(frequencia))

print("\nDistribuição geral de respostas:")
print(distribuicao_respostas)

# ===============================================================================
# 3. ANÁLISE POR GRUPO DE PERGUNTAS
# ===============================================================================

print("\n=== ANÁLISE POR GRUPO ===")

# Distribuição de respostas por grupo
respostas_por_grupo <- df %>%
  group_by(grupo, resposta_padronizada) %>%
  summarise(frequencia = n(), .groups = 'drop') %>%
  group_by(grupo) %>%
  mutate(
    total_grupo = sum(frequencia),
    percentual = round(frequencia/total_grupo*100, 2)
  ) %>%
  arrange(grupo, desc(frequencia))

print("Distribuição de respostas por grupo:")
print(respostas_por_grupo)

# Proporção de "sim" por grupo
prop_sim_grupo <- df %>%
  group_by(grupo) %>%
  summarise(
    total_respostas = n(),
    respostas_sim = sum(resposta_padronizada == "sim", na.rm = TRUE),
    prop_sim = round(respostas_sim/total_respostas*100, 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(prop_sim))

print("\nProporção de respostas 'Sim' por grupo:")
print(prop_sim_grupo)

# ===============================================================================
# 4. ANÁLISE POR PROPRIEDADE (ENTREVISTADO)
# ===============================================================================

print("\n=== ANÁLISE POR PROPRIEDADE ===")

# Distribuição de respostas por entrevistado
respostas_por_entrevistado <- df %>%
  group_by(entrevistado, resposta_padronizada) %>%
  summarise(frequencia = n(), .groups = 'drop') %>%
  group_by(entrevistado) %>%
  mutate(
    total_entrevistado = sum(frequencia),
    percentual = round(frequencia/total_entrevistado*100, 2)
  ) %>%
  arrange(entrevistado, desc(frequencia))

print("Distribuição de respostas por propriedade:")
print(respostas_por_entrevistado)

# Proporção de "sim" por propriedade
prop_sim_propriedade <- df %>%
  group_by(entrevistado) %>%
  summarise(
    total_respostas = n(),
    respostas_sim = sum(resposta_padronizada == "sim", na.rm = TRUE),
    prop_sim = round(respostas_sim/total_respostas*100, 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(prop_sim))

print("\nProporção de respostas 'Sim' por propriedade:")
print(prop_sim_propriedade)

# Ranking das propriedades por conformidade
propriedades_labels <- c("1" = "Propriedade A", "2" = "Propriedade B",
                         "3" = "Propriedade C", "4" = "Propriedade D",
                         "5" = "Propriedade E")

prop_sim_propriedade_labeled <- prop_sim_propriedade %>%
  mutate(propriedade = propriedades_labels[as.character(entrevistado)]) %>%
  select(propriedade, prop_sim) %>%
  arrange(desc(prop_sim))

print("\nRanking de conformidade por propriedade:")
print(prop_sim_propriedade_labeled)

# ===============================================================================
# 5. ANÁLISE CRUZADA: GRUPO vs PROPRIEDADE
# ===============================================================================

print("\n=== ANÁLISE CRUZADA ===")

# Tabela de contingência: proporção de "sim" por grupo e propriedade
tabela_cruzada <- df %>%
  group_by(entrevistado, grupo) %>%
  summarise(
    total = n(),
    sim = sum(resposta_padronizada == "sim", na.rm = TRUE),
    prop_sim = round(sim/total*100, 2),
    .groups = 'drop'
  ) %>%
  select(entrevistado, grupo, prop_sim) %>%
  pivot_wider(names_from = grupo, values_from = prop_sim)

print("Tabela cruzada: % de respostas 'Sim' por propriedade e grupo:")
print(tabela_cruzada)

# Identificar pontos fortes e fracos por propriedade
analise_pontos <- df %>%
  group_by(entrevistado, grupo) %>%
  summarise(
    prop_sim = round(mean(resposta_padronizada == "sim", na.rm = TRUE)*100, 2),
    .groups = 'drop'
  ) %>%
  group_by(entrevistado) %>%
  summarise(
    melhor_grupo = grupo[which.max(prop_sim)],
    melhor_score = max(prop_sim),
    pior_grupo = grupo[which.min(prop_sim)],
    pior_score = min(prop_sim),
    diferenca = melhor_score - pior_score,
    .groups = 'drop'
  )

print("\nPontos fortes e fracos por propriedade:")
for(i in 1:nrow(analise_pontos)) {
  prop <- propriedades_labels[as.character(analise_pontos$entrevistado[i])]
  print(paste(prop, ":"))
  print(paste("  Melhor grupo:", analise_pontos$melhor_grupo[i],
              "(", analise_pontos$melhor_score[i], "%)"))
  print(paste("  Pior grupo:", analise_pontos$pior_grupo[i],
              "(", analise_pontos$pior_score[i], "%)"))
  print(paste("  Diferença:", analise_pontos$diferenca[i], "%"))
  print("")
}

# ===============================================================================
# 6. TESTES ESTATÍSTICOS
# ===============================================================================

print("\n=== TESTES ESTATÍSTICOS ===")

# Preparar dados para testes
df_teste <- df %>%
  filter(!is.na(resposta_padronizada)) %>%
  mutate(
    resposta_binaria = ifelse(resposta_padronizada == "sim", 1, 0)
  )

# Teste Qui-quadrado: Independência entre grupos e respostas
if(length(unique(df_teste$resposta_padronizada)) > 1 &&
   length(unique(df_teste$grupo)) > 1) {

  tabela_contingencia <- table(df_teste$grupo, df_teste$resposta_padronizada)
  print("Tabela de contingência - Grupo vs Resposta:")
  print(tabela_contingencia)

  teste_qui_grupo <- chisq.test(tabela_contingencia)
  print("\nTeste Qui-quadrado - Grupo vs Resposta:")
  print(teste_qui_grupo)
}

# O valor de p em um teste qui-quadrado de independência mede a probabilidade de obter resultados tão extremos quanto os observados, assumindo que as variáveis são independentes. Um valor de p pequeno (tipicamente menor que o nível de significância, como 0,05) indica evidências contra a hipótese de independência, sugerindo que as variáveis estão relacionadas.
# Portanto o teste Qui-quadrado de Independência entre grupos de perguntas e as respostas obtidas NÂO estão relacionadas


# Teste Qui-quadrado: Independência entre propriedades e respostas
if(length(unique(df_teste$resposta_padronizada)) > 1 &&
   length(unique(df_teste$entrevistado)) > 1) {

  tabela_contingencia_prop <- table(df_teste$entrevistado, df_teste$resposta_padronizada)
  print("\nTabela de contingência - Propriedade vs Resposta:")
  print(tabela_contingencia_prop)

  teste_qui_prop <- chisq.test(tabela_contingencia_prop)
  print("\nTeste Qui-quadrado - Propriedade vs Resposta:")
  print(teste_qui_prop)
}

# Portanto o teste Qui-quadrado de Independência entre Proprietários e as respostas obtidas
# no questionário tem alguma relação, ou seja, existe uma dependência.


# ANOVA para comparar médias de conformidade entre grupos
modelo_anova <- df_teste %>%
  group_by(entrevistado, grupo) %>%
  summarise(prop_sim = mean(resposta_binaria, na.rm = TRUE), .groups = 'drop')

if(nrow(modelo_anova) > 0) {
  anova_grupos <- aov(prop_sim ~ grupo, data = modelo_anova)
  print("\nANOVA - Diferenças entre grupos:")
  print(summary(anova_grupos))

  # Teste post-hoc (Tukey)
  if(summary(anova_grupos)[[1]][["Pr(>F)"]][1] < 0.05) {
    tukey_test <- TukeyHSD(anova_grupos)
    print("\nTeste Tukey HSD (comparações múltiplas):")
    print(tukey_test)
  }
}

# Não há diferença entre os grupos de perguntas para a proporção de sim nas respostas do questionário.

# ===============================================================================
# 7. IDENTIFICAÇÃO DE PADRÕES CRÍTICOS
# ===============================================================================

print("\n=== IDENTIFICAÇÃO DE PADRÕES CRÍTICOS ===")

# Perguntas com maior taxa de "não" ou "NA"
perguntas_criticas <- df %>%
  group_by(grupo, pergunta) %>%
  summarise(
    total = n(),
    nao = sum(resposta_padronizada == "não", na.rm = TRUE),
    na_vals = sum(is.na(resposta_padronizada) | resposta_padronizada == "NA"),
    prop_problematica = round((nao + na_vals)/total*100, 2),
    .groups = 'drop'
  ) %>%
  filter(prop_problematica >= 40) %>%  # Perguntas com 40%+ de problemas
  arrange(desc(prop_problematica))

print("Perguntas mais críticas (≥40% de 'não' ou 'NA'):")
if(nrow(perguntas_criticas) > 0) {
  print(perguntas_criticas)
} else {
  print("Não foram encontradas perguntas críticas com esse critério.")
}

# Propriedades com baixa conformidade geral
propriedades_criticas <- prop_sim_propriedade %>%
  filter(prop_sim < 70) %>%  # Menos de 70% de conformidade
  arrange(prop_sim)

print("\nPropriedades com conformidade <70%:")
if(nrow(propriedades_criticas) > 0) {
  for(i in 1:nrow(propriedades_criticas)) {
    prop_name <- propriedades_labels[as.character(propriedades_criticas$entrevistado[i])]
    print(paste(prop_name, ":", propriedades_criticas$prop_sim[i], "%"))
  }
} else {
  print("Todas as propriedades têm conformidade ≥70%.")
}

# ===============================================================================
# 8. RELATÓRIO EXECUTIVO
# ===============================================================================

print(strrep("=", 80))
print("RELATÓRIO EXECUTIVO")
print(strrep("=", 80))

# Conformidade geral
conformidade_geral <- round(mean(df$resposta_padronizada == "sim", na.rm = TRUE)*100, 2)
print(paste("CONFORMIDADE GERAL:", conformidade_geral, "%"))

# Melhor e pior grupo
melhor_grupo <- prop_sim_grupo$grupo[1]
melhor_score_grupo <- prop_sim_grupo$prop_sim[1]
pior_grupo <- prop_sim_grupo$grupo[nrow(prop_sim_grupo)]
pior_score_grupo <- prop_sim_grupo$prop_sim[nrow(prop_sim_grupo)]

print(paste("MELHOR GRUPO:", melhor_grupo, "(", melhor_score_grupo, "%)"))
print(paste("PIOR GRUPO:", pior_grupo, "(", pior_score_grupo, "%)"))

# Melhor e pior propriedade
melhor_propriedade <- propriedades_labels[as.character(prop_sim_propriedade$entrevistado[1])]
melhor_score_prop <- prop_sim_propriedade$prop_sim[1]
pior_propriedade <- propriedades_labels[as.character(prop_sim_propriedade$entrevistado[nrow(prop_sim_propriedade)])]
pior_score_prop <- prop_sim_propriedade$prop_sim[nrow(prop_sim_propriedade)]

print(paste("MELHOR PROPRIEDADE:", melhor_propriedade, "(", melhor_score_prop, "%)"))
print(paste("PIOR PROPRIEDADE:", pior_propriedade, "(", pior_score_prop, "%)"))

# Variabilidade entre propriedades
variabilidade <- round(sd(prop_sim_propriedade$prop_sim), 2)
print(paste("VARIABILIDADE ENTRE PROPRIEDADES (desvio padrão):", variabilidade, "%"))

print("\nCONCLUSÕES PRINCIPAIS:")
print("1. Análise de conformidade por grupo de perguntas concluída")
print("2. Comparação entre proprietários realizada")
print("3. Identificação de pontos críticos finalizada")
print("4. Testes estatísticos aplicados")

print("PROSSEGUIR COM VISUALIZAÇÕES GRÁFICAS? Execute o próximo bloco de código.")

print(strrep("=", 80))
