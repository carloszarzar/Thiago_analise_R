# ===============================================================================
# VISUALIZAÇÕES GRÁFICAS - ANÁLISE DE BIOSSEGURANÇA EM AQUICULTURA
# ===============================================================================

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(viridis)

# Definir tema personalizado
tema_personalizado <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Paleta de cores
cores_grupos <- c("G1" = "#2E86AB", "G2" = "#A23B72", "G3" = "#F18F01", "G4" = "#C73E1D")
cores_propriedades <- viridis::viridis(5, option = "D")

# ===============================================================================
# GRÁFICO 1: DISTRIBUIÇÃO GERAL DE RESPOSTAS
# ===============================================================================

p1 <- df %>%
  filter(!is.na(resposta_padronizada)) %>%
  count(resposta_padronizada) %>%
  mutate(
    percentual = round(n/sum(n)*100, 1),
    resposta_padronizada = str_to_title(resposta_padronizada)
  ) %>%
  ggplot(aes(x = reorder(resposta_padronizada, n), y = n, fill = resposta_padronizada)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(n, "\n(", percentual, "%)")),
            vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Sim" = "#2E8B57", "Não" = "#CD5C5C", "NA" = "#708090")) +
  labs(
    title = "Distribuição Geral das Respostas",
    subtitle = "Frequência e percentual de cada tipo de resposta",
    x = "Tipo de Resposta",
    y = "Frequência",
    fill = "Resposta"
  ) +
  tema_personalizado +
  theme(legend.position = "none")

print(p1)

# ===============================================================================
# GRÁFICO 2: PROPORÇÃO DE "SIM" POR GRUPO
# ===============================================================================

dados_grupo_sim <- df %>%
  group_by(grupo) %>%
  summarise(
    total = n(),
    sim = sum(resposta_padronizada == "sim", na.rm = TRUE),
    prop_sim = sim/total*100,
    .groups = 'drop'
  ) %>%
  mutate(
    grupo_nome = case_when(
      grupo == "G1" ~ "G1: Avaliação de Riscos",
      grupo == "G2" ~ "G2: Equipamentos e Veículos",
      grupo == "G3" ~ "G3: Qualidade da Água",
      grupo == "G4" ~ "G4: Gerenciamento de Riscos"
    )
  )

p2 <- dados_grupo_sim %>%
  ggplot(aes(x = reorder(grupo_nome, prop_sim), y = prop_sim, fill = grupo)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(prop_sim, 1), "%")),
            hjust = -0.1, fontface = "bold", size = 4) +
  scale_fill_manual(values = cores_grupos) +
  coord_flip() +
  labs(
    title = "Conformidade por Grupo de Perguntas",
    subtitle = "Percentual de respostas 'Sim' por categoria",
    x = "Grupo de Perguntas",
    y = "Conformidade (%)",
    fill = "Grupo"
  ) +
  tema_personalizado +
  theme(legend.position = "none") +
  ylim(0, max(dados_grupo_sim$prop_sim) * 1.1)

print(p2)

# ===============================================================================
# GRÁFICO 3: CONFORMIDADE POR PROPRIEDADE
# ===============================================================================

dados_prop_sim <- df %>%
  group_by(entrevistado) %>%
  summarise(
    total = n(),
    sim = sum(resposta_padronizada == "sim", na.rm = TRUE),
    prop_sim = sim/total*100,
    .groups = 'drop'
  ) %>%
  mutate(
    propriedade = case_when(
      entrevistado == 1 ~ "Propriedade A",
      entrevistado == 2 ~ "Propriedade B",
      entrevistado == 3 ~ "Propriedade C",
      entrevistado == 4 ~ "Propriedade D",
      entrevistado == 5 ~ "Propriedade E"
    )
  )

p3 <- dados_prop_sim %>%
  ggplot(aes(x = reorder(propriedade, prop_sim), y = prop_sim, fill = propriedade)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(prop_sim, 1), "%")),
            vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = cores_propriedades) +
  labs(
    title = "Ranking de Conformidade das Propriedades",
    subtitle = "Percentual de respostas 'Sim' por propriedade",
    x = "Propriedade",
    y = "Conformidade (%)",
    fill = "Propriedade"
  ) +
  tema_personalizado +
  theme(legend.position = "none") +
  ylim(0, max(dados_prop_sim$prop_sim) * 1.1)

print(p3)

# ===============================================================================
# GRÁFICO 4: HEATMAP - CONFORMIDADE POR PROPRIEDADE E GRUPO
# ===============================================================================

dados_heatmap <- df %>%
  group_by(entrevistado, grupo) %>%
  summarise(
    prop_sim = mean(resposta_padronizada == "sim", na.rm = TRUE) * 100,
    .groups = 'drop'
  ) %>%
  mutate(
    propriedade = case_when(
      entrevistado == 1 ~ "Propriedade A",
      entrevistado == 2 ~ "Propriedade B",
      entrevistado == 3 ~ "Propriedade C",
      entrevistado == 4 ~ "Propriedade D",
      entrevistado == 5 ~ "Propriedade E"
    ),
    grupo_nome = case_when(
      grupo == "G1" ~ "Avaliação de Riscos",
      grupo == "G2" ~ "Equipamentos e Veículos",
      grupo == "G3" ~ "Qualidade da Água",
      grupo == "G4" ~ "Gerenciamento de Riscos"
    )
  )

p4 <- dados_heatmap %>%
  ggplot(aes(x = grupo_nome, y = propriedade, fill = prop_sim)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(round(prop_sim, 1), "%")),
            fontface = "bold", size = 3.5, color = "white") +
  scale_fill_gradient2(
    low = "#CD5C5C", mid = "#FFD700", high = "#2E8B57",
    midpoint = 50,
    name = "Conformidade\n(%)"
  ) +
  labs(
    title = "Mapa de Calor: Conformidade por Propriedade e Grupo",
    subtitle = "Percentual de respostas 'Sim' para cada combinação",
    x = "Grupo de Perguntas",
    y = "Propriedade"
  ) +
  tema_personalizado +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(p4)

# ===============================================================================
# GRÁFICO 5: DISTRIBUIÇÃO DE RESPOSTAS POR GRUPO (STACKED BAR)
# ===============================================================================

dados_stack <- df %>%
  filter(!is.na(resposta_padronizada)) %>%
  group_by(grupo, resposta_padronizada) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(grupo) %>%
  mutate(
    total = sum(n),
    prop = n/total*100,
    grupo_nome = case_when(
      grupo == "G1" ~ "G1: Avaliação\nde Riscos",
      grupo == "G2" ~ "G2: Equipamentos\ne Veículos",
      grupo == "G3" ~ "G3: Qualidade\nda Água",
      grupo == "G4" ~ "G4: Gerenciamento\nde Riscos"
    )
  )

p5 <- dados_stack %>%
  ggplot(aes(x = grupo_nome, y = prop, fill = str_to_title(resposta_padronizada))) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = ifelse(prop > 5, paste0(round(prop, 1), "%"), "")),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 3, color = "white") +
  scale_fill_manual(values = c("Sim" = "#2E8B57", "Não" = "#CD5C5C", "NA" = "#708090")) +
  labs(
    title = "Distribuição de Respostas por Grupo",
    subtitle = "Proporção de cada tipo de resposta em cada categoria",
    x = "Grupo de Perguntas",
    y = "Proporção (%)",
    fill = "Tipo de Resposta"
  ) +
  tema_personalizado

print(p5)

# ===============================================================================
# GRÁFICO 6: VARIABILIDADE ENTRE PROPRIEDADES POR GRUPO
# ===============================================================================

dados_variabilidade <- df %>%
  group_by(entrevistado, grupo) %>%
  summarise(prop_sim = mean(resposta_padronizada == "sim", na.rm = TRUE) * 100, .groups = 'drop') %>%
  mutate(
    propriedade = paste("Prop.", LETTERS[entrevistado]),
    grupo_nome = case_when(
      grupo == "G1" ~ "G1: Avaliação de Riscos",
      grupo == "G2" ~ "G2: Equipamentos e Veículos",
      grupo == "G3" ~ "G3: Qualidade da Água",
      grupo == "G4" ~ "G4: Gerenciamento de Riscos"
    )
  )

p6 <- dados_variabilidade %>%
  ggplot(aes(x = grupo_nome, y = prop_sim, color = propriedade, group = propriedade)) +
  geom_line(linewidth = 1.2, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = cores_propriedades) +
  labs(
    title = "Perfil de Conformidade das Propriedades",
    subtitle = "Variação da conformidade entre grupos para cada propriedade",
    x = "Grupo de Perguntas",
    y = "Conformidade (%)",
    color = "Propriedade"
  ) +
  tema_personalizado +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 100)

print(p6)

# ===============================================================================
# GRÁFICO 7: BOX PLOT - DISTRIBUIÇÃO DE CONFORMIDADE POR GRUPO
# ===============================================================================

dados_boxplot <- df %>%
  group_by(entrevistado, grupo) %>%
  summarise(prop_sim = mean(resposta_padronizada == "sim", na.rm = TRUE) * 100, .groups = 'drop') %>%
  mutate(
    grupo_nome = case_when(
      grupo == "G1" ~ "G1: Avaliação\nde Riscos",
      grupo == "G2" ~ "G2: Equipamentos\ne Veículos",
      grupo == "G3" ~ "G3: Qualidade\nda Água",
      grupo == "G4" ~ "G4: Gerenciamento\nde Riscos"
    )
  )

p7 <- dados_boxplot %>%
  ggplot(aes(x = grupo_nome, y = prop_sim, fill = grupo)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  geom_jitter(aes(color = grupo), alpha = 0.8, width = 0.2, size = 3) +
  scale_fill_manual(values = cores_grupos) +
  scale_color_manual(values = cores_grupos) +
  labs(
    title = "Distribuição de Conformidade por Grupo",
    subtitle = "Box plots mostrando variabilidade entre propriedades",
    x = "Grupo de Perguntas",
    y = "Conformidade (%)",
    fill = "Grupo",
    color = "Grupo"
  ) +
  tema_personalizado +
  theme(legend.position = "none") +
  ylim(0, 100)

print(p7)

# ===============================================================================
# DASHBOARD RESUMO
# ===============================================================================

# Criar painel com 4 gráficos principais
dashboard <- grid.arrange(
  p1 + theme(plot.title = element_text(size = 10)),
  p2 + theme(plot.title = element_text(size = 10)),
  p3 + theme(plot.title = element_text(size = 10)),
  p4 + theme(plot.title = element_text(size = 10)),
  ncol = 2, nrow = 2,
  top = "DASHBOARD - ANÁLISE DE BIOSSEGURANÇA EM AQUICULTURA"
)

print(dashboard)

# ===============================================================================
# EXPORTAÇÃO DOS GRÁFICOS (opcional)
# ===============================================================================

# Salvar gráficos individualmente (descomente se necessário)
# ggsave("distribuicao_geral.png", p1, width = 10, height = 6, dpi = 300)
# ggsave("conformidade_grupos.png", p2, width = 10, height
