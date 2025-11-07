##### Importando as libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)
library(corrplot)

##### Importando a base de dados
base = read_excel('./Base_trabalho.xlsx')

##### Tratando as variaveis
base = base |>
  mutate(
    escolaridade = factor(escolaridade, levels = c('1', '2', '3'), labels = c('Fundamental', 'Médio', 'Superior')),
    filhos = factor(filhos, levels = c('0', '1'), labels = c('Não', 'Sim')),
    sexo = factor(sexo, levels = c('0', '1'), labels = c('Feminino', 'Masculino')),
    casado = factor(casado, levels = c('0', '1'), labels = c('Não', 'Sim')),
    reincidente = factor(reincidente, levels = c(0, 1), labels = c('Não', 'sim'))
  )


##### Realizando a análise exploratória
analise = base |>
  summarise(
    media_score = mean(score_periculosidade, na.rm = TRUE),
    q1_score = quantile(score_periculosidade, 0.25, na.rm = TRUE),
    mediana_score = median(score_periculosidade, na.rm = TRUE),
    q3_score = quantile(score_periculosidade, 0.75, na.rm = TRUE),
    
    media_idade = mean(idade, na.rm = TRUE),
    q1_idade = quantile(idade, 0.25, na.rm = TRUE),
    mediana_idade = median(idade, na.rm = TRUE),
    q3_idade = quantile(idade, 0.75, na.rm = TRUE),
    
    media_tempo = mean(tempo_preso, na.rm = TRUE),
    q1_tempo = quantile(tempo_preso, 0.25, na.rm = TRUE),
    mediana_tempo = median(tempo_preso, na.rm = TRUE),
    q3_tempo = quantile(tempo_preso, 0.75, na.rm = TRUE)
  ) |>
  tibble()

#### Gráfico de dispersão
plotDispersao = base |>
  ggplot(aes(x = tempo_preso, y = score_periculosidade)) +
  geom_point(size = 2) +
  labs(
    title = 'Gráfico de dispersão para o Tempo Preso x Score de Periculosidade',
    x = 'Tempo Preso',
    y = 'Score Periculosidade'
  ) +
  theme_minimal()
plotDispersao


### Correlação
valorCorrelacao = base |>
  select(tempo_preso, score_periculosidade) |>
  cor(use = "pairwise.complete.obs")

valorCorrelacao |>
  corrplot(method = "color",
           tl.col = "black",      # cor dos nomes
           tl.srt = 45,           # rotação
           tl.cex = 1,          # aumenta nomes das variáveis
           addCoef.col = "white", # cor dos números
           number.cex = 1,      # aumenta números da correlação
           type = "upper",
           cl.pos = "n")


#### Analise exploratoria 2
analise2 = base |>
  summarise(
    var_score = var(score_periculosidade, na.rm = TRUE),
    sd_score = sd(score_periculosidade, na.rm = TRUE),
    amp_score = max(score_periculosidade, na.rm = TRUE) - min(score_periculosidade, na.rm = TRUE),
    
    var_idade = var(idade, na.rm = TRUE),
    sd_idade = sd(idade, na.rm = TRUE),
    amp_idade = max(idade, na.rm = TRUE) - min(idade, na.rm = TRUE),
    
    var_tempo = var(tempo_preso, na.rm = TRUE),
    sd_tempo = sd(tempo_preso, na.rm = TRUE),
    amp_tempo = max(tempo_preso, na.rm = TRUE) - min(tempo_preso, na.rm = TRUE)
  )
