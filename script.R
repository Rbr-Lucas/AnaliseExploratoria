##### Importando as libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)

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

##### Filtrando a base apenas para as linhas que contenham ao menos 1 NA
baseNA = base |>
  filter(if_any(everything(), is.na))

nrow(baseNA) # Não há nenhum NA na base de dados.

# Fazendo os plots
histIdade = base |>
  ggplot(aes(x = idade)) +
  geom_histogram(fill = 'lightblue') +
  theme_minimal() +
  labs(
    title = 'Distribuição da idade dos entrevistados.',
    x = 'Idade',
    y = 'Quantidade'
  )
ggsave('./histIdade.pdf', plot = histIdade)


boxTempo = base |>
  ggplot(aes(y = tempo_preso)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = 'Distribuição do tempo de prisão',
    x = '',
    y = 'Tempo preso'
  )
ggsave('./boxTempo.pdf', plot = boxTempo)


boxScore = base |>
  ggplot(aes(y = score_periculosidade)) +
  geom_boxplot() +
  facet_grid(cols = vars(escolaridade)) +
  theme_minimal() +
  labs(
    title = 'Distribuição do score de periculosidade',
    x = '',
    y = 'Score de Periculosidade'
  )
ggsave('./boxScore.pdf', plot = boxScore)


plotReincidencia = base |>
  ggplot(aes(x = reincidente)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = 'Distribuição da variável reincidente',
    x = 'Reincidência',
    y = 'Quantidade'
  )
ggsave('./plotReincidencia.pdf', plot = plotReincidencia)

