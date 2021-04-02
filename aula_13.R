####

# Aluguel ~ Salario
# Aluguel = f(Salario)
# Y = f(X)

# Aluguel = 0.30 * Salario +
# Y = B0 + B1X
library(tidyverse)

df <- readr::read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_series_temporais/master/data/Bike-Sharing-Dataset/day.csv")


glimpse(df)
df

df_transf <- df %>%
  # remover colunas irrelevantes
  select(-c(instant, workingday)) %>%
  # renomear algumas colunas
  rename(
    estacao = season,
    bikes_total = cnt,
    year = yr,
    month = mnth
  ) %>%
  # mudar weekday, que começa a contar do zero
  mutate(weekday = weekday + 1) %>%
  # transformar a variavel de feriado para texto
  mutate(holiday = as.character(holiday)) %>%
  # mudar os valores de algumas variaveis
  mutate(
    # substituir o codigo do ano  pelo ano real
    year = lubridate::year(dteday),
    # adicionar um leading zero no mês
    month = str_pad(month, width = 2, side = "left", pad = "0"),
    # converter weathersit para variavel do tipo factor
    weathersit = factor(weathersit,
                        levels = 1:4,
                        labels = c("muito bom", "bom", "ruim", "muito ruim")),
    # converter dia da semana para variavel do tipo factor
    weekday = factor(weekday,
                     levels = 1:7,
                     labels = c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab")),
    # fazer o mesmo para estacao
    estacao = factor(estacao,
                     levels = 1:4,
                     labels = c("Inverno", "Primavera", "Verao", "Outono")),
    # converter colunas numericas para escala normal (não-normalizada)
    temp = temp * 41,
    atemp = atemp * 50,
    hum = hum * 100,
    windspeed = windspeed * 67
  )

glimpse(df_transf)

