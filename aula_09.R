a <- c(1, 2, "c", 4)

a
class(a)

a[1] * 2

as.numeric(a)

class(TRUE)

class("TRUE")

b <- list(1, 2, "c", 4)
b

class(b[[1]])

class(b[[3]])

data_frame <- head(iris)
elemento_unico_inteiro <- 1
um_na <- NA
vetor_string <- letters[1:5]
modelo_regressao <- lm(mpg ~ wt, data = mtcars)

minha_lista <- list(tabela = data_frame,
                    "numero" = elemento_unico_inteiro,
                    # este elemento abaixo não vai possuir um nome
                    "indefinido" = um_na,
                    "letras" = vetor_string,
                    "regressao" = modelo_regressao)

minha_lista

class(minha_lista)

class(minha_lista[[1]])

class(minha_lista[[5]])

class(minha_lista$letras)

library(tidyverse)

map(minha_lista, class)

meu_vetor <- c(1, -3, 5, -10)

abs(meu_vetor)
class(abs(meu_vetor))

lista_numeros <- list(1, -3, 5, -10)
class(lista_numeros)

abs(lista_numeros)


map(lista_numeros, abs)
class(map(lista_numeros, abs))

map_dbl(lista_numeros, abs)

map(lista_numeros, abs)
lista_numeros %>% map(abs)


l <- list(v1 = c(1, 3, 5),
          v2 = c(2.0004789, 4.0010418, 6.89413),
          v3 = c(7, 8, 9))

map(l, round, 3)
map(l, round, digits = 4)

map(l, sqrt)

l %>% map(sum)

l %>% map(sqrt) %>% map(sum)


(l$v1^2)/3 * 2 / 7
#l$v1 * 0.2857143

# sintaxe de função anonima
map(l, function(x) (x^2)/3 * 2/7)

map(l, function(x) x %>% sqrt() %>% sum())
map(l, function(x) sum(sqrt(x)))

# sintaxe de formula
# generico: map(NOME_LISTA, ~ .x)

map(l, ~ (.x^2)/3 * 2/7)

map(l, ~ x.^2)
map(l, ~ .^2)

### projeto
library(tidyverse)
library(lubridate)

arquivo <- "dados/archive/DAYTON_hourly.csv"


df <- readr::read_csv(arquivo)

str_sub(colnames(df)[2], 1, -4)

df %>%
  rename(consumo = 2) %>%
  mutate(mes = month(Datetime)) %>%
  group_by(mes) %>%
  summarise(consumo_medio = mean(consumo))

retornar_consumo_mensal <- function(nome_arquivo){

  df <- readr::read_csv(nome_arquivo)
  nome_estacao <- str_sub(colnames(df)[2], 1, -4)

  df %>%
    rename(consumo = 2) %>%
    mutate(mes = month(Datetime),
           estacao = nome_estacao) %>%
    group_by(mes, estacao) %>%
    summarise(consumo_medio = mean(consumo))

}

arquivo <- "dados/archive/EKPC_hourly.csv"
retornar_consumo_mensal(arquivo)

vetor_arquivos <- list.files("dados/archive",
                             full.names = TRUE,
                             pattern = "_hourly.csv$")
vetor_arquivos
lst_dfs <- map(vetor_arquivos, retornar_consumo_mensal)
class(lst_dfs)

lst_dfs[[2]]

map(lst_dfs, class)

bind_rows(lst_dfs)






