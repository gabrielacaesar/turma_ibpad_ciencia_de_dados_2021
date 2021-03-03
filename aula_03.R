# aula 03
# professora gabriela

idade <- 28

uf <- c("PB", "PE", "PI", "RN  ", "RN", "SE", "MA", "BA", "CE")
regiao <- "Nordeste"
temperatura <- c(29, 30, 28, 27, 26, 30, 27, 26, 26)
renda <- c(2000, 1000, 500, 1800, 2000, 1200, 1100, 1900, 2000)

uf_nordeste <- data.frame(uf, regiao, temperatura, renda)

uf_nordeste$temperatura
uf_nordeste$uf

unique(uf_nordeste$temperatura)
unique(uf_nordeste$uf)

View(uf_nordeste)

vetor_num_repetidos <- c(rep(2, 50))
vetor_num_repetidos

str(uf_nordeste)

ano_atual <- "2021"
class(ano_atual)

ano_atual - 1992

as.numeric(ano_atual) - 1992

#######
library(tidyverse)

df <- readr::read_csv2("dados/Ano-2020.csv", n_max = 10)

?read_csv2()

is.na(df$cpf)
is.na(df$ideCadastro)
is.na(df$txNomeParlamentar)

linha_3_a_10 <- df[3:10,]

linha_3_a_10

complete.cases(df$cpf)
complete.cases(df$txNomeParlamentar)

variavel <- 300

if(variavel >= 500) {
  variavel + 500
  #executa uma tarefa se operação resultar TRUE
} else {
  variavel - 500
  #executa outra tarefa se operação resultar FALSE
}

### outro ifelse
x <- 300

ifelse(x >= 500, 'executa essa tarefa se TRUE', 'executa outra se FALSE')

### outro ifelse

a <- 9823

if(a >= 10000) {
  b <- 'VALOR ALTO'
} else if(a < 10000 & a >= 1000) {
  b <- 'VALOR MEDIO'
} else if(a < 1000) {
  b <- 'VALOR BAIXO'
}

b

a <- 839
c <- ifelse(a >= 10000, 
            'VALOR ALTO', 
     ifelse(a < 10000 & a >= 1000, 
     'VALOR MEDIO', 
            'VALOR BAIXO'))
c


for(x in c(1, 2, 3, 4, 5)) {
  print(x * 10 / 2)
}

###
for(i in 1:1000){
  if((i %% 29 == 0) & (i %% 3 == 0)){
    print(i)
  }
}

#### 

numeros <- 1:100
min(numeros)
max(numeros)
mean(numeros)
median(numeros)
head(numeros)
tail(numeros)
glimpse(numeros)

soma_10 <- function(i){
i + 10
}

soma_10(6)

calcula_imc <- function(a, b){
  a / b ^ 2
}

calcula_imc(60, 1.71)

###################
# parte 2

library(tidyverse)

df <- readr::read_csv2("dados/Ano-2020.csv")

glimpse(df)
colnames(df)
head(df)
tail(df)

df[10:1000,]

unique(df$sgPartido)

df[50:55, 1]

df[1000:1100, c('sgPartido', 'cpf')]

df <- df %>%
  #select_if(is.numeric) 
  #select(starts_with("vlr"))
  select(-vlrGlosa) %>%
  mutate(vlrDocumento = as.numeric(vlrDocumento))
  #filter(sgPartido %in% c("PSDB", "NOVO", "SOLIDARIEDADE", "PTB"))

df %>% 
  select(txNomeParlamentar, vlrLiquido, sgPartido) %>% 
  mutate(txNomeParlamentar = tolower(txNomeParlamentar)) %>% 
  mutate(sgPartido = tolower(sgPartido)) %>%
  tail()

acima_10mil <- df %>% 
  select(cpf, txNomeParlamentar, txtDescricao, vlrDocumento) %>% 
  mutate(vlrDocumento = as.numeric(vlrDocumento)) %>%
  filter(vlrDocumento >= 100000) 

df %>%
  janitor::clean_names()
  
# ?janitor::clean_names()

gastos_deputado <- df %>% 
  select(cpf, txNomeParlamentar, txtDescricao, vlrDocumento, sgPartido) %>% 
  mutate(vlrDocumento = as.numeric(vlrDocumento)) %>%
  group_by(txNomeParlamentar, sgPartido) %>%
  summarise(soma_total = sum(vlrDocumento),
            qt_nota_fiscal = n(),
            gasto_por_nota_fiscal = soma_total / qt_nota_fiscal) %>%
  arrange(desc(soma_total))

gastos_mensais <- df %>% 
  select(cpf, txNomeParlamentar, txtDescricao, vlrDocumento, sgPartido, numMes) %>% 
  mutate(vlrDocumento = as.numeric(vlrDocumento)) %>%
  group_by(numMes) %>%
  summarise(soma_total = sum(vlrDocumento)) %>%
  arrange(desc(soma_total))


