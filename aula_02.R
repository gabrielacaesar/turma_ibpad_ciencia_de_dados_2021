# aula 02 
# professora gabriela 

round(1.3333, digits = 2)  

pi

resultado <- round(pi)

salario <- 1000
aluguel <- 300

sobra_mensal <- salario - aluguel
sobra_mensal

salario <- 1300

100 > 1
100 < 1

1 == 1
1 == 2
1 != 2

nome <- "gabriela"

1 + 1
1 - 1
1 / 3
10 * 3

idade <- "27"

idade <- as.numeric(idade)

falso <- "FALSE"
falso <- as.logical(falso)

list.files()
getwd()

setwd("/Users/gabrielacaesar/Documents/")

Sys.Date()
Sys.time()

aniversario <- "2021-08-27"
aniversario

tempo_faltante <- as.Date(aniversario) - Sys.Date()
tempo_faltante

setwd("/Users/gabrielacaesar/Documents/r_projetos/curso_r_ibpad/")

dir.create("dados-2")

###  %>% 

#######################################

library(tidyverse)
library(readxl) # excel 
library(data.table)

getwd()

agenda <- readr::read_csv("dados/agendas_ministerio_comunicacoes/Agenda Ministro - 16-06-2020 a 18-02-2021.csv")

agenda <- read_csv("dados/agendas_ministerio_comunicacoes/Agenda Ministro - 16-06-2020 a 18-02-2021.csv")

glimpse(agenda)

unique(agenda$`Local do Compromisso`)

nrow(agenda)
colnames(agenda)

populacao <- readxl::read_excel("Downloads/turma_ibpad_ciencia_de_dados_2021-main/dados/ibge/estimativa_dou_2020.xls",
                                skip = 1,
                                sheet = "Municípios")

head(populacao)
head(populacao, 12)

tail(populacao)
tail(populacao, 12)

deputado <- readxl::read_xls("Downloads/turma_ibpad_ciencia_de_dados_2021-main/dados/camara_dos_deputados/deputado.xls")

glimpse(deputado)

summary(deputado)

class(deputado$`Mês Aniversário`)

municipios <- data.table::fread("https://raw.githubusercontent.com/betafcc/Municipios-Brasileiros-TSE/master/municipios_brasileiros_tse.csv",
                                nrows = 10,
                                select = c("codigo_tse", "codigo_ibge"))

agenda <- readr::read_csv("Downloads/turma_ibpad_ciencia_de_dados_2021-main/dados/agendas_ministerio_comunicacoes/Agenda Ministro - 16-06-2020 a 18-02-2021.csv")


candidatos_2012 <- fread("Downloads/turma_ibpad_ciencia_de_dados_2021-main/dados/tse/consulta_cand_2012/consulta_cand_2012_AC.txt",
                         encoding = "Latin-1")

glimpse(candidatos_2012)

?read_excel() # consultar a documentacao
?read_csv()

