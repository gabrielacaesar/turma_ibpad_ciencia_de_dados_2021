# aula 04
# professora gabriela

library(tidyverse)

df <- readr::read_csv2("dados/Ano-2020.csv")

# 1) Quais foram os deputados com mais despesas na cota parlamentar (considerando a coluna vlrDocumento)?

resposta_1 <- df %>%
  mutate(vlrDocumento = as.numeric(vlrDocumento)) %>%
  group_by(txNomeParlamentar, sgPartido) %>%
  summarise(soma_vlr = sum(vlrDocumento)) %>%
  arrange(desc(soma_vlr)) %>%
  head(5)
  
# 2) Quais foram as 5 empresas mais contratadas (considerando a coluna textCNPJCPF)?
  
resposta_2 <- df %>%
  mutate(vlrDocumento = as.numeric(vlrDocumento)) %>%
  group_by(txtCNPJCPF) %>%
  summarise(soma_vlr = sum(vlrDocumento)) %>%
  arrange(desc(soma_vlr)) %>%
  head(5)

resposta_2_2 <- df %>%
  filter(txtCNPJCPF %in% c("075.756.510/0015-9",
                           "020.128.620/0016-0",
                           "073.193.230/0019-1",
                           "025.581.570/0016-2",
                           "092.962.950/0016-0"))

empresa <- df %>%
  filter(txtCNPJCPF == "075.756.510/0015-9")

# 3) Qual foi o gasto médio dos deputados, por UF, com a cota parlamentar (considerando a coluna vlrDocumento)?
  
qt_deputados <- df %>%
  filter(!str_detect(txNomeParlamentar, "LIDERANÇA|LIDMIN")) %>%
  distinct(txNomeParlamentar, sgUF) %>%
  group_by(sgUF) %>%
  summarise(qt = n()) %>%
  arrange(desc(qt))
  

resposta_3 <- df %>%
  mutate(vlrDocumento = as.numeric(vlrDocumento)) %>%
  group_by(sgUF) %>%
  summarise(soma_vlr = sum(vlrDocumento),
            media_vlr = mean(vlrDocumento)) %>%
  left_join(qt_deputados, by = "sgUF") %>%
  mutate(media_gasto = soma_vlr / qt) %>%
  arrange(desc(media_gasto))

# 4) Quais categorias de gastos registraram mais despesas nas lideranças (considerando a coluna txtDescricao)?
  
resposta_4 <- df %>%
  mutate(vlrDocumento = as.numeric(vlrDocumento),
         nome_minusculo = tolower(txNomeParlamentar)) %>%
  filter(str_detect(txNomeParlamentar, "LIDERANÇA|LIDMIN")) %>%
  group_by(txtDescricao) %>%
  summarise(soma_vlr = sum(vlrDocumento)) %>%
  arrange(desc(soma_vlr))

# 5) Quantas linhas da coluna com o PDF da nota fiscal estão com NA (considerando a coluna urlDocumento)?
  
resposta_5 <- df %>%
  filter(is.na(urlDocumento)) %>%
  nrow()

# 6) Qual foi o mês com menos gastos (considerando a coluna numMes)?
  
resposta_6 <- df %>%
  mutate(vlrDocumento = as.numeric(vlrDocumento)) %>%
  group_by(numMes) %>%
  summarise(soma_nota = sum(vlrDocumento)) %>%
  arrange(soma_nota)

#############################################

# parte 2 / airbnb

# questao 1
df_anuncios <- readr::read_csv("dados/listings.csv.gz")

# questao 2
glimpse(df_anuncios)
summary(df_anuncios)

# questao 3
glimpse(df_anuncios)
class(df_anuncios$price)
class(df_anuncios$host_has_profile_pic)
class(df_anuncios$name)

# questao 4
class(df_anuncios$price)
class(df_anuncios$weekly_price)
class(df_anuncios$monthly_price)
head(df_anuncios$price)
sum(df_anuncios$price)

# questao 5 
df_anuncios <- df_anuncios %>%
  mutate(price = parse_number(price))

head(df_anuncios$price)
class(df_anuncios$price)

resposta_5 <- df_anuncios %>%
  select(listing_url, neighbourhood, price) %>%
  arrange(desc(price))

# questao 6 
resposta_6 <- df_anuncios %>%
  distinct(host_name, host_listings_count) %>%
  arrange(desc(host_listings_count))

resposta_6_2 <- df_anuncios %>%
  group_by(host_id, host_name) %>%
  summarise(quantidade = n()) %>%
  arrange(desc(quantidade))
  
# questao 7 
library(lubridate)

resposta_7 <- df_anuncios %>%
  distinct(host_name, host_since) %>%
  mutate(ano = year(host_since)) %>%
  count(ano)
  
# questao 8 
resposta_8 <- df_anuncios %>%
  select(name, space) %>%
  mutate(space = toupper(space)) %>%
  filter(str_detect(space, "PRAIA|BEACH|STRAND|PLAYA"))

# questao 9 
resposta_9 <- df_anuncios %>%
  select(space, price) %>%
  mutate(tem_praia = str_detect(space, "praia")) %>%
  group_by(tem_praia) %>%
  summarise(media_price = mean(price))

# questao 10 
resposta_10 <- df_anuncios %>%
  select(availability_30) %>%
  mutate(esgotado = availability_30 == 0)

# questao 11
resposta_11 <- df_anuncios %>%
  mutate(esgotado = availability_30 == 0) %>%
  filter(esgotado == TRUE) %>%
  group_by(neighbourhood, esgotado) %>%
  summarise(qt_anuncio = n(),
            tx_esgotado = mean(esgotado)) %>%
  arrange(desc(tx_esgotado)) %>%
  head(5)

# questao 12
resposta_12 <- df_anuncios %>%
  group_by(neighbourhood) %>%
  summarise(qt_anuncio = n(),
            qt_review = sum(number_of_reviews),
            tx_review_por_bairro = qt_review / qt_anuncio) %>%
  arrange(desc(tx_review_por_bairro))
  
# dever de casa: questoes 13, 14 e 15


  