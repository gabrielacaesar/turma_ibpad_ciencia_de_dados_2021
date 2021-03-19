# aula 05
# professora gabriela

# resolver exercicios 13, 14 e 15 do airbnb
library(tidyverse)

df_anuncios <- readr::read_csv("dados/listings.csv.gz")

# questao 13
df_anuncios %>%
  distinct(room_type)

?distinct()

unique(df_anuncios$room_type)

# questao 14
# A quantidade de quartos tem relação com o 
# preço dos apartamentos inteiros?

df_anuncios %>%
  select(room_type, bedrooms, price) %>%
  mutate(price = parse_number(price)) %>%
  filter(room_type == "Entire home/apt") %>%
  group_by(bedrooms) %>%
  summarise(preco_medio = mean(price),
            preco_mediano = median(price),
            qtd_anuncios = n())

df_anuncios <- df_anuncios %>%
  mutate_at(vars(weekly_price, monthly_price, price, 
                 cleaning_fee, extra_people, 
                 security_deposit), 
            parse_number)

# questao 15 

df_anuncios %>% 
  select(listing_url, neighbourhood, host_neighbourhood,
         availability_30, minimum_nights, 
         security_deposit, instant_bookable,
         guests_included, price, room_type, number_of_reviews,
         cleaning_fee, review_scores_rating) %>%
  filter(
    neighbourhood %in% c("Leblon", "Copacabana", "Ipanema"),
    host_neighbourhood == neighbourhood,
    security_deposit == 0,
    instant_bookable,
    room_type == "Entire home/apt",
    guests_included >= 2
  ) %>%
  mutate(preco_total_viagem = cleaning_fee + price * 5) %>%
  select(listing_url, preco_total_viagem) %>%
  arrange(preco_total_viagem)

#########

table1
table2
table3
table4a

relig_income %>%
  pivot_longer(2:11,
               names_to = "income",
               values_to = "n_respondees")

relig_income %>%
  pivot_longer(-religion,
               names_to = "income",
               values_to = "n_respondees")

relig_income %>%
  pivot_longer(`<$10k`:`Don't know/refused`,
               names_to = "income",
               values_to = "n_respondees")

# ?pivot_longer()
# ?relig_income
?us_rent_income

us_rent_income %>%
  pivot_wider(names_from = "variable",
              values_from = c("estimate", "moe"))
  


table3 %>%
  separate(rate, into = c("casos", "populacao"), sep = "/")

?separate()
?unite()

table5 %>%
  unite(ano, c(century, year), sep = "", remove = FALSE)


# criar dataframe de exemplo
exemplo <- tibble(grupo = c("a", "a", "b","b"),
                  y = c("1, 2", "3;4", "1,2,3", "4"))

exemplo %>% 
  separate_rows(y, sep = ",")

## exercicio 5.3

# Transforme a table1 para a table2 usando pivot_longer()
table1 %>%
  pivot_longer(cols = c(cases, population),
               names_to = "type",
               values_to = "count")

?pivot_longer()
# Transforme a table2 para a table1 usando pivot_wider()
table2 %>%
  pivot_wider(names_from = "type",
              values_from = "count")

# Transforme a table5 para a table1 e para a table2

table5 %>%
  unite("year", c("century", "year"), sep = "") %>%
  separate(rate, c("cases", "population"), sep = "/")

table5 %>%
  unite("year", c("century", "year"), sep = "") %>%
  separate(rate, c("cases", "population"), sep = "/") %>%
  pivot_longer(cols = c(cases, population),
               names_to = "type",
               values_to = "count")

?pivot_longer()

###### capitulo 6

dados2016 <- data.frame(ano = c(2016, 2016, 2016), 
                        valor = c(938, 113, 1748), 
                        produto = c('A', 'B', 'C'))

dados2017 <- data.frame(valor = c(8400, 837, 10983), 
                        produto = c('H', 'Z', 'X'),
                        ano = c(2017, 2017, 2017))

dados.finais <- bind_rows(dados2016, dados2017)

dados.finais

### cruzamento

band_members
band_instruments

band_members %>%
  inner_join(band_instruments)

inner_join(band_members, band_instruments, by = "name")

band_instruments2

inner_join(band_members, 
           band_instruments2, 
           by = c("name" = "artist"))

?inner_join()

left_join(band_members,
          band_instruments,
          by = "name")

right_join(band_members,
           band_instruments,
           by = "name")

full_join(band_members,
          band_instruments,
          by = "name")

##############
install.packages("nycflights13")
library(nycflights13)

df_airports <- airports %>%
  select(faa, name)

flights %>%
  group_by(origin, dest) %>%
  summarise(qtd_voos = n()) %>%
  left_join(df_airports, by = c("origin" = "faa")) %>%
  left_join(df_airports, by = c("dest" = "faa")) %>%
  rename(origem = name.x,
         destino = name.y) %>%
  select(origem, destino, qtd_voos) %>%
  arrange(desc(qtd_voos))
  
flights %>%
  count(origin, dest, sort = TRUE, name = "qtd_voos") %>%
  left_join(df_airports, by = c("origin" = "faa")) %>%
  left_join(df_airports, by = c("dest" = "faa")) %>%
  rename(origem = name.x,
         destino = name.y) %>%
  select(origem, destino, qtd_voos) 


# dever de casa - kaggle sobre super herois (6.4 do livro)



