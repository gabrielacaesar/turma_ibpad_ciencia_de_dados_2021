library(tidyverse)
library(readxl)
library(wpp2019)
library(countrycode) # install.packages("coutrycode")

# importar a planilha
df_feliz <- read_excel("dados/WHR2018Chapter2OnlineData.xls")
# limpar o nome das colunas
df_feliz <- janitor::clean_names(df_feliz)
# renomear algumas colunas
df_feliz <- df_feliz %>%
  rename(life_expec = healthy_life_expectancy_at_birth)


# consertar manualmente os nomes de certos paises

df_feliz$country[df_feliz$country == "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"
df_feliz$country[df_feliz$country == "Czech Republic"] <- "Czechia"
df_feliz$country[df_feliz$country == "Hong Kong S.A.R. of China"] <- "Hong Kong SAR China"
df_feliz$country[df_feliz$country == "Taiwan Province of China"] <- "Taiwan"


# consulte a documentação do dataset
# ?countrycode::codelist
df_continente <- countrycode::codelist  %>%
  # selecionar colunas importantes
  select(country = country.name.en, continent, country_code = iso3n) %>%
  # filtrar fora os paises sem continentes
  filter(!is.na(continent))

# identificar os paises que nao tem continente
left_join(df_feliz, df_continente) %>%
  distinct(country, continent) %>%
  filter(is.na(continent))


# criar dataframe com juncao dos dois
df_feliz <- inner_join(df_feliz, df_continente, by = "country")


# coletar dados de populacao a partir de outro dataset, do pacote wpp2019
data(pop)
df_populacao <- pop %>%
  select(country_code, `2020`) %>%
  rename(populacao_2020 = 2)

df_feliz <- df_feliz %>%
  left_join(df_populacao, by = 'country_code')

# criar dataset apenas para o ano mais recente
df_2017 <- df_feliz %>%
  filter(year == max(year))

glimpse(df_feliz)
df_2017

ggplot(df_2017, aes(x = log_gdp_per_capita,
                    y = life_expec,
                    color = continent)) +
  geom_point()

df_2017 %>% filter(is.na(log_gdp_per_capita))


ggplot(df_2017) +
  geom_point(aes(x = log_gdp_per_capita,
                 y = life_expec),
             color = "blue",
             size = 0.5)


ggplot(df_2017) +
  geom_point(aes(x = log_gdp_per_capita,
                 y = life_expec,
                 size = populacao_2020),
             color = "blue")



ggplot(df_2017) +
  geom_point(aes(x = log_gdp_per_capita,
                 y = life_expec,
                 size = populacao_2020,
                 color = continent),
             alpha = 0.3)


ggplot(df_2017) +
  geom_point(aes(x = log_gdp_per_capita,
                 y = life_expec,
                 color = "yellow"))

df_2017 %>% write_csv("df_2017.csv")
df_feliz %>% write_csv("df_feliz.csv")

df_feliz <- read_csv("df_feliz.csv")
df_2017 <- read_csv("df_2017.csv")

#ggplt(df_2017)# ...

df_2017 %>%
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec,
             color = continent)) +
  geom_point() +
  geom_smooth(method = "lm")


df_2017 %>%
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec)) +
  geom_point(aes(color = continent)) +
  geom_smooth(method = "lm")


df_2017 %>%
  ggplot() +
  geom_point(aes(x = log_gdp_per_capita,
                 y = life_expec,
                 color = continent)) +
  geom_smooth(method = "lm")

df_2017 %>%
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec)) +
  geom_smooth(method = "lm") +
  geom_point()


ggplot(df_2017,
       aes(x =  log_gdp_per_capita,
           y = life_expec)) +
  geom_col()


df_2017 %>%
  group_by(continent) %>%
  summarise(expec_media = mean(life_expec)) %>%
  ggplot(aes(y = continent,
             x = expec_media)) +
  geom_col(color = "red",
           fill = "blue",
           alpha = 0.5)

df_feliz %>%
  filter(year > 2005) %>%
  group_by(year) %>%
  summarise(expec_media = mean(life_expec),
            qtd_paises = n()) %>%
  ggplot(aes(x = year,
             y = expec_media))  +
  geom_col()



df_feliz %>%
  filter(year == 2007 | year == 2017) %>%
  mutate(year = as.character(year)) %>%
  group_by(continent, year) %>%
  summarise(expec_vida_media = mean(life_expec)) %>%
  ggplot(aes(x = continent,
             y = expec_vida_media,
             fill = year)) +
  geom_col(position = "dodge")

df_2017 %>%
  group_by(continent) %>%
  summarise(expec_media = mean(life_expec),
            pop_total = sum(populacao_2020)) %>%
  ggplot(aes(x = fct_reorder(continent, pop_total, .desc = TRUE),
             y = pop_total)) +
  geom_col()



df_2017 %>%
  group_by(continent) %>%
  summarise(expec_media = mean(life_expec),
            pop_total = sum(populacao_2020)) %>%
  ggplot(aes(x = fct_reorder(continent, pop_total, .desc = TRUE),
             y = expec_media)) +
  geom_col()

lista_datasets <- rbcb::get_series(code = c(ipca = 433, selic = 4390))
lista_datasets$ipca
lista_datasets$selic

df_st <- left_join(lista_datasets$ipca, lista_datasets$selic, by = "date")

library(lubridate)
df_st %>%
  #filter(year(date) >= 2000) %>%
  filter(date >= as.Date("2000-01-01")) %>%
  ggplot(aes(x = date,
             y = ipca)) +
  geom_line() +
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y-%m")

df_st %>%
  #filter(year(date) >= 2000) %>%
  filter(date >= as.Date("2020-02-01")) %>%
  ggplot(aes(x = date,
             y = ipca)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m-%y")

df_st %>%
  #filter(year(date) >= 2000) %>%
  filter(date >= as.Date("2000-01-01")) %>%
  ggplot(aes(x = date,
             y = ipca)) +
  # curva do ipca
  geom_line(color = "red") +
  # curva da selic
  geom_line(aes(y = selic)) +
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y-%m")

usd <- rbcb::get_series(code = c(dolar = 1),
                        start_date = as.Date("1999-01-01"))

library(lubridate)
usd %>%
  filter(day(date) == 1)

usd_mes <- usd %>%
  mutate(inicio_mes = floor_date(date, "month")) %>%
  group_by(inicio_mes) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  mutate(dolar_anterior = lag(dolar, n = 1),
         variacao = dolar/dolar_anterior - 1) %>%
  select(inicio_mes, variacao)

tail(usd_mes)

df_st <- inner_join(df_st, usd_mes,by = c("date" = "inicio_mes"))

df_st %>%
  filter(date >= as.Date("2019-01-01")) %>%
  pivot_longer(cols = c(ipca, selic, variacao),
               names_to = "nome_indicador",
               values_to = "valor") %>%
  ggplot(aes(x = date, y = valor, color = nome_indicador)) +
  geom_line()


mean(df_2017$life_expec)

df_2017 %>%
  ggplot(aes(x = life_expec)) +
  geom_histogram(color = "red",
                 binwidth =  5,
                 boundary = 0)




