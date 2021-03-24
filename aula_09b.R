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
                    y = life_expec)) +
  geom_point()








