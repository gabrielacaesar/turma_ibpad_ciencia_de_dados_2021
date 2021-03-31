library(tidyverse)

df_feliz <- read_csv("df_feliz.csv")
df_2017 <- read_csv("df_2017.csv")


# relembrando histograma
df_2017 %>%
  ggplot(aes(x = life_expec)) +
  geom_histogram(color = "red",
                 binwidth =  5)


df_2017 %>%
  filter(continent == "Americas") %>%
  ggplot(aes(x = populacao_2020/1000)) +
  geom_histogram(binwidth = 10, boundary = 0)


df_2017 %>%
  select(country, populacao_2020) %>%
  filter(country == "Brazil")

df_2017 %>%
  ggplot(aes(x = life_expec, fill = continent)) +
  geom_histogram(binwidth =  5,
                 boundary = 0)

# boxplot
df_2017 %>%
  ggplot(aes(x = continent, y = life_expec)) +
  geom_boxplot()


df_2017 %>%
  ggplot(aes(y = continent, x = life_expec)) +
  geom_boxplot()

df_2017 %>%
  ggplot(aes(y = continent, x = life_expec)) +
  geom_violin()

df_2017 %>%
  arrange(desc(life_expec)) %>%
  head(10) %>%
  ggplot(aes(y = fct_reorder(country, life_expec),
             x = life_expec)) +
  geom_col() +
  geom_text(aes(label = round(life_expec, 1)),
            hjust = 1.2,
            color = "white")


df_2017 %>%
  arrange(desc(life_expec)) %>%
  head(10) %>%
  ggplot(aes(y = fct_reorder(country, life_expec),
             x = life_expec)) +
  geom_col() +
  geom_text(aes(label = round(populacao_2020/1000, 1)),
            hjust = 1.2,
            color = "white")



df_2017 %>%
  arrange(desc(life_expec)) %>%
  head(10) %>%
  ggplot(aes(y = fct_reorder(country, life_expec),
             x = life_expec,
             fill = continent)) +
  geom_col() +
  geom_text(aes(label = round(populacao_2020/1000, 1)),
            hjust = 1.2,
            color = "black")




df_2017 %>%
  arrange(desc(life_expec)) %>%
  head(10) %>%
  ggplot(aes(y = fct_reorder(country, life_expec),
             x = life_expec)) +
  geom_col() +
  geom_label(aes(label = round(life_expec, 1)),
            hjust = 1.2)



df_brazil <- df_2017 %>% filter(country == "Brazil")
df_brazil

df_2017 %>%
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec)) +
  geom_point() +
  geom_text(aes(label = country))


ggplot(data = df_2017,
       aes(x = log_gdp_per_capita,
           y = life_expec)) +
  geom_point() +
  geom_text(data = df_brazil,
                  aes(label = country))



ggplot(data = df_2017,
       aes(x = log_gdp_per_capita,
           y = life_expec)) +
  geom_point() +
  geom_point(data = df_brazil, color = "red", size  = 4) +
  geom_text(data = df_brazil, aes(label = country)) +
  geom_text(x = 7.5, y = 75, label = "Ponto do Brasil\n em vermelho")


df_2017 %>%
  filter(continent == "Americas") %>%
  ggplot(aes(y = country, x = life_expec)) +
  geom_point()

df_feliz %>%
  filter(continent == "Americas", year %in% c(2005, 2007)) %>%
  ggplot(aes(x = life_expec,
             y = country,
             color = year)) +
  geom_point()

library(gapminder)
gapminder

library(ggthemes)

gapminder %>%
  filter(continent == "Americas", year %in% c(1987, 2007)) %>%
  ggplot(aes(x = lifeExp,
             y = fct_reorder(country, lifeExp, .fun = max),
             color = as.character(year))) +
  geom_point() +
  geom_line(aes(group = country), color = "grey") +
  labs(x = "Expectativa de vida",
       y = NULL,
       color = "Ano",
       title = "Evolução da expec. de vida dos países americanos",
       subtitle = "Gráfico mostra a desigualdade nos países") +
  geom_vline(xintercept = 72.4, linetype = "dashed") +
  theme_hc()



# install.packages("ggThemeAssist")

gapminder %>%
  filter(country == "Brazil", year == 2007)


#### facets
ggplot(data = df_2017,
       aes(x = log_gdp_per_capita,
           y = life_expec)) +
  geom_point(aes(color = continent)) +
  facet_wrap(vars(continent), scales = "free_x")

gapminder %>%
  filter(year %in% c(1987, 2007)) %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point() +
  facet_grid(rows = vars(year), cols = vars(continent))
ggsave(filename = "meu grafico.png", width = 12, height = 7)


#### graficos interativos
ggplot(data = df_2017,
       aes(x = log_gdp_per_capita,
           y = life_expec)) +
  geom_point(aes(color = continent,
                 text = str_c("País: ", country)))

#library(plotly)
ggplotly(tooltip = "text")

# y = f(x)
# aluguel = f(salario)







