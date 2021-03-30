library(tidyverse)

df_feliz <- read_csv("df_feliz.csv")
df_2017 <- read_csv("df_2017.csv")


# relembrando histograma
df_2017 %>%
  ggplot(aes(x = life_expec)) +
  geom_histogram(color = "red",
                 binwidth =  5,
                 boundary = 0)
