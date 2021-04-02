library(tidyverse)

herois <- read_csv("herois_completo.csv")

herois <- herois %>%
  mutate(height = height / 100,
         weight = weight * 0.45)

herois %>%
  ggplot(aes(x = height,
             y = weight)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted")

herois <- herois %>%
  filter(height > 0 & weight > 0)

herois %>%
  select(name, weight, height) %>%
  arrange(weight)

# 2
?scale_x_continuous

herois %>%
  filter(height <= 3) %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 0.05, boundary = 0) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 10))


seq(from = 0, to = 3, by = 0.20)



herois %>%
  filter(height <= 3) %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 0.05, boundary = 0) +
  scale_x_continuous(breaks = seq(0, 3, 0.20))


# 3
herois %>%
  ggplot(aes(x = publisher, y = weight)) +
  geom_boxplot()

IQR(herois$weight)
quantile(herois$weight)

mediana <- median(herois$weight)
q3 <- quantile(herois$weight, .75)
limite_max_iqr <- q3 + 1.5 * IQR(herois$weight)
limite_max_iqr

herois %>%
  ggplot(aes(x = weight)) +
  geom_histogram() +
  geom_vline(xintercept = c(mediana, q3, limite_max_iqr),
             linetype = "dashed")



herois %>%
  ggplot(aes(x = log(weight))) +
  geom_histogram()

# 4
herois %>%
  group_by(publisher) %>%
  summarise(qtd_personagens = n())

herois %>%
  count(publisher, name = "qtd") %>%
  ggplot(aes(x = fct_reorder(publisher, qtd, .desc = TRUE),
             y = qtd)) +
  geom_col() +
  geom_text(aes(label = qtd), vjust = c(1.5, 1.5, -0.1))

# 5
herois %>%
  count(publisher, alignment, name = "qtd") %>%
  filter(!is.na(alignment)) %>%
  ggplot(aes(x = publisher, y = qtd, fill = alignment)) +
  geom_col(position = "dodge")

# 6
herois %>%
  count(publisher, alignment, name = "qtd") %>%
  filter(!is.na(alignment)) %>%
  ggplot(aes(x = publisher, y = qtd, fill = alignment)) +
  geom_col(position = "fill")

herois %>%
  count(publisher, alignment, name = "qtd") %>%
  filter(!is.na(alignment)) %>%
  group_by(publisher) %>%
  mutate(qtd_total = sum(qtd),
         prop = qtd/qtd_total) %>%
  ggplot(aes(x = publisher, y = prop, fill = alignment)) +
  geom_col(position = "dodge")

glimpse(herois)

# 7
hero_agg <- herois %>%
  pivot_longer(cols = agility:omniscient,
               names_to = "nome_poder",
               values_to = "tem_poder") %>%
  group_by(publisher, name) %>%
  summarise(qtd_poderes = sum(tem_poder))


# 8
hero_agg %>%
  group_by(publisher) %>%
  slice_max(order_by = qtd_poderes, n = 5) %>%
  ggplot(aes(y = name, x = qtd_poderes, fill = publisher)) +
  geom_col() +
  facet_wrap(vars(publisher), scales = "free_y") +
  theme(legend.position = "bottom")

hero_agg %>%
  ggplot(aes(x = qtd_poderes, fill = publisher)) +
  geom_density(alpha = 0.3)





