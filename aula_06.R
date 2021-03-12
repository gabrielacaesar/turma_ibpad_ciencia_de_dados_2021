# install.packages("janitor")

# questao 0
library(tidyverse)
library(janitor)

# questao 1
hero_powers <- readr::read_csv("dados/archive/super_hero_powers.csv", na = c("", "-", "NA"))

hero_info <- readr::read_csv("dados/archive/heroes_information.csv", na = c("", "-", "NA"))

# ?read_csv()
glimpse(hero_powers)
glimpse(hero_info)

# questao 2
hero_powers <- hero_powers %>%
  clean_names()

hero_info <- hero_info %>%
  clean_names()

# alternativa: hero_info <- clean_names(hero_info)
# alternativa: hero_powers <- clean_names(hero_powers)

# questao 3
hero_info <- hero_info %>%
  select(-x1)

# questao 4
glimpse(hero_powers)

hero_powers %>%
  mutate(agility = as.numeric(agility))

hero_powers %>% 
  mutate_at(vars(-hero_names), as.numeric)

# questao 5 

hero_info %>%
  distinct(publisher)

unique(hero_info$publisher)

hero_info %>%
  select(publisher) %>%
  mutate(publisher = ifelse(publisher == "Marvel Comics", 
                            "Marvel",
                     ifelse(publisher == "DC Comics",
                            "DC",
                            "Outros")))

hero_info %>%
  select(publisher) %>%
  mutate(publisher = case_when(
    publisher == "Marvel Comics" ~ "Marvel",
    publisher == "DC Comics" ~ "DC",
    TRUE ~ "Outros"))

 ?case_when()

# questao 6 

hero_info %>%
  select(publisher, race) %>%
  filter(!is.na(race)) %>%
  distinct() %>%
  count(race) %>%
  arrange(n)

# questao 7 

hero_info %>%
  select(eye_color, gender) %>%
  na.omit() %>%
  count(gender, eye_color) %>%
  #arrange(desc(n)) %>%
  group_by(gender) %>%
  slice_max(n = 3, order_by = n)

# questao 8 

hero_powers %>%
  summarise_if(is.logical, mean)

# questao 9 

hero_powers %>%
  pivot_longer(cols = -hero_names,
               names_to = "poder",
               values_to = "possui_poder") %>%
  group_by(poder) %>%
  summarise(media_poder = mean(possui_poder))
  
# questao 10 

hero <- hero_info %>%
  inner_join(hero_powers, by = c("name" = "hero_names"))

# hero <- inner_join(hero_info, hero_powers, by = c("name" = "hero_names"))

# questao 11

hero %>%
  select(publisher, telepathy) %>%
  group_by(publisher) %>%
  summarise(perc = mean(telepathy),
            qtd = n()) %>%
  slice_head(n = 20)

# ?slice_head()

# questao 12
hero %>%
  select(name, publisher, flight, weight) %>%
  filter(flight == TRUE) %>%
  slice_max(n = 10, order_by = weight)

# questao 13

# getwd() 
# dir.create()
# setwd()
# Sys.Date()

readr::write_csv(hero, "herois_completo.csv")

################

# capitulo 7 / parte 2

nome <- c("joao", "jose", "maria")
sobrenome <- c("souza", "silva", "pereira")

paste(nome, sobrenome)

paste(nome, sobrenome, sep = "-")
?paste()

paste0("www.minhaagenda.com.br/", 1:31, "/1/2021")

paste0("www.dadosinteressantes.com.br/page=", 1:100)

dir.create(paste0("pasta_da_gabriela", Sys.Date()))

library(stringr)

cnae.texto <- c('10 Fabricação de produtos alimentícios', '11 Fabricação de bebidas',
                '12 Fabricação de produtos do fumo', '13 Fabricação de produtos têxteis',
                '14 Confecção de artigos do vestuário e acessórios',
                '15 Preparação de couros e fabricação de artefatos de couro, artigos para viagem e calçados',
                '16 Fabricação de produtos de madeira',
                '17 Fabricação de celulose, papel e produtos de papel')


cnae <- str_sub(cnae.texto, 0, 2)
?str_sub()

texto <- str_sub(cnae.texto, start = 4, end = -1)

texto
cnae

celulares <- c("11 2244-5566", "11 3344-5566", "11 4455-6677")
str_sub(celulares, start = -4, end = -3)
str_sub(celulares, start = 0, end = 2)

telefones <- c('21-9931-9572-', '8591-5772-', '8562-1923')
str_replace(telefones, '-', ' ')

str_replace_all(telefones, '-', ' ')

str_remove_all(telefones, "-")

cnpj <- c('19.702.231/9999-98', '19.498.482/9999-05', '19.499.583/9999-50', '19.500.999/9999-46', '19.501.139/9999-90')
str_replace_all(cnpj, "\\.", " ")


meu_nome <- c("Mariana", "Maria", "Mariangela", "Maribela")

x <- c("Prazer, sou a {meu_nome}")
print(x)

str_glue(x)
