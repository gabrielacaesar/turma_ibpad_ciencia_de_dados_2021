#### aula 08 ----
library(tidyverse)

?runif
set.seed(123)

df_idades <- data.frame(
  idade = round(runif(20, 18, 100))
)


faixa_etaria <- function(a) {
  if(a <= 29) {
    return("0 a 29")
  } else if(a >= 30 & a <= 59) {
    return("30 a 59")
  } else if(a >= 60 & a <= 89) {
    return("60 a 89")
  } else if(a >= 90){
    return("90 ou mais")
  }
}


df_idades %>%
  mutate(faixa_idade = faixa_etaria(idade))

faixa_etaria(42)
faixa_etaria(5)
faixa_etaria(c(5, 42))

map(c(5, 42), faixa_etaria)

df_idades %>%
  mutate(faixa_idade = map_chr(idade, faixa_etaria))

### cut()
cut

df_idades$idade
cut(df_idades$idade,
    breaks = c(0, 29, 59, 89,  Inf),
    labels = c("0 a 29", "30 a 59", "60 a 89", "90 ou mais"),
    include.lowest = TRUE)

df_idades %>%
  mutate(faixa_idade = cut(idade,
                           breaks = c(0, 29, 59, 89,  Inf),
                           labels = c("0 a 29", "30 a 59", "60 a 89", "90 ou mais"),
                           include.lowest = TRUE))

#  ( == intervalo aberto
#  [ == intervalo fechadi

# (29, 59] == x > 29 & x <= 59 (include.lowest = FALSE)
# [29, 59] (include.lowest = TRUE)

#### capitulo 07
telefones <- c('9931-9572', '8591-5772', '8562-1923')

# contar ocorrencias de um caracter
str_count(telefones, "7")

str_count(telefones, "0")
str_count(telefones, "9")

# o telefone possui o algarismo 7?
str_count(telefones, "7") > 0

str_detect(telefones, "7")

str_detect(telefones, "9")

str_starts(telefones, "9")

str_ends(telefones, "3")


cpfs <- as.character(c(1234, 01833827570, 45614814570, 4, 4000001111))

cpfs

?str_pad

str_length(cpfs)
cpfs[2] <- str_pad(cpfs[2], width = 11, side = "left", pad = "0")
cpfs

x <- c("      inicio",
       "final      ",
       "      ambos      ",
       "    no       meio        ")

x
?str_trim
str_trim(x)
str_squish(x)

str_remove_all(x, " ")

cnpj <- c('19.702.231/9999-98',
          '19.498.482/9999-05',
          '19.499.583/9999-50',
          '19.500.999/9999-46',
          '19.501.139/9999-90')

str_remove_all(cnpj, ".")
?str_remove_all

str_replace_all(cnpj, ".", "a")

str_remove_all(cnpj, "\\.")


str_remove_all(cnpj, "/") %>%
  str_remove_all("-") %>%
  str_remove_all("\\.")

str_remove_all(cnpj, "/|-|\\.")

str_extract_all(cnpj, "\\d")

textos <- c("Fulano", "fulano", "abcdeF", "01584",
            "abc456", "123def", "OI", "meuemail@gmail.com",
            "www.google.com", "Meu nome é Fulano")
textos

str_subset(textos, "a")
str_subset(textos, "i")

str_subset(textos, "F")

textos[str_starts(textos, "F")]

str_subset(textos, "^F")

str_subset(textos, "[0-9]")

str_subset(textos, "^[0-9]")

str_subset(textos, "[0-9]$")


cadastros <- data.frame(
  email = c('joaodasilva@gmail.com', 'rafael@hotmail.com', 'maria@uol.com.br', 'juliana.morais@outlook.com'),
  telefone = c('(61)99831-9482', '32 8976 2913', '62-9661-1234', '15-40192.5812')
)


cadastros %>%
  separate(col = email,
           into = c("login", "dominio"),
           sep = "@") %>%
  mutate(dominio = word(dominio, sep = "\\.")) %>%
  separate(col = telefone,
           into = c("ddd", "telefone"),
           sep = -10) %>%
  mutate(ddd = str_remove_all(ddd, "\\(|\\)|-"),
         telefone = str_replace_all(telefone, "\\s|\\.", "-")) %>%
  mutate(telefone = ifelse(str_starts(telefone, "-"),
                           str_remove(telefone, "-"),
                           telefone
                           ))




cadastros %>%
  separate(col = email,
           into = c("login", "dominio"),
           sep = "@") %>%
  mutate(dominio = word(dominio, sep = "\\.")) %>%
  mutate(telefone = str_remove_all(telefone, "\\(|\\)|-"))




word("Meu nome é Sillas")


remotes::install_github("sillasgonzaga/literaturaBR")
library(literaturaBR)
df_livros <- literaturaBR::load_all_books()
head(df_livros)
glimpse(df_livros)

unique(df_livros$book_name)

df_livros <- as_tibble(df_livros)
df_livros

df_livros %>%
  separate_rows(text, sep = " ")



str_subset(textos, "[A-Z]|[a-z]")


df_livros_sep <- df_livros %>%
  separate_rows(text, sep = " ") %>%
  filter(str_detect(text, "[A-Z]|[a-z]")) %>%
  mutate(text = str_to_lower(text))

df_livros_sep %>%
  group_by(book_name) %>%
  summarise(qtd_total = n(),
            qtd_palavras_distintas = n_distinct(text)) %>%
  mutate(taxa = qtd_palavras_distintas / qtd_total) %>%
  arrange(desc(taxa))

#### capitulo 08 ----

x <- c("2014-07-15", "2018/03/20", "2019-12-31", "20170511")

class(x)
x

as.Date(x)

library(lubridate)

as_date(x)
class(as_date(x))

### gerar sequencia de datas

?seq.Date

seq.Date(from = as_date("2021-01-01"),
         to = as_date("2021-12-31"),
         length.out = 366)

seq.Date(from = as_date("2021-01-01"),
         to = as_date("2021-12-31"),
         by = "1 month")

seq.Date(from = as_date("2021-01-01"),
         to = as_date("2021-12-31"),
         by = "30 days")

seq.Date(from = as_date("2021-01-01"),
         length.out = 6,
         by = "1 week")


wday(today())

datas_niver <- seq.Date(from = as_date("1993-09-01"),
                        by = "1 year",
                        length.out = 28)

wday(datas_niver) %>% table()
table(wday(datas_niver))


as_date("01/09/1993")

class(dmy("01/09/1993"))

datas_brasil <- c("01/12/2019", "20/11/2018", "30011990", "17-03-2000")

dmy(datas_brasil)


class(dmy_hms("30-09-2019 14:51:39"))
