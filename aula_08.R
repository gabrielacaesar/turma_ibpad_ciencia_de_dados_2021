library(lubridate)
library(tidyverse)

datas_brasil <- c("01/12/2019", "20/11/2018", "30011990", "17-03-2000")

dmy(datas_brasil)

?dmy

yq(c("202101", "20212", "202103"))

my("082017")

class(datas_brasil)

datas_brasil <- dmy(datas_brasil)
datas_brasil
class(datas_brasil)

datas_brasil_chr <- c("01/12/2019 13:51:15",
                      "20/11/2018 00:00:00",
                      "30011990 080000",
                      "17-03-2000 203000")

datas_brasil <- dmy_hms(datas_brasil_chr)

datas_brasil_chr
datas_brasil

class(datas_brasil_chr)
class(datas_brasil)

wday(datas_brasil_chr)
wday(datas_brasil)
?wday

wday(datas_brasil, label = TRUE, abbr = FALSE)

datas_brasil
day(datas_brasil)
month(datas_brasil)
year(datas_brasil)

week(datas_brasil)
?day

yday(datas_brasil)

hour(datas_brasil)
minute(datas_brasil)
second(datas_brasil)

datas_brasil

?ddays

datas_brasil + ddays(20)
datas_brasil - ddays(15)

datas_brasil + dweeks(3)

datas_brasil
datas_brasil + dhours(5)

data1 <- as_date("2021/01/15")
data2 <- as_date("2021/03/18")

data1
data2

data2 - data1

datatempo1 <- dmy_hms("01/09/1993 20:00:00")
datatempo2 <- dmy_hms("18-03-2021 19:46:15")

datatempo2 - datatempo1

datatempo3 <- dmy_hms("18-03-2021 18:00:00")
datatempo4 <- dmy_hms("18-03-2021 19:46:15")


datatempo4 - datatempo3

?difftime

difftime(data2, data1, units = "days")

difftime(datatempo4, datatempo3, units = "days")

class(difftime(datatempo4, datatempo3, units = "days"))


difftime(datatempo4, datatempo3, units = "days") %>% as.numeric()
# é equivalente a
as.numeric(difftime(datatempo4, datatempo3, units = "days"))

as.numeric(difftime(datatempo2, datatempo1, units = "days"))/365

# arredondar datas
datas_brasil
wday(datas_brasil)

?floor_date

floor_date(datas_brasil, "week")

floor_date(datas_brasil, "month")
floor_date(datas_brasil, "year")

floor_date(datas_brasil, "halfyear")

datas_brasil
ceiling_date(datas_brasil, "month") - ddays(1)


ceiling_date(datas_brasil, "year") - ddays(1)


df_vacinas <- data.frame(
  data = seq.Date(from = as.Date("2020-11-18"),
                  to = as.Date("2021-03-18"),
                  by = "1 day")
)

#?runif
set.seed(1)
df_vacinas$pessoas <- round(runif(121, 0, 1000))


df_vacinas %>%
  mutate(mes = floor_date(data, "month")) %>%
  group_by(mes) %>%
  summarise(total_pessoas = sum(pessoas),
            media_diaria = mean(pessoas))

write_csv

readr::write_csv2(df_vacinas, "vacinas por dia.csv")

library(writexl)

write_xlsx(df_vacinas, "vacinas por dia.xlsx")



#### API OKANEBOX ----
library(httr)
library(jsonlite)

acao <- "PETR4"
datainicial <- "20200312"
datafinal <- "20200830"

url <- "https://www.okanebox.com.br/api/acoes/hist/{acao}/{datainicial}/{datafinal}/"
url <- str_glue(url)
url

ambev <- GET(url)


content(ambev, as = "text") %>%
  fromJSON() %>%
  as_tibble() %>%
  mutate(DATPRG = str_sub(DATPRG, 1, 10))

baixar_dados_acao <- function(codigo,
                              data_inicial,
                              data_final){

  url <- "https://www.okanebox.com.br/api/acoes/hist/{codigo}/{data_inicial}/{data_final}/"
  url <- str_glue(url)
  resposta <- GET(url)

  content(resposta, as = "text") %>%
    fromJSON() %>%
    as_tibble() %>%
    mutate(DATPRG = str_sub(DATPRG, 1, 10))
}


baixar_dados_acao("PETR4", "20180425", "20191128")


url <- "https://www.okanebox.com.br/api/acoes/hist/ABEV3/20210101/20210318/"
ambev <- GET(url)

content(ambev, as = "text") %>%
  fromJSON() %>%
  as_tibble() %>%
  mutate(DATPRG = str_sub(DATPRG, 1, 10))

baixar_dados_acao("ABEV3", "20210101", "20210318")

library(rvest)

url <- "https://pt.wikipedia.org/wiki/Campeonato_Brasileiro_de_Futebol_de_2018_-_Série_A" %>%
  read_html()

url <- read_html("https://pt.wikipedia.org/wiki/Campeonato_Brasileiro_de_Futebol_de_2018_-_Série_A")

tb <- html_table(url)

tb[[6]]
class(tb)





