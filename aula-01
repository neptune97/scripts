library (readxl)
library (descr)
library (tidyverse)


## abrir banco de dados
## use o botão Import Dataset e selecione o formato, ou siga as linhas de comando abaixo

bd <- read_excel("Banco de dados Final.xlsx")

## mudando nome das variáveis
################################ loop
a <- c()
for (i in 1:68) {
  d=paste("BD", i, sep = "")
  a=c(a,d)
}

dt <- bd

names(dt) <- a

################################ tidyverse

dt2 <- bd %>% 
  rename(Local = Município, Sexo = `1 - Anote o sexo:`, Idade = `2 - Qual a sua idade? (ESPONTÂNEA ? ANOTE EXATAMENTE O QUE O ENTREVISTADO RESPONDER)`)

## checando classes

class(dt$BD17)

## alterando classes

dt$BD17 <- as.numeric(dt$BD17)
dt$BD17 <- as.factor(dt$BD17)

## corrigindo "-"
dt[dt == "-"] <- NA
dt$BD17 <- as.numeric(dt$BD17)

## o que fazer com NS/NR??

## freqs (1x1)
freq(dt$BD3)
freq(dt$BD6)

## freq (2x2)
dt %>% 
  group_by(BD3) %>% 
  count(BD6)

################################################ limpeza do banco

dt$BD7

a <- dt %>% 
  separate(BD7, sep = "/", into = c("Igreja", "Endereço"))

################################################ filtros

b <- dt %>% 
  filter(BD2 == "Belo Horizonte") %>% 
  count (BD6)

