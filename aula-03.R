library (readxl)
library (descr)
library (tidyverse)
library (frequency)


# adendo ------------------------------------------------------------------

freq(bd)

## problemas no banco de dados
## necessidade de corrigir

# preparando dados --------------------------------------------------------

bd <- read_excel("Banco de dados Final.xlsx")

## mudando nome das variáveis

a <- c()
for (i in 1:68) {
  d=paste("BD", i, sep = "")
  a=c(a,d)
}

dt <- bd

names(dt) <- a


## seleção de variáveis de interesse e conversão
dt$idade <- as.numeric(dt$BD4)
dt$rel <- as.factor(dt$BD6)
dt$raca <- as.factor(dt$BD11)
dt$hora_trabalho <- as.numeric(dt$BD17)
dt$renda <- as.numeric(dt$BD23)

## transformações
dt$renda_log <- log(dt$renda)
dt$raca[dt$raca == "NS/NR"] <- NA

# criação de dado único
a <- dt %>% 
  select(renda_log, hora_trabalho, raca, rel, idade) %>% 
  drop_na()

# regressão ---------------------------------------------------------------

# linear simples

reg01 <- lm(renda_log ~ hora_trabalho, data = a)
reg02 <- lm (renda_log ~ raca, data = a)


# linear múltipla

reg03 <- lm(renda_log ~ hora_trabalho + idade, data = a)
reg04 <- lm(renda_log ~ hora_trabalho + raca + idade , data = a)


# logística

# criação do y binário
dt <- dt %>% 
  mutate(indicacao = if_else(dt$BD43 == "Sim", 1, 0))

a <- dt %>% 
  select(renda_log, hora_trabalho, raca, rel, idade, indicacao) %>% 
  drop_na()

# regressao

reg05 <- lm (indicacao ~ hora_trabalho + raca + idade, data = a)


