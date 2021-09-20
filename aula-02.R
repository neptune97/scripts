library(tidyverse)

# barras ------------------------------------------------------------------

## ajustando dados

b <- dt %>% 
  group_by(BD2) %>% 
  summarise(mean = mean(BD17, na.rm = T))

## barra simples

b %>% 
  ggplot(aes(x = BD2, y = mean)) +
  geom_bar(stat = "identity")


b %>% 
  ggplot(aes(x = BD2, y = mean)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Estado",
    y = "Média de Idade"
  )  

b %>% 
  ggplot(aes(x = BD2, y = mean)) +
  geom_bar(stat = "identity", fill = "#74ccaf") +
  labs(
    x = "Estado",
    y = "Média de Idade"
  )  


b %>% 
  ggplot(aes(x = BD2, y = mean)) +
  geom_bar(stat = "identity", fill = "#74ccaf") +
  labs(
    title = "Média de Idade por Estado",
    subtitle = "XXXX, 2021",
    x = "Estado",
    y = "Média de Idade"
  ) +
  scale_x_discrete(labels = c("Belo Horizonte" = "BH",
                              "Rio de Janeiro" = "RJ", 
                              "São Paulo" = "SP")) +
  theme_minimal()


#36454F

## barra dupla

## ajustando dados

b <- dt %>% 
  group_by(BD2, BD3) %>% 
  summarise(mean = mean(BD17, na.rm = T))

## gráficos

b %>% 
  ggplot(aes(x = BD2, y = mean, fill = BD3)) +
  geom_bar(stat = "identity", position = "dodge")


b %>% 
  ggplot(aes(x = BD2, y = mean, fill = BD3)) +
  geom_bar(stat = "identity", position = "stack")


b %>% 
  ggplot(aes(x = BD2, y = mean, fill = BD3)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Estado",
    y = "Média de Idade",
    fill = "Sexo"
  )  

b %>% 
  ggplot(aes(x = BD2, y = mean, fill = BD3)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Média de Idade por Estado",
    subtitle = "XXXX, 2021",
    x = "Estado",
    y = "Média de Idade",
    fill = "Sexo"
  ) +
  scale_x_discrete(labels = c("Belo Horizonte" = "BH",
                              "Rio de Janeiro" = "RJ", 
                              "São Paulo" = "SP")) +
  scale_fill_manual(values = c("#397B87", "#74CCAF")) +
  theme_minimal()

# pontos e linhas ---------------------------------------------------------

## dados
b <- dt %>% 
  group_by(BD2, BD3) %>% 
  summarise(mean = mean(BD17, na.rm = T))

## transformação em fator
b$BD3 <- as.factor(b$BD3)

## gráficos

b %>% 
  ggplot(aes(x = BD2, y = mean, group = BD3)) +
  geom_point() +
  geom_line()

b %>% 
  ggplot(aes(x = BD2, y = mean, group = BD3, colour = BD3)) +
  geom_point(aes(shape=BD3), size = 2) +
  geom_line(size = 0.8, linetype = "dashed")
  

b %>% 
  ggplot(aes(x = BD2, y = mean, group = BD3, colour = BD3)) +
  geom_point(size = 2, shape = 21) +
  geom_line(size = 0.8, linetype = "dashed") +
  labs(
    title = "Média de Idade por Estado",
    subtitle = "XXXX, 2021",
    x = "Estado",
    y = "Média de Idade",
    colour = "Sexo"
  ) +
  scale_x_discrete(labels = c("Belo Horizonte" = "BH",
                              "Rio de Janeiro" = "RJ", 
                              "São Paulo" = "SP")) +
  scale_colour_manual(values = c("#397B87", "#74CCAF")) +
  theme_minimal()


# proporções --------------------------------------------------------------

b %>% 
  ggplot(aes(x = BD2, y = mean, fill = BD3)) +
  geom_bar(stat = "identity", position = "fill")


b %>% 
  ggplot(aes(x = BD2, y = mean, fill = BD3)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Estado",
    y = "Proporção",
    fill = "Sexo"
  ) 

b %>% 
  ggplot(aes(x = BD2, y = mean, fill = BD3)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Proporção de Idade por Estado",
    subtitle = "XXXX, 2021",
    x = "Estado",
    y = "Proporção",
    fill = "Sexo"
  ) +
  scale_x_discrete(labels = c("Belo Horizonte" = "BH",
                              "Rio de Janeiro" = "RJ", 
                              "São Paulo" = "SP")) +
  scale_fill_manual(values = c("#397B87", "#74CCAF")) +
  theme_minimal()


# avançados ---------------------------------------------------------------

## corte de linhas

## dados
b <- dt %>% 
  group_by(BD2, BD3) %>% 
  summarise(mean = mean(BD17, na.rm = T))

## transformação em fator
b$BD3 <- as.factor(b$BD3)

## gráficos

b %>% 
  ggplot(aes(x = BD2, y = mean, group = BD3)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 45.2)


b %>% 
  ggplot(aes(x = BD2, y = mean, group = BD3)) +
  geom_point() +
  geom_line()+
  geom_hline(yintercept = 45.2, linetype = "dashed", colour = "#74CCAF")


b %>% 
  ggplot(aes(x = BD2, y = mean, group = BD3)) +
  geom_point() +
  geom_line()+
  geom_hline(yintercept = 45.2, linetype = "dashed", colour = "#74CCAF") +
  labs(
    title = "Média de Idade por Estado",
    subtitle = "XXXX, 2021",
    x = "Estado",
    y = "Média de Idade",
    colour = "Sexo"
  ) +
  scale_x_discrete(labels = c("Belo Horizonte" = "BH",
                              "Rio de Janeiro" = "RJ", 
                              "São Paulo" = "SP")) +
  scale_colour_manual(values = c("#397B87", "#74CCAF")) +
  theme_minimal()

## facet_wrap

## dados
b <- dt %>% 
  group_by(BD2, BD3, BD6) %>% 
  count (BD6)

## transformação em fator
b$BD3 <- as.factor(b$BD3)

## gráfico

b %>% 
  ggplot(aes(x = BD2, y = n, fill = BD3)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    x = "Estado",
    y = "Frequência"
  ) +
  facet_wrap(~BD6)

b %>% 
  ggplot(aes(x = BD2, y = n, fill = BD3)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    x = "Estado",
    y = "Frequência"
  ) +
  labs(
  title = "Frequência de Praticantes por Estado e Sexo",
  subtitle = "XXXX, 2021",
  x = "Estado",
  y = "Frequência",
  fill = "Sexo"
) +
  scale_x_discrete(labels = c("Belo Horizonte" = "BH",
                              "Rio de Janeiro" = "RJ", 
                              "São Paulo" = "SP")) +
  scale_fill_manual(values = c("#397B87", "#74CCAF")) +
  theme_minimal() +
  facet_wrap(~BD6)


## coord_flip

## ajustando dados

b <- dt %>% 
  group_by(BD2) %>% 
  summarise(mean = mean(BD17, na.rm = T))

## barra simples

b %>% 
  ggplot(aes(x = BD2, y = mean)) +
  geom_bar(stat = "identity") +
  coord_flip()

b %>% 
  ggplot(aes(x = BD2, y = mean)) +
  geom_bar(stat = "identity", fill = "#74ccaf") +
  labs(
    title = "Média de Idade por Estado",
    subtitle = "XXXX, 2021",
    x = "Estado",
    y = "Média de Idade"
  ) +
  scale_x_discrete(labels = c("Belo Horizonte" = "BH",
                              "Rio de Janeiro" = "RJ", 
                              "São Paulo" = "SP")) +
  theme_minimal() +
  coord_flip()
