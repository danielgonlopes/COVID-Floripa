rm(list=ls())
#### PACOTES ####
library(tidyverse)
library(ISOweek)



#### LEITURA E LIMPEZA DOS DADOS ####

df <-  read.csv("Dados/covid_florianopolis.csv", encoding="UTF-8")
dados <- df

# Transformando data de notificação em Date
dados$data_notificacao <- as.Date(dados$data_notificacao) 

# Criando semanas
dados$week <- ISOweek(dados$data_notificacao)

# Filtrando datas de notificações com erro de digitação
dados <- dados %>% 
  filter(data_notificacao >= "2020-03-01") 

# Filtrando casos de outras cidades de SC (Mantendo Florianópolis e outros estados)
dados <- dados %>% 
  filter(municipio_residencia == "FLORIANOPOLIS" | uf_residencia != "SANTA CATARINA") 


#### PERFIL DOS CASOS ####

#### PORCENTAGEM DE CASOS POR CONDIÇÃO ####


#### PERFIL DOS ÓBITOS ####

dados_obitos <- dados %>%
  filter(obito == "SIM")

# Óbitos por sexo, idade e raça

table(dados_obitos$sexo)
table(dados_obitos$faixa_idade)
table(dados_obitos$raca)

# Gráfico de sexo dividido por faixa de idade

dados_obitos %>%
  filter(faixa_idade != "ATÉ 9 ANOS") %>%
  ggplot(aes(x = sexo)) +
  geom_bar() + 
  facet_grid(. ~ faixa_idade) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Gráfico de faixa de idade dividido por raça

dados_obitos %>%
  filter(faixa_idade != "ATÉ 9 ANOS") %>%
  ggplot(aes(x = faixa_idade)) +
  geom_bar() +
  facet_grid(. ~ raca) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Quantidade de óbitos por bairro

obitos_bairro <- data.frame(table(dados_obitos$bairro))

obitos_bairro_arranged <- obitos_bairro %>%
  arrange(desc(Freq))

head(obitos_bairro_arranged, 10)



#### PORCENTAGEM DE ÓBITOS POR CONDIÇÃO ####

# Porcentagem de morte por infectado
t_infectado <- table(dados$obito)
round(t_infectado[2]/t_infectado[1]*100, 2)

# Função para cálculo porcentagem de morte por condição (comorbidades)
porcentagem_obitos_por_condicao <- function(a, b) {
  tabela <- table(a, b)
  round(tabela[4]/tabela[3]*100, 2)
}

# Porcentagem de morte para quem tem diabetes
porcentagem_obitos_por_condicao(dados$obito, dados$diabetes)

# Porcentagem de morte para quem tem doença cardíaca crônica
porcentagem_obitos_por_condicao(dados$obito, dados$doenca_card_cronica)

# Porcentagem de morte para quem tem doença respiratória descompensada
porcentagem_obitos_por_condicao(dados$obito, dados$doenca_resp_descompensada)

# Porcentagem de morte para quem tem doenças renais avançadas
porcentagem_obitos_por_condicao(dados$obito, dados$doencas_renais_avancado)

# Porcentagem de morte por faixa de idade
t_faixa_idade <- table(dados$obito, dados$faixa_idade)

list_idade <- list()

for (i in c(2,4,6,8,10,12,14,16,18,20)){
  list_idade[[i]] <- list(round(t_faixa_idade[i]/t_faixa_idade[i-1]*100, 2))
}

morte_idade <- data.frame(list_idade[2],list_idade[4],list_idade[6],list_idade[8],
                          list_idade[10],list_idade[12],list_idade[14],list_idade[16],
                          list_idade[18],list_idade[20])
colnames(morte_idade) <- levels(dados$faixa_idade)

morte_idade



#### ANÁLISE TEMPORAL ####

#### SETANDO PERÍODO PARA ANÁLISE ####

data_de_corte <- "2021-01-01" # Inserir data



#### CASOS POR DIA ####

# Filtrando casos após a data de corte e somente idosos (70+)

dados_casos_dia <- dados %>%
  filter(data_notificacao >= data_de_corte) %>%
  group_by(data_notificacao, faixa_idade) %>%
  summarise(quantidade = n()) %>%
  filter(faixa_idade == "90 ANOS OU MAIS" | faixa_idade == "80 A 89 ANOS" | faixa_idade == "70 A 79 ANOS" | faixa_idade == "60 A 69 ANOS") %>%
  mutate(vacinado = faixa_idade == "90 ANOS OU MAIS" | faixa_idade == "80 A 89 ANOS")


# Plotando gráfico com barras empilhadas (100%)

ggplot(dados_casos_dia, aes(x = data_notificacao, y = quantidade, color = faixa_idade)) +
  geom_line()


# Plotando gráfico com barras

ggplot(dados_casos_dia, aes(x = data_notificacao, y = quantidade, fill = faixa_idade)) +
  geom_bar(stat = "identity", position = "fill")



#### CASOS POR SEMANA ####

# Filtrando casos após a data de corte e somente idosos (70+)

dados_casos_semana <- dados %>%
  filter(data_notificacao >= data_de_corte) %>%
  group_by(week, faixa_idade) %>%
  summarise(quantidade = n()) %>%  
  filter(faixa_idade == "90 ANOS OU MAIS" | faixa_idade == "80 A 89 ANOS" | faixa_idade == "70 A 79 ANOS" | faixa_idade == "60 A 69 ANOS") %>%
  mutate(vacinado = faixa_idade == "90 ANOS OU MAIS" | faixa_idade == "80 A 89 ANOS")


# Plotando gráfico com barras empilhadas (100%)

ggplot(dados_casos_semana, aes(x = week, y = quantidade, fill = faixa_idade)) +
  geom_bar(stat = "identity", position = "fill")


# Plotando gráfico com barras

ggplot(dados_casos_semana, aes(x = week, y = quantidade, fill = faixa_idade)) +
  geom_bar(stat = "identity", position = "dodge")



#### OBITOS POR DIA ####

# Filtrando somente obitos, após a data de corte, sem missings e somente idosos (70+)

dados_obitos_dia <- dados %>%
  filter(obito == "SIM" & data_notificacao >= data_de_corte) %>%
  filter(data_obito != "") %>%
  group_by(data_obito, faixa_idade) %>%
  summarise(quantidade = n()) %>%
  filter(faixa_idade == "90 ANOS OU MAIS" | faixa_idade == "80 A 89 ANOS" | faixa_idade == "70 A 79 ANOS")


# Plotando gráfico com barras empilhadas (100%)

ggplot(dados_obitos_dia, aes(x = data_obito, y = quantidade, fill = faixa_idade)) +
  geom_bar(stat = "identity", position="fill")


# Plotando gráfico com barras

ggplot(dados_obitos_dia, aes(x = data_obito, y = quantidade, fill = faixa_idade)) +
  geom_bar(stat = "identity", position = "dodge")



#### OBITOS POR SEMANA ####

# Filtrando somente obitos, após a data de corte, sem missings e somente idosos (70+)

dados_obitos_semana <- dados %>%
  filter(obito == "SIM" & data_notificacao >= data_de_corte) %>%
  filter(data_obito != "") %>%
  group_by(week, faixa_idade) %>%
  summarise(quantidade = n()) %>%
  filter(faixa_idade == "90 ANOS OU MAIS" | faixa_idade == "80 A 89 ANOS" | faixa_idade == "70 A 79 ANOS")


# Plotando gráfico com barras empilhadas (100%)

ggplot(dados_obitos_semana, aes(x = week, y = quantidade, fill = faixa_idade)) +
  geom_bar(stat = "identity", position="fill")


# Plotando gráfico com barras

ggplot(dados_obitos_semana, aes(x = week, y = quantidade, fill = faixa_idade)) +
  geom_bar(stat = "identity", position = "dodge")
