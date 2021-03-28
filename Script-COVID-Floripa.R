rm(list=ls())
#### PACOTES ####
library(tidyverse)
library(ISOweek) # Para agrupar dias em semanas
library(DescTools) # Para calcular PseudoR2 do modelo Logit
library(caret) # Para rodar Árvore de Decisão
library(rattle) # Para plotar Árvore de Decisão
library(randomForest) # Para rodar Modelo Random Forest

#### LEITURA E LIMPEZA DOS DADOS ####

df <-  read.csv("Dados/covid_florianopolis.csv", encoding="UTF-8", stringsAsFactors = T)
dados <- df

# Transformando data de notificação em Date
dados$data_notificacao <- as.Date(dados$data_notificacao)

# Criando semanas
dados$week <- ISOweek::ISOweek(dados$data_notificacao)

# Filtrando datas de notificações com erro de digitação
dados <- dados %>% 
  filter(data_notificacao >= "2020-03-01")

# Filtrando somente casos confirmados  
dados_confirmados <- dados %>%   
  filter(classificacao_final == "CONFIRMAÇÃO LABORATORIAL" | 
           classificacao_final == "CONFIRMAÇÃO CLÍNICO EPIDEMIOLÓGICO" )

# Verificando quantidade de óbitos
table(dados_confirmados$obito)



#### PERFIL DOS CASOS ####

#### PORCENTAGEM DE CASOS POR CONDIÇÃO ####


#### PERFIL DOS ÓBITOS ####

dados_obitos <- dados_confirmados %>%
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

dados_casos_dia <- dados_confirmados %>%
  filter(data_notificacao >= data_de_corte) %>%
  group_by(data_notificacao, faixa_idade) %>%
  summarise(quantidade = n()) %>%
  filter(faixa_idade == "90 ANOS OU MAIS" | faixa_idade == "80 A 89 ANOS" | faixa_idade == "70 A 79 ANOS" | faixa_idade == "60 A 69 ANOS") %>%
  mutate(vacinado = faixa_idade == "90 ANOS OU MAIS" | faixa_idade == "80 A 89 ANOS")


# Plotando gráfico de linha

ggplot(dados_casos_dia, aes(x = data_notificacao, y = quantidade, color = faixa_idade)) +
  geom_line()


# Plotando gráfico com barras empilhadas (100%)

ggplot(dados_casos_dia, aes(x = data_notificacao, y = quantidade, fill = faixa_idade)) +
  geom_bar(stat = "identity", position = "fill")



#### CASOS POR SEMANA ####

# Filtrando casos após a data de corte e somente idosos (70+)

dados_casos_semana <- dados_confirmados %>%
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

dados_obitos_dia <- dados_confirmados %>%
  filter(obito == "SIM" & data_notificacao >= data_de_corte) %>%
  filter(data_obito != "") %>%
  group_by(data_obito, faixa_idade) %>%
  summarise(quantidade = n()) %>%
  filter(faixa_idade == "90 ANOS OU MAIS" | faixa_idade == "80 A 89 ANOS" | faixa_idade == "70 A 79 ANOS")


# Plotando gráfico com barras empilhadas (100%)

ggplot(dados_obitos_dia, aes(x = data_obito, y = quantidade, fill = faixa_idade)) +
  geom_bar(stat = "identity", position="fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Plotando gráfico com barras

ggplot(dados_obitos_dia, aes(x = data_obito, y = quantidade, fill = faixa_idade)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#### OBITOS POR SEMANA ####

# Filtrando somente obitos, após a data de corte, sem missings e somente idosos (70+)

dados_obitos_semana <- dados_confirmados %>%
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



#### MODELO LOGIT ####

# Treinando o modelo de regressão logística

logit_model <- glm(obito ~ sexo  + faixa_idade + raca  + + diabetes + doenca_resp_descompensada 
             + doencas_renais_avancado + doenca_card_cronica + gestante_alto_risco
             + portador_doenca_cromossomica + imunossupressao, data = dados_confirmados, 
             family = binomial)

summary(logit_model)


# Cálculando o PseudoR2

DescTools::PseudoR2(logit_model)


# Prevendo a probabilidade de cada caso do modelo de regressão logística

logit_model_probs <- predict(logit_model,type = "response")


# Prevendo o resultado de cada probabilidade do modelo de regressão logística

logit_model_preds <- ifelse(logit_model_probs > 0.5, "Pred Obito", "Pred Vivo")


# Calculando a matriz de confusão do modelo de regressão logística

logit_matriz_confusao <- table(dados_confirmados$obito, logit_model_preds)
logit_matriz_confusao


# Calculando porcentagem de acertos do modelo de regressão logística
logit_acertos <- logit_matriz_confusao[2] + logit_matriz_confusao[3]
logit_porcentagem_acertos <- logit_acertos / length(dados_confirmados$obito)
logit_porcentagem_acertos



#### DECISION TREE ####


# Set seed

set.seed(123)


# Criando datasets de treino e teste

inTrain <- caret::createDataPartition(y = dados_confirmados$obito, p = 0.7, list = FALSE)

treino <- dados_confirmados[inTrain, ]

teste <- dados_confirmados[-inTrain, ]


# Treinando o modelo de árvore de decisão no dataset de treino

Tree_Model <- caret::train(obito ~ ., method = "rpart", data = treino[,c(7:9,16:27,33)])


# Plotando árvore de decisão

rattle::fancyRpartPlot(Tree_Model$finalModel)


# Prevendo o resultado do modelo de árvore de decisão no dataset de teste

Tree_Preds <- predict(Tree_Model, newdata = teste[,c(7:9,16:27)])


# Calculando a matriz de confusão do modelo de árvore de decisão

confusionMatrix(Tree_Preds, teste$obito)



#### RANDOM FOREST ####

# Treinando o modelo de random forest no dataset de treino 

forest_model <- randomForest::randomForest(obito ~ ., data = treino[,c(7:9,16:27,33)])


# Prevendo o resultado do modelo de random forest no dataset de teste

forest_model_preds <- predict(forest_model, newdata = teste[,c(7:9,16:27)])


# Calculando a matriz de confusão do modelo de random forest

confusionMatrix(forest_model_preds, teste$obito)


