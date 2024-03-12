

# Projeto Para Git Hub

# Projeto 02 - Medindo Eficiência em Extintores de Incêndio

# # O objetivo é encontrar um modelo de machine learning para prever a eficiência do extintor. 
# Por se tratar de um projeto que envolve a segurança de pessoas a meta utilizada será um 
# acurácia de pelo menos 90%.
# O modelo irá produzir um resultado de uma classe: Extinsão do fogo ou não, portanto trata-se de um
# modelo supervisionado de classificação.

# 0 indica não extinção do fogo
# 1 indica extinção do fogo

# Será usado linguagem R para elaboração da solução do projeto


# Diretório de trabalho:
setwd("C:/Users/fhveg/OneDrive/Documentos/Git_Projects/Efficiency-Fire-Extinguishers")

# Pacotes usados no desenvolvimento do projeto:
library(readxl)
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(caret)

# Carregando os dados:
dados <- read_excel("Acoustic_Extinguisher_Fire_Dataset.xlsx",sheet = "A_E_Fire_Dataset")

# Conhecendo o dataset:
View(dados)
str(dados)
dim(dados)

# Retirando uma amostra dos dados para testes posteriores:
novos.dados <- dados[8000:8500,]

# Foi retirado uma amostra do dataset que não fará parte do trainamento.
# Reagrupando os dados restantes:

dados.2 <- rbind(dados[1:7999,],dados[8501:17442,])

# Adicionando uma coluna com valores de extinção ou não do fogo:
dados.2$STATUS_STRING <- c(ifelse(dados.2$STATUS == 0,yes = "Not_Extint",no = "Extint"))

dim(dados.2)
View(dados.2)

# Valores nulos:
sum(is.na(dados.2))

# -----------------------------------------------------------------------------

### ANÁLISE EXPLORATÓRIA

# Proporção dos valores das colunas SIZE, STATUS e FUEL:
table(dados$SIZE)
table(dados$STATUS)
table(dados$FUEL)
table(dados.2$DISTANCE)

# Criando a variável STATUS_STRING para auxiliar na geração dos gráficos:
dados.2$STATUS_STRING <- as.factor(dados.2$STATUS_STRING)
str(dados.2)

# Gráfico de barras da variável FUEL:
ggplot(dados.2, aes(x=FUEL)) + 
  geom_bar(color="black", fill="#845EC2")

# Gráfico de barras da variável SIZE:
ggplot(dados.2, aes(x=as.factor(SIZE) )) +
  geom_bar(color="black", fill="#2C73D2")

# Gráfico de barras da variável STATUS:
ggplot(dados.2, aes(x=as.factor(STATUS) )) +
  geom_bar(color="black", fill="#008F7A" )


# Relação entre variáveis FUEL e STATUS:
# Criando a tabela de relacionamento:
tb_fuel <- table(dados.2$FUEL,dados.2$STATUS_STRING)
tb_fuel
tb_fuel.df <- as.data.frame(tb_fuel)
colnames(tb_fuel.df) <- c("FUEL","CONDITION","FREQUENCY")
tb_fuel.df

# Gráfico de barras para a relação FUEL e STATUS da tabela tb_fuel_df:
ggplot(tb_fuel.df, aes(fill=CONDITION, y=FREQUENCY, x=FUEL)) + 
  geom_bar(position="dodge", stat="identity")

# -----------------------------------------------------------------------------------

# Análise Estatística das Variáveis Categóricas:

# Avaliando o relacionamento das variáveis categóricas FUEL e STATUS:

# Porcentagem do relacionamento entre FUEL e STATUS:
prop.table(table(dados.2$FUEL,dados.2$STATUS),margin = 2)

# Porcentagem dos valores totais entre FUEL e STATUS:
prop.table(marginSums(table(dados.2$FUEL,dados.2$STATUS),margin = 1))

# Reunindo as informações acima em uma única tabela:
prop.fuel <- prop.table(table(dados.2$FUEL,dados.2$STATUS_STRING),margin = 2)
prop.total <- prop.table(marginSums(table(dados.2$FUEL,dados.2$STATUS_STRING),margin = 1))

# Combinando as tabelas:
prop.fuel.total <- cbind(prop.fuel,prop.total)
prop.fuel.total

# Tabela total em porcentagem do relacionamento das variáveis FUEL e STATUS_STRING:
df.fuel.total <- as.data.frame(prop.fuel.total)
df.fuel.total

# Cálculo do qui-quadrado para as duas variáveis FUEL e STATUS:
chisq.test(table(dados.2$FUEL,dados.2$STATUS))
# Resultado de 115.9 do x-squared e p-value muito pequeno
# Indica que há relação, porém fraca, entre as variáveis



# Análise Estatística das Variáveis Numéricas

# Análise das variáveis quantitativas em relação a variável STATUS:

# Análise da variável SIZE em relação a variável alvo STATUS
tb_size <- table(dados.2$SIZE,dados.2$STATUS_STRING)
tb_size <- as.data.frame(tb_size)
colnames(tb_size) <- c("SIZE","CONDITION","FREQUENCY")
tb_size

# Gráfico do tamanho e a condição de extinção do fogo:
ggplot(tb_size, aes(fill=CONDITION, y=FREQUENCY, x=SIZE)) + 
  geom_bar(position="stack", stat="identity")

# Análise da variável DISTANCE e STATUS_STRING:
tb_dist <- table(dados.2$DISTANCE,dados.2$STATUS_STRING)
tb_dist

# Gráfico boxplot da relação da variável STATUS_STRING com FREQUENCY:
dados.2 %>%
  ggplot( aes(x=STATUS_STRING, y=FREQUENCY, fill=STATUS_STRING)) +
  geom_boxplot() +
  ggtitle(label = "STATUS VS FREQUENCY") 

# Gráfico boxplot da relação da variável STATUS com AIRFLOW:
dados.2 %>%
  ggplot( aes(x=STATUS_STRING, y=AIRFLOW, fill=STATUS_STRING)) +
  geom_boxplot() +
  ggtitle(label = "STATUS VS AIRFLOW") 

# Gráfico boxplot da relação da variável STATUS com DESIBEL:
dados.2 %>%
  ggplot( aes(x=STATUS_STRING, y=DESIBEL, fill=STATUS_STRING)) +
  geom_boxplot() +
  ggtitle(label = "STATUS VS DESIBEL") 

# Gráfico de correlação das variáveis quantitativas:
dados.num <- dados.2[,-c(2,7,8)]
dados.num

# Calculando a correlação:
plot.cor <- cor(dados.num)
plot.cor
# Gráfico de correlação:
ggcorr(plot.cor, method = c("everything", "pearson"))


# ---------------------------------------------------------------------------------

### PRÉ-PROCESSAMENTO

# Criando variáveis dummy para variável FUEL:
dummye <- dummyVars(STATUS ~ FUEL,data = dados.2)
fuel.dummye <- predict(dummye,newdata = dados.2)
head(fuel.dummye)

# Agrupando as variáveis dummy ao dataset dados.2:
dados.3 <- cbind(fuel.dummye,dados.2)
dados.3$FUEL <- NULL
dados.3$STATUS_STRING <- NULL
head(dados.3)

# Padronizando dados das colunas numéricas:
dados.3 <- scale(dados.3[c("DISTANCE","DESIBEL","AIRFLOW","FREQUENCY")])
head(dados.3)

# Combinando com as variáveis dummy:
dados <- cbind(fuel.dummye,dados.3,dados.2$STATUS)
dados <- as.data.frame(dados)
colnames(dados)[which(colnames(dados) == 'V9')] <- 'STATUS'
head(dados)

# -----------------------------------------------------------------------------

# Preparo para modelagem:

# Transformando a variável target em fator:
dados$STATUS <- as.factor(dados$STATUS)

# Criando um index:
index <- createDataPartition(dados$STATUS, times = 1,p = 0.70, list = FALSE)

# Dados de treino e teste

# Dados de treino:
treino <- dados[index,]

# Dados de teste:
teste <- dados[-index,]

# Criando parâmetro traincontrol:
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3, verboseIter = TRUE)
metric <- "Accuracy"


# ----------------------------------------------------------------------------------

# Modelagem

# Bagged CART
treebag.fit <- train(STATUS ~., data = treino, method = "treebag", metric = metric,
                     trControl = trainControl)

# Random Forest:
rf.fit <- train(STATUS ~., data = treino, method = "rf", metric = metric,
                trControl = trainControl)

# GBM - Stochastic Gradient Boosting
gbm.fit <- train(STATUS~., data = treino, 
                 method = "gbm",metric = metric,trControl = trainControl, 
                 verbose = FALSE)
# C5.0
c50.fit <- train(STATUS ~., data = treino, method = "C5.0", metric = metric,
                 trControl = trainControl)

# GLMNET - Regularized Logistic Regression
glmnet.fit <- train(STATUS ~., data = treino, method="glmnet",
                    metric=metric,trControl=trainControl)

# KNN - k-Nearest Neighbors 
knn.fit <- train(STATUS ~., data = treino, method="knn",
                 metric=metric,trControl=trainControl)

# CART - Classification and Regression Trees (CART), 
cart.fit <- train(STATUS ~., data = treino, method="rpart",
                  metric=metric,trControl=trainControl)

# NB - Naive Bayes (NB)
Grid = expand.grid(usekernel=TRUE,adjust=1,fL=c(0.2,0.5,0.8))
nb.fit <- train(STATUS ~., data = treino, method="nb",
                metric=metric,trControl=trainControl,
                tuneGrid=Grid)

# ---------------------------------------------------------------------------------

# Visualizando os resultados:

# Criando a lista de resultados dos modelos:
lista.modelos <- resamples(list("bagget.cart" = treebag.fit,
                                "random" = rf.fit,
                                "gbm" = gbm.fit,
                                "c50" = c50.fit,
                                "glmnet" = glmnet.fit,
                                "knn" = knn.fit,
                                "cart" = cart.fit,
                                "naive.bayes" = nb.fit))

# Resumo dos resultados dos modelos:
summary(object = lista.modelos)

# Visualizando:
bwplot(x = lista.modelos)

# --------------------------------------------------------------------------------

# Selecionando os algoritmos para as previsões:

# Os 3 melhores foram: c50, knn e gbm

# Fazendo Previsões

# Modelo c50:
prev1.c50 <- predict(object = c50.fit, teste)
summary(prev1.c50)

  # Comparando os resultados:
    cm.c50 <- confusionMatrix(prev1.c50,teste$STATUS)
    cm.c50

# Modelo knn:
prev1.knn <- predict(object = knn.fit,teste)
summary(prev1.knn) 

  # Comparando os resultados:
    cm.knn <- confusionMatrix(prev1.knn,teste$STATUS)
    cm.knn


# Modelo gbm:
prev1.gbm <- predict(object = gbm.fit,teste)
summary(prev1.gbm) 

  # Comparando os resultados:
    cm.gbm <- confusionMatrix(prev1.gbm,teste$STATUS)
    cm.gbm

# -------------------------------------------------------------------------------







