

# Projeto Para Git Hub

# Projeto 02 - Medindo Eficiência em Extintores de Incêndio

# # O objetivo é encontrar um modelo de machine learning para prever a eficiência do extintor. 
# Por se tratar de um projeto que envolve a segurança de pessoas a meta utilizada será um 
# acurácia de pelo menos 90%.
# O modelo irá produzir um resultado de uma classe: Extinsão do fogo ou não, portanto trata-se de um
# modelo supervisionado de classificação.

# 0 indica não extinção do fogo
# 1 indica extinção do fogo

# Diretório de trabalho:
setwd("Defina seu diretório aqui!")

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
