---
title: "Projeto02_DSA"
output: html_document
date: "2024-03-07"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Projeto-02 DSA - Medindo Eficiência em Extintores de Incêndio

Este projeto consiste em criar um modelo de Machine Learning para prever a eficiência de determinado
extintor de incêndio

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r dados}
'Tabela com Valores da variável STATUS'
table(dados$STATUS)

```

## Including Plots

Segue análise sobre os tipos de combustíveis:

```{r, echo=FALSE}
ggplot(dados.2, aes(x=FUEL)) + 
  geom_bar(color="black", fill="#00abff")
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
