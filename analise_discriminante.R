#  ANÁLISE DISCRIMINANTE

#Descrição: Classifica indivíduos em grupos pré-definidos com base em variáveis preditoras.
#Vantagens: Útil quando os estratos já são conhecidos.
#Desvantagens: Requer conhecimento prévio dos estratos.

## Carregando pacotes
library(tidyverse)
library(MASS)
library(klaR)
set.seed(101)
sample_n(iris, 10)

## Preparando os dados
training_sample <- sample(c(TRUE, FALSE), nrow(iris), replace = T, prob = c(0.6,0.4))
train <- iris[training_sample, ]
test <- iris[!training_sample, ]

## Aplicando LDA (Análise Discriminante Linear)
lda.iris <- lda(Species ~ ., train)
lda.iris #show results

plot(lda.iris, col = as.integer(train$Species))
plot(lda.iris, dimen = 1, type = "b")

## LDA plots particionados
partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train, method="lda")

## Predições LDA
lda.train <- predict(lda.iris)
train$lda <- lda.train$class
table(train$lda,train$Species)

lda.test <- predict(lda.iris,test)
test$lda <- lda.test$class
table(test$lda,test$Species)

#Análise Discriminante Quadrática (QDA)
qda.iris <- qda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, train)
qda.iris #show results

##QDA quadráticas
partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train, method="qda")

##Predições das QDAs
qda.train <- predict(qda.iris)
train$qda <- qda.train$class
table(train$qda,train$Species)

qda.test <- predict(qda.iris,test)
test$qda <- qda.test$class
table(test$qda,test$Species)
