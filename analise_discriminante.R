#  AN�LISE DISCRIMINANTE

#Descri��o: Classifica indiv�duos em grupos pr�-definidos com base em vari�veis preditoras.
#Vantagens: �til quando os estratos j� s�o conhecidos.
#Desvantagens: Requer conhecimento pr�vio dos estratos.

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

## Aplicando LDA (An�lise Discriminante Linear)
lda.iris <- lda(Species ~ ., train)
lda.iris #show results

plot(lda.iris, col = as.integer(train$Species))
plot(lda.iris, dimen = 1, type = "b")

## LDA plots particionados
partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train, method="lda")

## Predi��es LDA
lda.train <- predict(lda.iris)
train$lda <- lda.train$class
table(train$lda,train$Species)

lda.test <- predict(lda.iris,test)
test$lda <- lda.test$class
table(test$lda,test$Species)

#An�lise Discriminante Quadr�tica (QDA)
qda.iris <- qda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, train)
qda.iris #show results

##QDA quadr�ticas
partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train, method="qda")

##Predi��es das QDAs
qda.train <- predict(qda.iris)
train$qda <- qda.train$class
table(train$qda,train$Species)

qda.test <- predict(qda.iris,test)
test$qda <- qda.test$class
table(test$qda,test$Species)
