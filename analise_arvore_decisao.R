# ÁRVORES DE DECISÃO

## Descrição: Usa algoritmos de árvore de decisão para dividir a população com base em regras de decisão.
## Vantagens: Fácil de interpretar e implementar.
## Desvantagens: Pode superajustar os dados.

## Particionando os dados utilizando o método holdout
library(rminer)

df = read.csv('credit_data.csv', header = T)

# O C5.0 obriga que a classe seja um factor (nem string serve)
# No caso do nosso dataset, a classe é originalmente um inteiro 0 ou 1. 
# Então convertemos para factor primeiro: 
df$default = as.factor(df$default)

# Removendo o ID do cliente, pois não será útil para os modelos.
df = df[,-1]

# Aplicando particionamento holdout, com 70% treino e 30% teste
data = holdout(df$default, ratio = 0.7, mode = 'stratified')

# O retorno da função holdout dá as posições das entradas que foram selecionadas.
# Por isso precisamos voltar ao dataframe e selecionar as linhas, conforme abaixo:
train = df[data$tr, ]
test = df[data$ts, ]

# No nosso dataframe, a classe está na quarta coluna.
X_train = train[,-4]
X_test = test[,-4]

y_train = train[,4]
y_test = test[,4]


## Treinamento e predição dos modelos

library(C50)
tree_model = C5.0(X_train, y_train)  # treinamento

### Predição
outcome = predict(tree_model, X_test, type = 'class')
outcome = cbind(test, outcome)

### Gerando a matriz de confusão
cm = table(outcome$default, outcome$outcome)
cm

## Computando as métricas manualmente:
acc1 = (cm[1,1] + cm[2,2]) / sum(cm)
prec1 = cm[2,2] / (cm[2,2] + cm[1,2] )
rec1 = cm[2,2] / (cm[2,2] + cm[2,1] )
f1sc1 = 2*(prec1*rec1/(prec1 + rec1))
c50_metrics = c(acc1,prec1,rec1,f1sc1)

### Utilizando o método rpart

library(rpart)

tree_model2 = rpart(default ~ ., data = train, method = 'class')

# A notação ~. serve para indicar que o atributo classe é "default" e todas as outras colunas
# serão usadas como atributos preditivos. Também é possível usar apenas colunas específicas 
# como atributos preditivos. Por exemplo, se quiséssemos usar apenas loan e age, seria:
# rpart(default ~ loan + age, data = train, method = 'class')

# Predição
outcome2 = predict(tree_model2, X_test, type = 'class')
outcome2 = cbind(test, outcome2)

# Gerando a matriz de confusão
cm2 = table(outcome2$default, outcome2$outcome2)
cm2

# Computando as métricas manualmente:
acc2 = (cm2[1,1] + cm2[2,2]) / sum(cm2)
prec2 = cm2[2,2] / (cm2[2,2] + cm2[1,2] )
rec2 = cm2[2,2] / (cm2[2,2] + cm2[2,1] )
f1sc2 = 2*(prec2*rec2/(prec2 + rec2))
rpart_metrics = c(acc2,prec2,rec2,f1sc2)

## Comparativo dos modelos
metrics = data.frame(cbind(c50_metrics,rpart_metrics))
rownames(metrics) = c('Accuracy','Precision','Recall','F1')
colnames(metrics) = c('C5.0','rpart')

metrics

### Visualizando as árvores
# Com o pacote C50
summary(tree_model)

# Com o pacote rport
tree_model12