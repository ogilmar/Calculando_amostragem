# �RVORES DE DECIS�O

## Descri��o: Usa algoritmos de �rvore de decis�o para dividir a popula��o com base em regras de decis�o.
## Vantagens: F�cil de interpretar e implementar.
## Desvantagens: Pode superajustar os dados.

## Particionando os dados utilizando o m�todo holdout
library(rminer)

df = read.csv('credit_data.csv', header = T)

# O C5.0 obriga que a classe seja um factor (nem string serve)
# No caso do nosso dataset, a classe � originalmente um inteiro 0 ou 1. 
# Ent�o convertemos para factor primeiro: 
df$default = as.factor(df$default)

# Removendo o ID do cliente, pois n�o ser� �til para os modelos.
df = df[,-1]

# Aplicando particionamento holdout, com 70% treino e 30% teste
data = holdout(df$default, ratio = 0.7, mode = 'stratified')

# O retorno da fun��o holdout d� as posi��es das entradas que foram selecionadas.
# Por isso precisamos voltar ao dataframe e selecionar as linhas, conforme abaixo:
train = df[data$tr, ]
test = df[data$ts, ]

# No nosso dataframe, a classe est� na quarta coluna.
X_train = train[,-4]
X_test = test[,-4]

y_train = train[,4]
y_test = test[,4]


## Treinamento e predi��o dos modelos

library(C50)
tree_model = C5.0(X_train, y_train)  # treinamento

### Predi��o
outcome = predict(tree_model, X_test, type = 'class')
outcome = cbind(test, outcome)

### Gerando a matriz de confus�o
cm = table(outcome$default, outcome$outcome)
cm

## Computando as m�tricas manualmente:
acc1 = (cm[1,1] + cm[2,2]) / sum(cm)
prec1 = cm[2,2] / (cm[2,2] + cm[1,2] )
rec1 = cm[2,2] / (cm[2,2] + cm[2,1] )
f1sc1 = 2*(prec1*rec1/(prec1 + rec1))
c50_metrics = c(acc1,prec1,rec1,f1sc1)

### Utilizando o m�todo rpart

library(rpart)

tree_model2 = rpart(default ~ ., data = train, method = 'class')

# A nota��o ~. serve para indicar que o atributo classe � "default" e todas as outras colunas
# ser�o usadas como atributos preditivos. Tamb�m � poss�vel usar apenas colunas espec�ficas 
# como atributos preditivos. Por exemplo, se quis�ssemos usar apenas loan e age, seria:
# rpart(default ~ loan + age, data = train, method = 'class')

# Predi��o
outcome2 = predict(tree_model2, X_test, type = 'class')
outcome2 = cbind(test, outcome2)

# Gerando a matriz de confus�o
cm2 = table(outcome2$default, outcome2$outcome2)
cm2

# Computando as m�tricas manualmente:
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

### Visualizando as �rvores
# Com o pacote C50
summary(tree_model)

# Com o pacote rport
tree_model12