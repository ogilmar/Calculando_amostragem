# Análise de Componentes Principais (PCA)

##Descrição: Reduz a dimensionalidade dos dados, identificando as principais variáveis que explicam a maior parte da variância.
##Vantagens: Útil quando há muitas variáveis correlacionadas.
##Desvantagens: Pode ser difícil interpretar os componentes principais.

# Carregando os pacotes

library(tidyverse)
library(readr)
library(FactoMineR)
library(factoextra)
library(DT)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(gt)

# Carregando os dados

dados <- read.csv("forestfires.csv", sep = ",")
datatable(dados,
          class = "row-border hover",
          options = list(
            scrollX = TRUE,
            dom = 'ltipr'
          ))

## Caso havam variáveis numéricas e não numéricas no mesmo conjunto de dados, use o comando a baixo para poder extrair as variáveis numéricas.

dados_num <- dados[,c(5:13)]
datatable(dados_num,
          class = "row-border hover",
          options = list(
            scrollX = TRUE,
            dom = 'ltipr'
          ))

# Plotando Matriz de Correlação
ggcorrplot(
  cor(dados_num),
  hc.order = T,
  type = "lower",
  lab = T,
  colors = c("#003262", "#FFFFFF", "#D21E1C")
)

# Matriz de variância e covariância
Mcov<-cov(dados_num)
Mcov<-round(Mcov, 4)
datatable(Mcov,
          class = "row-border hover",
          options = list(
            scrollX = TRUE,
            dom = 'ltipr'
          ))


# Análise de Principais Componentes

res.pca<-PCA(dados_num, graph = F)
eig.val <- as.data.frame(get_eigenvalue(res.pca))
row.names(eig.val) <- c("Componente.1", "Componente.2", "Componente.3", "Componente.4", "Componente.5", "Componente.6", "Componente.7", "Componente.8", "Componente.9")
nomes<-rownames(eig.val)
eig.val<-cbind(nomes, eig.val)
colnames(eig.val)<-c("Componente", "Autovalor", "Percentual", "Acumulado")
gt(eig.val) %>%
  tab_header(title = "Resumo dos componentes") %>%
  fmt_number(columns = c(2:4), decimals = 4) %>%
  opt_stylize(style = 3, color = "blue") %>%
  tab_options(table.width ="500px") 

# Seguindo o princípio de Pareto (80% dos resultados são gerado por 20% das causas) deve-se selecionar a quantidade de componentes que explique 80% dos dados. NO exemplo, 5 componentes.

fviz_eig(res.pca, addlabels = TRUE, xlab='Componentes', ylab = 'Percentual da variância explicada', ylim = c(0, 50))

# Investigando cada interação entre os componentes
fviz_pca_var(res.pca, col.var = "contrib",
             axes = c(1, 2),
             gradient.cols = c("#003262", "#E7B800", "#D21E1C"),
             repel = TRUE 
)

fviz_pca_var(res.pca, col.var = "contrib",
             axes = c(3, 4),
             gradient.cols = c("#003262", "#E7B800", "#D21E1C"),
             repel = TRUE 
)

# Plotando as combinações entre todos os componentes
colors = c("#003262", "#FFFFFF", "#FF0000")
pal = colorRampPalette(colors)
grad_colors = pal(1000)
corrplot(res.pca$var$cos2, is.corr=FALSE, col=grad_colors)

# Contribuição de cada componente
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10,
             title ="Contribuição das variáveis para o componente 1"
)

fviz_contrib(res.pca, choice = "var", axes = 2, top = 10,
             title ="Contribuição das variáveis o componente 2"
)

fviz_contrib(res.pca, choice = "var", axes = 3, top = 10,
             title ="Contribuição das variáveis o componente 3"
)
