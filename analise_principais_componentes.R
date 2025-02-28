# An�lise de Componentes Principais (PCA)

##Descri��o: Reduz a dimensionalidade dos dados, identificando as principais vari�veis que explicam a maior parte da vari�ncia.
##Vantagens: �til quando h� muitas vari�veis correlacionadas.
##Desvantagens: Pode ser dif�cil interpretar os componentes principais.

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

## Caso havam vari�veis num�ricas e n�o num�ricas no mesmo conjunto de dados, use o comando a baixo para poder extrair as vari�veis num�ricas.

dados_num <- dados[,c(5:13)]
datatable(dados_num,
          class = "row-border hover",
          options = list(
            scrollX = TRUE,
            dom = 'ltipr'
          ))

# Plotando Matriz de Correla��o
ggcorrplot(
  cor(dados_num),
  hc.order = T,
  type = "lower",
  lab = T,
  colors = c("#003262", "#FFFFFF", "#D21E1C")
)

# Matriz de vari�ncia e covari�ncia
Mcov<-cov(dados_num)
Mcov<-round(Mcov, 4)
datatable(Mcov,
          class = "row-border hover",
          options = list(
            scrollX = TRUE,
            dom = 'ltipr'
          ))


# An�lise de Principais Componentes

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

# Seguindo o princ�pio de Pareto (80% dos resultados s�o gerado por 20% das causas) deve-se selecionar a quantidade de componentes que explique 80% dos dados. NO exemplo, 5 componentes.

fviz_eig(res.pca, addlabels = TRUE, xlab='Componentes', ylab = 'Percentual da vari�ncia explicada', ylim = c(0, 50))

# Investigando cada intera��o entre os componentes
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

# Plotando as combina��es entre todos os componentes
colors = c("#003262", "#FFFFFF", "#FF0000")
pal = colorRampPalette(colors)
grad_colors = pal(1000)
corrplot(res.pca$var$cos2, is.corr=FALSE, col=grad_colors)

# Contribui��o de cada componente
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10,
             title ="Contribui��o das vari�veis para o componente 1"
)

fviz_contrib(res.pca, choice = "var", axes = 2, top = 10,
             title ="Contribui��o das vari�veis o componente 2"
)

fviz_contrib(res.pca, choice = "var", axes = 3, top = 10,
             title ="Contribui��o das vari�veis o componente 3"
)
