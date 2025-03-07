# An�lise de Cluster (An�lise de Agrupamentos)

##Descri��o: Utiliza algoritmos de clustering (como K-means) para agrupar indiv�duos com base em caracter�sticas semelhantes.
##Vantagens: Identifica grupos naturais na popula��o.
##Desvantagens: Requer software estat�stico e pode ser complexo.

#Carregamento dos dados
data("dados")
df=scale(dados)
head(df, n=3)

# N�mero �timo de clusters
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

# Clusteriza��o k-means
set.seed(123)
km.res=kmeans(df, 4, nstart=25)
print(km.res)

aggregate(dados, by=list(cluster=km.res$cluster), mean)

mtcars2=cbind(dados, cluster=km.res$cluster)
head(dados)

km.res$centers

# Vizualizando os clusters

library(ggplot2)
library(factoextra)

fviz_cluster(km.res, data=dados,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)
