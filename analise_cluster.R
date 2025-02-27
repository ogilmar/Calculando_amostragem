# Análise de Cluster (Análise de Agrupamentos)

##Descrição: Utiliza algoritmos de clustering (como K-means) para agrupar indivíduos com base em características semelhantes.
##Vantagens: Identifica grupos naturais na população.
##Desvantagens: Requer software estatístico e pode ser complexo.

#Carregamento dos dados
data("dados")
df=scale(dados)
head(df, n=3)

# Número ótimo de clusters
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

# Clusterização k-means
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
