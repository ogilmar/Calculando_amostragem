## 5. AN�LISE DE CORRESPOND�NCIA M�LTIPLA (MCA)

#Descri��o: Usada para dados categ�ricos, identifica padr�es e agrupamentos.
#Vantagens: Adequada para vari�veis categ�ricas.
#Desvantagens: Menos comum e pode ser complexa.

# Instalando pacotes

# install.packages("FactoMineR", "gglot2", "readxl")
# install.packages("devtools")
# devtools::install_github("kassambara/factoextra")
rm(list=ls(all=TRUE))
library("ggplot2")
library("FactoMineR")
library("factoextra")
library("readxl")
library("gplots")
library("corrplot")
library("graphics")
library("foreign")
library("readxl")
library("amap")

# ler os dados
investidor <- read_excel("perfil investidor x aplicacao x estadocivil.xlsx")
investidor 

# Codifica��o bin�ria e matriz de BURT

matbinaria <- matlogic(dados)
matbinaria

dados.burt <-burt(dados)
dados.burt

# MCA with function dudi.acm
library(ade4)
# apply dudi.acm
mca1 <- dudi.acm(dados, scannf = FALSE)
# eig   eigenvalues, a vector with min(n,p) components
# c1    principal axes, data frame with p rows and nf columns
# co    column coordinates, data frame with p rows and nf columns
summary(mca1)

#coordenadas principais (BURT)
round(mca1$co, 3)

# coordenadas padr�o (geradas com a matriz bin�ria)
round(mca1$c1, 3)

# number of categories per variable
cats = apply(dados, 2, function(x) nlevels(as.factor(x)))
# data frame for ggplot
mca1_vars_df = data.frame(mca1$co, Variable = rep(names(cats), cats))
# plot
ggplot(data = mca1_vars_df, 
       aes(x = Comp1, y = Comp2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour = Variable)) +
  ggtitle("MCA plot of variables")