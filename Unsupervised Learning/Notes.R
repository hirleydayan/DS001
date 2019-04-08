# Aula 12/03/2019 ##############################################################
library(caret)
measured <- as.factor(c(0,1,1,0,1,1,1,0,0,1,1)) 
calculated <- as.factor(c(0,0,1,1,1,1,0,1,1,1,0))
confusionMatrix(calculated, measured)

# Aula 19/03/2019 ##############################################################
library(arules)
transactions <- read.transactions(file.choose(), format="basket", sep=",")
inspect(transactions)
image(transacoes)
rules <- apriori(transactions, parameter = list(supp=0.5,conf=0.5))
rules
inspect(rules)


# install.packages("arulesViz")
# install.packages("git2r")
# install.packages("mclust")
# install.packages("dendextend")
# install.packages('robustbase')
library(dendextend)
library(arulesViz)
plot(rules, method="graph",control=list(type="items"))
data(Groceries)
transacoes <- Groceries
summary(transacoes)
itemFrequencyPlot(Groceries , topN=20, type="absolute")
itemFrequencyPlot(transacoes, support=0.1, cex.names=0.8)
itemFrequencyPlot(transacoes, support=0.05, cex.names=0.8)
regras <- apriori(Groceries , parameter=list(supp=0.001, conf=0.8))
inspect(regras[1:5])
regras
summary(regras)
regras <- sort(regras , by="confidence", decreasing=TRUE)
inspect(regras[1:5])
regras <- apriori(data=Groceries , 
                  parameter=list(supp=0.001, conf=0.08), 
                  appearance=list(default="lhs",
                                  rhs="whole milk"),
                  control=list(verbose=F))
regras <- sort(regras, decreasing=TRUE, by="confidence")
inspect(regras[1:5])
regras <- apriori(data=Groceries ,
                  parameter=list(supp=0.001, conf=0.15, minlen=2),
                  appearance=list(default="rhs",
                                  lhs="whole milk"),
                  control=list(verbose=F))
regras <- sort(regras, decreasing=TRUE, by="confidence")
inspect(regras[1:5])
plot(regras , method="graph", control=list(type="items"))
regras <- apriori(Groceries ,
                  parameter=list(supp=0.009, conf =0.25 , minlen=2))
regras
inspect(head(sort(regras, by="lift"), 5))
inspect(sort(sort(regras , by="support"), by="confidence")[1:5])
plot(regras , measure=c("support", "confidence"), shading="lift", interactive=FALSE)
meat.regras <- sort(subset(regras,
                           subset=lhs %in% "beef" |
                               lhs %in% "sausage" |
                               lhs %in% "chicken"), by="confidence")
summary(meat.regras)
inspect(meat.regras)
plot(meat.regras , method="graph", 
     interactive=FALSE, shading="lift")
milk.regras <- sort(subset(regras, subset = rhs %in% "whole milk"),
                    by = "confidence")
summary(milk.regras)
inspect(milk.regras)
coke.regras <- sort(subset(regras, 
                           subset = rhs %in% "soda"),
                    by = "confidence")
summary(coke.regras) 
inspect(coke.regras)
yogurt.regras <- sort(subset(regras,
                             subset = lhs %in% "yogurt"), 
                      by = "confidence")
summary(yogurt.regras)
inspect(yogurt.regras)

library(colorspace)
plot(meat.regras , method="matrix",
     measure=c("support", "confidence"),
     control=list(col=sequential_hcl(200)))
plot(meat.regras , method="grouped",
     measure="support",
     control=list(col=sequential_hcl(100)))

# install.packages("treemap")
# install.packages("magrittr")
# install.packages("dplyr")
library(treemap)
library(magrittr)
library(dplyr)
occur1 <- transacoes@itemInfo %>% 
    group_by(level1) %>%
    summarize(n=n())
treemap(occur1 ,index=c("level1"), vSize="n",
        title="", palette="Dark2", border.col="#FFFFFF")

occur2 <- transacoes@itemInfo %>%
    group_by(level1 , level2) %>%
    summarize(n=n())
treemap(occur2 ,index=c("level1", "level2"), 
        vSize="n", title="", palette="Dark2", border.col="#FFFFFF")

occur3 <- transacoes@itemInfo %>% 
    group_by(level1 , level2 , labels) %>%
    summarize(n=n())
treemap(occur3 ,index=c("level1", "labels"), 
        vSize="n", title="", palette="Dark2", border.col="#FFFFFF")

inspect(tail(sort(regras, by="lift")))



library(arules)
library(arulesViz)
data("AdultUCI")
dim(AdultUCI)
head(AdultUCI)
AdultUCI[["age"]] <-ordered(cut(AdultUCI[["age"]], c(15,25,45,65,100)),
                            labels = c("Young", "Middle -aged", 
                                       "Senior", "Old"))

AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]],
                                            c(0,25,40,60,168)),
                                        labels = c("Part -time", "Full-time", 
                                                   "Over-time", "Workaholic"))

AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain"]],
                                          c(-Inf, 0, 
                                            median(AdultUCI[["capital-gain"]]
                                   [AdultUCI[["capital-gain"]] > 0]), 
                                   labels = c("None", "Low", "High"))))

                                   
# Aula 02/04/2019 ##############################################################

# Reducao de Dimensionalidade:
library(mlbench)
library(caret)
data(PimaIndiansDiabetes)
matriz_corr <- cor(PimaIndiansDiabetes[,1:8])
print(matriz_corr)
altamente_corr <- findCorrelation(matriz_corr, cutoff =0.5)
print(altamente_corr)

# Metodo de selcao de atributos:
# Solucao por Evoltorio (selecao de atributo com o uso de random forrest)
library(mlbench)
library(caret)
data(PimaIndiansDiabetes)

# para garantir repetibilidade dos resultados > set.seed(7)
set.seed(7)

# Recursive Feature Elimination (RFE)
controle <- rfeControl(functions=rfFuncs, method="cv", number=10)

resultados <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9],
                  sizes=c(1:8),
                  rfeControl=controle)
print(resultados)
predictors(resultados)
ggplot(resultados)

# PCA Analysis
library(datasets)
data(iris)
summary(iris)

# Using PrComp
iris.pca1 <- prcomp(iris[,1:4], scale.=TRUE)
iris.pca1

# Without scalling for comparison reasons:
iris.pca2 <- prcomp(iris[,1:4])
iris.pca2

summary(iris.pca1)

summary(iris.pca2)

# Using PrinComp: 
iris.pca3 <- princomp(iris[,1:4],cor=TRUE)
iris.pca3
summary(iris.pca3)

head(iris.pca1$x)
head(iris.pca1$x[ ,1:2])

z <- iris.pca1$x[ ,1:2] %*% t(iris.pca1$rotation [ ,1:2])
head(z)
summary(iris.pca1)

# Other way for calculating the contribution of each PCA:
pr_std_dev <- iris.pca1$sdev
pr_var <- pr_std_dev^2
pr_var

prop_varex <- pr_var/sum(pr_var)
prop_varex

plot(prop_varex, 
     xlab="Componentes Principais",
     ylab = "Propor¸c~ao de Vari^ancia Explicada", type = "b")

plot(cumsum(prop_varex),
     xlab = "Componentes Principais",
     ylab = "Vari^ancia Explicada Acumulada", type = "b")

biplot(iris.pca1, 
       xlab="Componente Principal 1",
       ylab="Componente Principal 2", scale=0)

# Simple Ploting:
plot(iris[,1:2], col=iris[,5])

# PCA Ploting:
plot(iris.pca1$x[,1:2], col=iris [,5])

# Wine Dataset:
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")
colnames(wine) <- c("Cvs", "Alcohol", "Malic acid",
                        "Ash", "Alcalinity of ash",
                        "Magnesium", "Total phenols",
                        "Flavanoids",
                        "Nonflavanoid phenols",
                        "Proanthocyanins",
                        "Color intensity", "Hue",
                        "OD280/OD315 of diluted wines",
                        "Proline")
wine.classes <- factor(wine$Cvs)
summary(wine[,2:13])
wine.pca <- prcomp(wine[,2:13], scale.=TRUE)
wine.pca
summary(wine.pca)

head(wine.pca$x)
head(wine.pca$x[ ,1:7])

z <- wine.pca$x[ ,1:7] %*% t(wine.pca1$rotation [ ,1:7])
head(z)

pr_std_dev <- wine.pca$sdev
pr_var <- pr_std_dev^2
pr_var

prop_varex <- pr_var/sum(pr_var)
prop_varex

plot(prop_varex, 
     xlab="Componentes Principais",
     ylab = "Propor¸c~ao de Vari^ancia Explicada", type = "b")

plot(cumsum(prop_varex),
     xlab = "Componentes Principais",
     ylab = "Variancia Explicada Acumulada", type = "b")

biplot(wine.pca, 
       xlab="Componente Principal 1",
       ylab="Componente Principal 2", scale=0)

# Car Dataset:
cars.pca <- prcomp(datasets::cars , scale.=TRUE)
summary(cars.pca)


# t-SNE example:
library(Rtsne)
library(datasets)

data(iris)

# t-SNE does not work with duplicated lines, so:
iris_unicos <- unique(iris)

set.seed (42)

# Perplexity: 
# A variancia da distribuicao normal utilizada para calcular as similaridades 
# no espaco de maior dimensao e definida a partir de um hiper-parametro da 
# tecnica, chamado de perplexidade (valor definido pelo usuario), que pode ser 
# interpretado pela quantidade de vizinhos muito proximos que cada ponto tem
tsne <- Rtsne(as.matrix(iris_unicos [ ,1:4]), perplexity = 30, dims =3)

plot(tsne$Y, col=iris_unicos$Species,
     xlab="dimens~ao 1", ylab="dimens~ao 2", pch =16)

# Other dataset:
library(readr)
library(Rtsne)

train <- read_csv("https://www.ic.unicamp.br/~helio/MNIST/train.csv")

train$label <- as.factor(train$label)

set.seed (1)

tsne <- Rtsne(train[,-1], dims = 2, perplexity = 30, verbose = TRUE , max_iter = 500)
colors <- rainbow(length(unique(train$label)))
names(colors) <- unique(train$label)
plot(tsne$Y, t='n', main="", xlab="dimensao 1", ylab="dimensao 2")
text(tsne$Y, labels=train$label, col=colors[train$label], cex =0.5)


# References

# https://distill.pub/2016/misread-tsne/