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
                            labels = c("Young", "Middle -aged", "Senior", "Old"))

AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]],
                                            c(0,25,40,60,168)),
                                        labels = c("Part -time", "Full-time", "Over-time", "Workaholic"))

AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain"]],
                                          c(-Inf, 0, median(AdultUCI[["capital-gain"]]
                                   [AdultUCI[["capital-gain"]] > 0]), 
                                   labels = c("None", "Low", "High"))