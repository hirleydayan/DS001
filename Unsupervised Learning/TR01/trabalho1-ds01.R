########### Trabalho 1 - Modulo 2  ###########
## Individual [   ]          Dupla [ X ]    
## Aluno 1: Marcia Maria Parmigiani Martins
	# 
## Aluno 2: Hirley Dayan Lourenço da Silva 
	# 
#install.packages("e1071")
#install.packages("corrplot")
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)
knitr::kable
library(knitr)
library(e1071)
library(arules)
library(arulesViz)
library(dplyr)
library(corrplot)

#### Atividade 1 - base de dados: telecom-churn.csv

## Item 1
# Separacao dos dados numericos
caminho <- file.choose()
get_dados_numericos <- function(caminho) {
  telecom_churn <- read.csv(caminho, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  df <- telecom_churn[, unlist(lapply(telecom_churn, is.numeric))]
  row.has.na <- apply(df, 1, function(x){any(is.na(x))})
  df.filtered <- df[!row.has.na, ]
  return(df.filtered)
}

telecom_churn_num <- get_dados_numericos(caminho); telecom_churn_num

## Item 2
# Informacoes de Caracteriticas
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
  }

print_info <- function(data_frame) {
  v1 <- sapply(data_frame, mean)
  v2 <- sapply(data_frame, var)
  v3 <- sapply(data_frame, e1071::moment)
  v4 <- sapply(data_frame, e1071::skewness)
  v5 <- sapply(data_frame, e1071::kurtosis)
  v6 <- sapply(data_frame, min)
  v7 <- sapply(data_frame, max)
  v8 <- sapply(data_frame, median)
  v9 <- sapply(data_frame, moda)
  
  print(kable(v1, col.names = "Media"))
  print(kable(v2, col.names = "Variancia"))
  print(kable(v3, col.names = "Momento"))
  print(kable(v4, col.names = "Assimetria"))
  print(kable(v5, col.names = "Curtose"))
  print(kable(v6, col.names = "Min"))
  print(kable(v7, col.names = "Max"))
  print(kable(v8, col.names = "Mediana"))
  print(kable(v9, col.names = "Moda"))
}

print_info(telecom_churn_num)

## Item 3
# Relacoes entre Caracteristicas
print_relacao <- function(data_frame) {
  dataframe.subset <- data_frame[,2:length(data_frame)]
  print(round(cor(dataframe.subset, data_frame), 2))
  print(round(cov(dataframe.subset, data_frame), 2))
  corrplot(cor(data_frame), method="circle")
}

print_relacao(telecom_churn_num)

#### Atividade 2 - base de dados: bakery.csv

## Item 1
# Análise de Frequência

caminho <- file.choose()
get_histo_top20 <- function(caminho) {

  transacoes <- read.transactions(caminho, 
                                  format="basket", 
                                  sep=",", 
                                  rm.duplicates = TRUE)
  itemFrequencyPlot(transacoes , topN=20, type="absolute")
  return(transacoes)
}

transacoes <- get_histo_top20(caminho)

## Item 2
# Mineração de Regras
################################################################################################################
#   # confiança # suporte #        regras de associação                                                        #    
################################################################################################################
# 1 #     0.5   #    0.1  # {} => {Coffee} e {Cake} => {Coffee}                                                # 
# 2 #     0.5   #   0.01  # {Cake,Sandwich} => {Coffee} e {Toast} => {Coffee}                                  # 
# 3 #     0.8   #  0.001  # {Extra Salami or Feta,Juice} => {Salad} e {Extra Salami or Feta,Sandwich} =>{Salad}# 
# 4 #     0.67  #  0.001  # {Postcard} => {Tshirt} e  {Extra Salami or Feta,Juice} =>{Salad}                   # 
# 5 #     0.5   #  0.001  # {Postcard} => {Tshirt} e {Coffee,Juice,Salad} => {Extra Salami or Feta}            # 
################################################################################################################
minerar_regras <- function(transacoes, 
                           supportes, 
                           confiancas,
                           num_transac = 2
){

  regras.cross <- NULL
  for (s in supportes){
    for (c in confiancas){
      regras <- apriori(transacoes, parameter=list(supp=s, conf=c))
      regras.inspect <- inspect(regras)
      if (!is.null(regras.inspect)){
        regras.inspect <- regras.inspect[order(regras.inspect$lift,
                                               decreasing = TRUE),]
        regras.inspect <- regras.inspect[1:ifelse(
          nrow(regras.inspect)>num_transac,
          num_transac,nrow(regras.inspect)),]
        regras.inspect[,"min support"] <- s
        regras.inspect[,"min confidence"] <- c
        if (is.null(regras.cross)){
          regras.cross <- regras.inspect
        } else {
          regras.cross <- rbind(regras.cross, regras.inspect)
        }
        rownames(regras.cross)<-NULL
      }
    }
  }
  if(is.null(regras.cross)){
    return(regras.cross)
  }else{
    return(regras.cross[order(regras.cross$lift,
                              decreasing = TRUE),])
  }
}
regras <- minerar_regras(transacoes, 
                         supportes=c(0.001, 0.01, 0.1),
                         confiancas=c(0.8, 0.67, 0.5))
print(regras)

## Item 3
# Determinação de Dependência Estatística
get_regras_interessantes <- function(caminho, 
                                     minsupp, 
                                     minconf,
                                     conviccoes=c(1.01,5),
                                     lift=c(1, Inf),
                                     razao=c(0, Inf)
) {

  transacoes <- read.transactions(caminho, 
                                  format="basket", 
                                  sep=",", 
                                  rm.duplicates = TRUE)
  
  regras <- apriori(transacoes, 
                    parameter=list(supp=minsupp, conf=minconf))
  
  regras.inspect <- inspect(regras)
  
  regras.interest <- interestMeasure(regras, 
                                     c("conviction", "oddsRatio"), 
                                     transactions = transacoes)
  
  regras.inspect <- cbind(regras.inspect, regras.interest)
  regras.inspect <- regras.inspect %>% select(-count, count)
  
  regras.filtered <- regras.inspect[ which( 
    (regras.inspect$conviction >= min(conviccoes) & 
       regras.inspect$conviction <= max(conviccoes)) & 
      (regras.inspect$lift >= min(lift) & 
         regras.inspect$lift <= max(lift)) &
      (regras.inspect$oddsRatio >= min(razao) & 
         regras.inspect$oddsRatio <= max(razao))) , ]
  return (regras.filtered)
}

r <- get_regras_interessantes(caminho, 
                              minsupp=0.001, 
                              minconf=0.1)
print(r)