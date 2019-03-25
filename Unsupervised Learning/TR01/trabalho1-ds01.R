########### Trabalho 1 - Módulo 2  #############################################
## Individual [   ]          Dupla [   ]    
## Aluno 1: 
	# 
## Aluno 2 (deixar em branco caso seja individual): 
################################################################################


#### Atividade 1 - base de dados: telecom-churn.csv

## Item 1
# Separação dos dados numéricos
get_dados_numericos <- function(caminho) {}

## Item 2
# Informações de Características
print_info <- function(data_frame) {}

## Item 3
# Relações entre Características
print_relacao <- function(data_frame) {}

################################################################################
#### Atividade 2 - base de dados: bakery.csv ###################################
################################################################################
#
#####----------------------------------------------------------------------#####
#
# Preparation
#
#####
#
# Libraries
library(dplyr)
library(arules)
library(arulesViz)
#
# Carregar arquivos desejado:
caminho <- file.choose()
#
#####----------------------------------------------------------------------#####
#
## Item 1
#  Análise de Frequência
#
#####
get_histo_top20 <- function(caminho) {
    switch(Sys.info()[['sysname']],
           Windows= {to_null = "NUL"},
           Linux  = {to_null = "/dev/null"},
           Darwin = {to_null = "/dev/null"})
    
    sink(to_null)
    transacoes <- read.transactions(caminho, 
                                      format="basket", 
                                      sep=",", 
                                      rm.duplicates = TRUE)
    itemFrequencyPlot(transacoes , topN=20, type="absolute")
    sin
    return(transacoes)
}
#
transacoes <- get_histo_top20(caminho)
#####----------------------------------------------------------------------#####
#
## Item 2
#  Mineração de Regras
#
#####
minerar_regras <- function(transacoes, 
                           supportes, 
                           confiancas,
                           num_transac = 2
                           ){
    switch(Sys.info()[['sysname']],
           Windows= {to_null = "NUL"},
           Linux  = {to_null = "/dev/null"},
           Darwin = {to_null = "/dev/null"})
    sink(to_null)
    
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
    sink()
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
#####----------------------------------------------------------------------#####
#
## Item 3
#  Determinação de Dependência Estatística
# 
#####
get_regras_interessantes <- function(caminho, 
                                     minsupp, 
                                     minconf,
                                     conviccoes=c(1.01,5),
                                     lift=c(1, Inf),
                                     razao=c(0, Inf)
                                     ) {
    switch(Sys.info()[['sysname']],
           Windows= {to_null = "NUL"},
           Linux  = {to_null = "/dev/null"},
           Darwin = {to_null = "/dev/null"})
    
    sink(to_null)
    
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
    sink()
    return (regras.filtered)
}

r <- get_regras_interessantes(caminho, 
                              minsupp=0.001, 
                              minconf=0.1)
print(r)
################################################################################







