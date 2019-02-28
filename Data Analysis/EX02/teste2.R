########################################
# Teste 2         
# Nome(s):HIRLEY DAYAN LOURENCO DA SILVA 
########################################

## 1 - Agrupamento

groupsum <- function(df, colgroup, colsum){
    df[, colgroup] <- as.factor(df[, colgroup])    #Just to make sure we have factors
    t <- tapply(df[,colsum], df[,colgroup], sum)
    df <- data.frame(names(t),t)
    names(df) <- c(colgroup, colsum)
    rownames(df) <- NULL
    return(df)
}

##### Exemplos no PDF:
dia <- c(01, 01, 02, 02, 03, 03, 04, 04, 05, 05)
cidade <- c('Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas')
chuva <- c(0.15, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45)
chuvas <- data.frame(cidade, dia, chuva)
groupsum(chuvas, "cidade", "chuva")
customer_churn <- read.table("customer_churn.csv", sep=",", header = TRUE, stringsAsFactors = FALSE)
groupsum(customer_churn, "Contract", "MonthlyCharges")

## 2 - Binario para Decimal

# mask <- function(n){
#     r <- numeric(0)
#     for(i in c(n:1)){
#         r[length(r) + 1] <- 2^(i-1)
#     }
#     return(r)
# }
# 
# binToDec <- function(...){
#     decs <- numeric(0)
#     for(b in bins){
#         # mask <- sapply(c(length(b):1),function(x){2^(x-1)})
#         decs[length(decs) + 1] <- sum(b * mask(length(b)))
#     }
#     return(decs)
# } 

binToDec <- function(...){
    as.numeric(lapply(list(...), function(b){sum(b * sapply(c(length(b):1),function(x){2^(x-1)}))}))
} 

##### Exemplos no PDF:
binToDec(c(1, 0))
binToDec(c(0, 0, 1), c(1, 1))
binToDec(rep(1, 3), rep(0, 2), rep(c(1,0), 2))

## 3 - Ocorrencia de palavras

wordCount <- function(word, text){
    # text <- gsub("[[:punct:]]", "", tolower(text))
    text <- gsub("[.,!?]", "", tolower(text))
    words <- strsplit(tolower(text), split = " ")[[1]]
    if (length(words)>0){
        return(sum(is.element(words, word)))
    }
    return(0)
}

##### Exemplos no PDF:
text <- "O rAto roeu a roupa do Rei de Roma! RainhA raivosa rasgou o resto."
wordCount("rato", text)
wordCount("roma", text)
text <- "A vaca malHada foi molhADA por outra VACA, MOLhada e MALhaDa."
wordCount("outra", text)
wordCount("vaca", text)
wordCount("malhada", text)
text <- "Se a liga me ligasse, eu tambem ligava a liga. Mas a liga nao me liga, eu tambem nao ligo a liga."
wordCount("liga", text)
wordCount("ligasse", text)

## 4 - Ordenacao de panquecas

giro <- function(v,i){
    if(i<=length(v)){
        v[length(v):i] <- v[i:length(v)]
    }
    return(v)
}

ordenar <- function(v){
    r <- v
    v.sorted <- sort(v, decreasing = TRUE)
    while(all(r==v.sorted)!=TRUE){
        x <- as.numeric(r!=v.sorted) * 1:length(v)
        i <- ifelse(sample(1:10,1) %% 2 == 0, min(x[x!=0]), max(x[x!=max(x)]))
        r <- giro(r,i)
        print(r)
    }
    return(r)
}

##### Exemplos no PDF:
panquecas <- c(3,4,1,2)
giro(panquecas, 2)
panquecas <- ordenar(panquecas)

