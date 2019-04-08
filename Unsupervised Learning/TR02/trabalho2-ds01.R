########### Trabalho 2 - Módulo 2 ###########
## Individual [   ]          Dupla [   ]    
## Aluno 1: 
# 
## Aluno 2 (deixar em branco caso seja individual): 
# 

#### Atividade 1 ###############################################################

############
## Item 1 ##
############

# Obtenção dos autovetores e autovalores
get_autovetores <- function(base, scale=TRUE) {
    pca <- prcomp(base, scale.=scale)
    return(pca)
}

f <- file.choose()
frogs <- read.csv(f, stringsAsFactors = TRUE)
colnames(frogs) <- c(
    "MFCCs_1",  "MFCCs_2",  "MFCCs_3",
    "MFCCs_4",  "MFCCs_5",  "MFCCs_6",
    "MFCCs_7",  "MFCCs_8",  "MFCCs_9",
    "MFCCs_10", "MFCCs_11", "MFCCs_12",
    "MFCCs_13", "MFCCs_14", "MFCCs_15",
    "MFCCs_16", "MFCCs_17", "MFCCs_18",
    "MFCCs_19", "MFCCs_20", "MFCCs_21",
    "MFCCs_22", "Family",   "Genus",
    "Species",  "RecordID")

sapply(frogs, class)

autovetores <- get_autovetores(frogs[1:22])
head(autovetores$x)

############
## Item 2 ##
############

# Escolha do número de dimensões para redução
get_numero_dimensoes <- function(autovetores, x) {
    cp <- cumsum(autovetores$sdev^2 / sum(autovetores$sdev^2))
    autovetores$x[,which(x >= cp)]
}

results <- data.frame()
K <- 0.90
autovetores.selected <- get_numero_dimensoes(autovetores, K)
k <- ncol(autovetores.selected)
head(autovetores.selected)
print(k)
results[as.character(paste0(K*100,"%")),"K"] <- k 

K <- 0.95
autovetores.selected <- get_numero_dimensoes(autovetores, K)
k <- ncol(autovetores.selected)
head(autovetores.selected)
print(k)
results[as.character(paste0(K*100,"%")),"K"] <- k 

K <- 0.99
autovetores.selected <- get_numero_dimensoes(autovetores, K)
k <- ncol(autovetores.selected)
head(autovetores.selected)
print(k)
results[as.character(paste0(K*100,"%")),"K"] <- k 

print(results)

###################
#     #     K     #   
###################
# 90% #           # 
# 95% #           # 
# 99% #           #
###################

#### Atividade 2 ###############################################################

##############
## Função 1 ##
##############
grafico_pca <- function(base, col = 1, scale = TRUE){
    pca <- prcomp(base, scale.=scale)
    plot(pca$x[,1:2], col=col, xlab="Dimensao 1", ylab="Dimensao 2")
}

grafico_pca(frogs[,1:22], col=frogs$Family)

##############
## Função 2 ##
##############
grafico_tsne <- function(base, label=1){
    library(readr)
    library(Rtsne)
    set.seed (1)
    tsne <- Rtsne(base, dims = 2, 
                  perplexity = 30, 
                  verbose = TRUE, 
                  max_iter = 500)
    colors <- rainbow(length(unique(label)))
    names(colors) <- unique(label)
    plot(tsne$Y, t='n', main="", xlab="Dimensao 1", ylab="Dimensao 2")
    text(tsne$Y, labels=label, col=colors[label], cex =0.5)
}

grafico_tsne(frogs[,1:22], label=frogs$Family)

### Melhor projeção:
### Motivo: