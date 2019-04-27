source("GoT_aux.R") # auxiliary codes

#######################################
#    SETUP DATA                       #
#######################################
rawData <- read.csv("GoT_data.csv")

summary(rawData)

#Cleaning dataset 
data <- cleanDataset(rawData)
summary(data)

#Splitting into training and validation sets
idx <- sample(1:nrow(data), 0.8*nrow(data))

trainData <- data[idx,]
dim(trainData)

valData <- data[-idx,]
dim(valData)

#Check the no. of samples per class
table(trainData$deadOrAlive)
table(valData$deadOrAlive)



# formula used for training after removing features
formula <- as.formula("deadOrAlive ~ title + gender + culture +
                     house  + book1 +  book2 + book3 +  book4 +  book5 + 
                     popularity + isAliveMother +  isAliveFather +  isAliveHeir +
                     isAliveSpouse + isMarried +  isNoble +  numDeadRelations")



#######################################
#   Decision Tree  - Unbalanced       #
#######################################
treeModel <- rpart(formula = formula, 
                  data = trainData,
                  method="class",
                  control=rpart.control(minsplit=2, cp=0.0),
                  parms= list(split="information"))

# Predict model
prediction <- predict(treeModel, valData)
head(prediction)

unbalTreeResults <- getBalancedAcc(prediction[, "alive"], valData$deadOrAlive)
unbalTreeResults


#######################################
#   Decision Tree  - Undersampling    #
#######################################
set.seed(42)
aliveTrainData <- trainData[trainData$deadOrAlive == "alive", ]
deadTrainData <- trainData[trainData$deadOrAlive == "dead", ]

aliveIdx <- sample(1:nrow(aliveTrainData), nrow(deadTrainData), replace=FALSE)

#We will use all of the dead examples
balancedTrainData <- rbind(aliveTrainData[aliveIdx,], deadTrainData)
table(balancedTrainData$deadOrAlive)

#Training the DT
treeModel <- rpart(formula = formula, 
                   data = balancedTrainData,
                   method="class",
                   control=rpart.control(minsplit=2, cp=0.0),
                   parms= list(split="information"))

# Predict model
prediction <- predict(treeModel, valData)
head(prediction)

balTreeResults <- getBalancedAcc(prediction[, "alive"], valData$deadOrAlive)
balTreeResults





#######################################
#   Random Forest                     #
#######################################
#Error because the implementation of randomForest
#can't deal with categorical features with a lot of categories
rf <- randomForest(formula=formula, 
                   data= balancedTrainData, ntree=100)



# Random forest setup
targetAttribute <- "deadOrAlive"    #select the target variable
featureNames <- colnames(balancedTrainData[,2:(ncol(balancedTrainData)-1)])  #select the other features
featureNames

#Number of trees
ntrees <- 100

#Percentage of sampled features in each tree
percSampledFeatures <- 0.5  
# percSampledFeatures <- sqrt(length(featureNames)) / length(featureNames)

#Percentage of total data used in each tree
percTrainingSamples <- 0.9  

#Train our RandomForest
rf <- rpartRF(balancedTrainData, targetAttribute, featureNames, 
              percSampledFeatures, percTrainingSamples, ntrees)

#Predict the validation set
valPred <- c()
for (i in 1:ntrees) {
    valPred <- cbind(valPred, predict(rf[[i]], valData)[,"alive"])
}

#Mean probabilities
avgPred <- rowMeans(valPred)
getBalancedAcc(avgPred, valData$deadOrAlive)

#Majority Vote
majVote <- apply(valPred, 1, getMajVote)
majVoteBaggingResults <- getBalancedAcc(majVote, valData$deadOrAlive)
majVoteBaggingResults







#######################################
#   Boosting - Decision Trees         #
#######################################
nIterations <- 3
valPred <- c()

#define the weight of each sample
weights <- rep(1.0/nrow(balancedTrainData), nrow(balancedTrainData)) 
for (it in 1:nIterations) {
    #Train a DT
    boostingModel <- rpart(formula, balancedTrainData, weights = weights)
    
    #Predict the training set
    trainPred <- predict(boostingModel, balancedTrainData)[, "alive"]
    trainPred <- ifelse(trainPred >= 0.5, "alive", "dead")
    
    #Predict the validation set
    prediction <- predict(boostingModel, valData)
    alivePred <- prediction[,"alive"]
    
    #Store the predictions
    valPred <- cbind(valPred, alivePred)
    
    #Check which samples we got right/wrong
    missedSamples <- (trainPred != balancedTrainData$deadOrAlive)
    correctSamples <- (trainPred == balancedTrainData$deadOrAlive)
    
    #Update weights (simplified version)
    weights[missedSamples] <- 1.1 * weights[missedSamples]
    weights[correctSamples] <- 0.9 * weights[correctSamples]
}

#Mean of probabilities
avgPred <- rowMeans(valPred)
avgPred
avgBoostingResults <- getBalancedAcc(avgPred, valData$deadOrAlive)
avgBoostingResults


#Majority Vote
majVote <- apply(valPred, 1, getMajVote)
majVoteBoostingResults <- getBalancedAcc(majVote, valData$deadOrAlive)
majVoteBoostingResults




#######################################
#    Bagging                          #
#######################################
table(trainData$deadOrAlive)

#Split "alive" and "dead" samples
aliveTrainData <- trainData[trainData$deadOrAlive == "alive", ]
deadTrainData <- trainData[trainData$deadOrAlive == "dead", ]

set.seed(42)
nIterations <- 100
valPred <- c()
for (it in 1:nIterations) {
    #we will select a number of dead samples from [0.9 to 1.0] * nrow(deadTrainData)
    nsamples <- round(runif(1, min=0.9, max=1.0) * nrow(deadTrainData))
    #nsamples
    
    #Sample both sets
    aliveIdx <- sample(1:nrow(aliveTrainData), nsamples, replace=TRUE)
    deadIdx <- sample(1:nrow(deadTrainData), nsamples, replace=TRUE)
    
    #Combine them
    baggingTrainSet <- rbind(aliveTrainData[aliveIdx,], deadTrainData[deadIdx,])
    
    # table(baggingTrainSet$deadOrAlive)
    
    #Train the DT
    treeModel <- rpart(formula = formula, 
                      data = baggingTrainSet,
                      method="class",
                      control=rpart.control(minsplit=2, cp=0.0),
                      parms= list(split="information"))
    
    #Predict the validation set
    prediction <- predict(treeModel, valData)
    alivePred <- prediction[,"alive"]
    
    #Store the predictions
    valPred <- cbind(valPred, alivePred)
}

#One prediction for each classifier
valPred[1,]

#Mean of probabilities
avgPred <- rowMeans(valPred)
avgPred
avgBaggingResults <- getBalancedAcc(avgPred, valData$deadOrAlive)
avgBaggingResults


#Majority Vote
majVote <- apply(valPred, 1, getMajVote)
majVoteBaggingResults <- getBalancedAcc(majVote, valData$deadOrAlive)
majVoteBaggingResults





###### EXTRA - What we really want to know ######
#Will all the Starks die? 
starkNames <- c("Jon Snow", "Arya Stark", "Sansa Stark", "Bran Stark")
valData <- data[data$name %in% starkNames, 1:(ncol(data)-1)]
valData

#Run Bagging again...

avgPred <- rowMeans(valPred)
ifelse(avgPred>=0.5, "alive", "dead")