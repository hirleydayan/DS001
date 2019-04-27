#################################
# MDC - Machine Learning		#
# Linear Regression with GD 	#
# Abalone						#
#################################

#Set seed
set.seed(42)


#############################
#### Define MAE function ####
getMAE <- function(pred, true){
    MAE <- sum(abs(pred - true)) / length(pred)
    return(MAE)
}
#############################


# Load the data
data <- read.csv("abalone.data", header=TRUE) 


########### Train / Val splits ###########
# 80% train / 20% Val
randomTrainIndexes <- sample(1:nrow(data), size=0.8*nrow(data))
trainData <- data[randomTrainIndexes,]
valData <- data[-randomTrainIndexes,]


################### Min-Max Normalization ###################
maxTrainFeatures <- apply(trainData[,2:8], 2, max) #max of each feature
minTrainFeatures <- apply(trainData[,2:8], 2, min) #min of each feature
minMaxDiffTrain <- (maxTrainFeatures - minTrainFeatures)

trainData[,2:8] <- sweep(trainData[,2:8], 2, minTrainFeatures, "-")
trainData[,2:8] <- sweep(trainData[,2:8], 2, minMaxDiffTrain, "/")

valData[,2:8] <- sweep(valData[,2:8], 2, minTrainFeatures, "-")
valData[,2:8] <- sweep(valData[,2:8], 2, minMaxDiffTrain, "/")



#####################################################
# Gradient Descent                                  #
#####################################################
install.packages("gradDescent")
library("gradDescent")

#alpha is the learning rate
#it expects that the last column is the target value (rings)
gd <- gradDescentR.learn(trainData[,2:ncol(trainData)], learningMethod = "GD",
                           featureScaling = FALSE, seed = 42,
                           control = list(alpha = 0.1, maxIter=1000))
gd$model

# gd <- gradDescentR.learn(trainData[,2:9], learningMethod = "GD",
#                         featureScaling = TRUE, scalingMethod="MINMAX", 
#                         seed = 42, control = list(alpha = 0.1, maxIter=1000))


#NOTE: for some reason, this predict method expects a matrix with the 
# same number of rows as trained data (trainData[,2:ncol(trainData)]). 
# I will add a column of '1's  in the last row, but it won't be used
vSamples <- cbind(valData[, 2:(ncol(valData)-1) ], 1)

valPred <- predict(gd, vSamples)
valPred <- valPred[,ncol(valPred)]



# MAE
MAE <- getMAE(valPred, valData$rings)
MAE




##################################################
########## Testing number of iterations ##########
##################################################
iterations <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15) 
# iterations <- c(10, 50, 100, 500, 1000, 5000, 10000)


MAE_train_array <- c(length(iterations))
MAE_val_array <- c(length(iterations))


i <- 1
for (it in iterations) {
    gd <- gradDescentR.learn(trainData[,2:ncol(trainData)], learningMethod = "GD",
                            featureScaling = FALSE, seed = 42,
                            control = list(alpha = 0.1, maxIter=it))
    
    trainPred <- predict(gd, trainData[,2:ncol(trainData)])
    trainPred <- trainPred[,ncol(trainPred)]
    MAE_train_array[i] <- getMAE(trainPred, trainData$rings)
    
    valPred <- predict(gd, vSamples)
    valPred <- valPred[,ncol(valPred)]
    MAE_val_array[i] <- getMAE(valPred, valData$rings)
    i <- i+1
}

#Ploting MAE of train and validation
plot(MAE_train_array, type="o", col="blue", 
     ylim=c(min(min(MAE_train_array), min(MAE_val_array)),
            max(max(MAE_train_array), max(MAE_val_array))))
points(MAE_val_array, col="red", pch="*")
lines(MAE_val_array, col="red",lty=2)





###########################################
########## Testing learning rate ##########
###########################################
learningRates <- c(0.00001, 0.0001, 0.001, 0.01, 0.1) #, 1.0)

MAE_train_array <- c(length(learningRates))
MAE_val_array <- c(length(learningRates))


i <- 1
for (lr in learningRates) {
    gd <- gradDescentR.learn(trainData[,2:ncol(trainData)], learningMethod = "GD",
                            featureScaling = FALSE, seed = 42,
                            control = list(alpha = lr, maxIter=1000))
    
    trainPred <- predict(gd, trainData[,2:ncol(trainData)])
    trainPred <- trainPred[,ncol(trainPred)]
    MAE_train_array[i] <- getMAE(trainPred, trainData$rings)
    
    valPred <- predict(gd, vSamples)
    valPred <- valPred[,ncol(valPred)]
    MAE_val_array[i] <- getMAE(valPred, valData$rings)
    i <- i+1
}

#Ploting MAE of train and validation
plot(learningRates, MAE_train_array, type="o", col="blue", log="x", 
     ylim=c(min(min(MAE_train_array), min(MAE_val_array)),
            max(max(MAE_train_array), max(MAE_val_array))))
points(learningRates, MAE_val_array, col="red", pch="*")
lines(learningRates, MAE_val_array, col="red",lty=2)
