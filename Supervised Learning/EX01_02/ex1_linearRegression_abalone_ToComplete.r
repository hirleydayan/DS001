#################################
# MDC - Machine Learning		#
# Linear Regression				#
# Abalone						#
#################################

#Set seed
set.seed(42)

# Load the data
data <- read.csv("abalone.data", header=TRUE) 


############ Inspect the Data ############
# What is the data like? (Also, take a look at "abalone.names")
# - Is the data numeric or categorical? 
summary(data)

# - How many examples do we have?
dim(data) #4177 samples with 8 features + rings information



########### Train / Val splits ###########
# 80% train / 20% Val
randomTrainIndexes <- sample(1:nrow(data), size=0.8*nrow(data))
trainData <- data[randomTrainIndexes,]
valData <- data[-randomTrainIndexes,]


# Inspect trainData
any(is.na(trainData))
cor(trainData[,2:8]) #removing sex & rings


# Let's plot some of the features

#### Plot 01
plot(trainData[,"length"], ylab="value", ylim=c(0,30) )
points(trainData[,"whole_weight"],  pch="*", col="red")
points(trainData[,"rings"], pch="+", col="green")

#### Plot 02
plot(trainData[,"length"], ylab="value", ylim=c(0,3) )
points(trainData[,"whole_weight"],  pch="*", col="red")



################### Normalization ###################
# We can either perform Z-Norm or Min-Max           #
#####################################################

# Z-Norm
# meanTrainFeatures <- colMeans(trainData[,2:8]) #mean of each feature
# stdTrainFeatures <- apply(trainData[,2:8], 2, sd) #std of each feature
# 
# meanTrainFeatures
# stdTrainFeatures
# 
# trainData[,2:8] <- sweep(trainData[,2:8], 2, meanTrainFeatures, "-")
# trainData[,2:8] <- sweep(trainData[,2:8], 2, stdTrainFeatures, "/")
# 
# valData[,2:8] <- sweep(valData[,2:8], 2, meanTrainFeatures, "-")
# valData[,2:8] <- sweep(valData[,2:8], 2, stdTrainFeatures, "/")
# 
# trainData[1:5,]
# summary(trainData)
# 
# plot(trainData[,"length"], ylab="value", ylim=c(0,5) )
# points(trainData[,"whole_weight"],  pch="*", col="red")


# Min-Max
maxTrainFeatures <- apply(trainData[,2:8], 2, max) #max of each feature
minTrainFeatures <- apply(trainData[,2:8], 2, min) #min of each feature

maxTrainFeatures
minTrainFeatures

minMaxDiffTrain <- (maxTrainFeatures - minTrainFeatures)
minMaxDiffTrain

trainData[,2:8] <- sweep(trainData[,2:8], 2, minTrainFeatures, "-")
trainData[,2:8] <- sweep(trainData[,2:8], 2, minMaxDiffTrain, "/")

valData[,2:8] <- sweep(valData[,2:8], 2, minTrainFeatures, "-")
valData[,2:8] <- sweep(valData[,2:8], 2, minMaxDiffTrain, "/")

trainData[1:5,]
summary(trainData)
plot(trainData[,"length"], ylab="value", ylim=c(0,1) )
points(trainData[,"whole_weight"],  pch="*", col="red")






######### TO COMPLETE ###############
## Let's train a linear regression
### ADD CODE

### SUMMARY MODEL

### MAKE PREDICTION








#############################
#### Define MAE function ####
getMAE <- function(pred, true){
    MAE <- sum(abs(pred - true)) / length(pred)
    return(MAE)
}
#############################
#### Define MSE function ####
getMSE <- function(pred, true){
    MSE <- sum((pred - true)**2) / length(pred)
    return(MSE)
}

###################################
#### Define R-squared function ####
getRSQ <- function(pred, true){
    rss <- sum((pred - true) ^ 2)
    tss <- sum((pred - mean(true)) ^ 2)
    RSQ <- 1 - rss/tss
    return(RSQ)
}





# MAE
MAE_simple <- getMAE(valPred, valData$rings)
MAE_simple

# MSE
MSE_simple <- getMSE(valPred, valData$rings)
MSE_simple

# R-squared
RSQ_simple <- getRSQ(valPred, valData$rings)
RSQ_simple




######### TO COMPLETE ###############
## Adding complexity to the model

##### ADD CODE

#### PREDICT VALDATA





# MAE
MAE_complex <- getMAE(valPred, valData$rings)
MAE_complex

# MSE
MSE_complex <- getMSE(valPred, valData$rings)
MSE_complex

# R-squared
RSQ_complex <- getRSQ(valPred, valData$rings)
RSQ_complex





###########################################################
###########################################################
###########################################################
####################### EXTRAS ############################

# 1) How would you add the "Sex" feature to your model?
# Add it and see if it improves. 

trainData$male <- as.numeric(trainData$sex == "M")
trainData$female <- as.numeric(trainData$sex == "F")
trainData$infant <- as.numeric(trainData$sex == "I")
trainData$sex <- NULL

valData$male <- as.numeric(valData$sex == "M")
valData$female <- as.numeric(valData$sex == "F")
valData$infant <- as.numeric(valData$sex == "I")
valData$sex <- NULL


model <- lm(formula = rings ~ .,
            data=trainData)

valPred <- predict(model, valData)
MAE <- getMAE(valPred, valData$rings)
MAE




###########################################################
###########################################################
###########################################################
##################### EXTRAS 2 ############################
#MAE x Polynomial degree

f1 <- formula(rings ~ length + diameter + height + shell_weight +
                 whole_weight + shucked_weight + viscera_weight)

f2 <- formula(rings ~ length + diameter + height + shell_weight +
                 whole_weight + shucked_weight + viscera_weight + 
                 I(length^2) + I(diameter^2) +  I(height^2) +
                 I(shell_weight^2) + I(whole_weight^2) +  
                 I(shucked_weight^2) + I(viscera_weight^2))

f3 <- formula(rings ~ length + diameter + height + shell_weight +
                 whole_weight + shucked_weight + viscera_weight + 
                 I(length^2) + I(diameter^2) +  I(height^2) +
                 I(shell_weight^2) + I(whole_weight^2) +  
                 I(shucked_weight^2) + I(viscera_weight^2) + 
                 I(length^3) + I(diameter^3) +  I(height^3) +
                 I(shell_weight^3) + I(whole_weight^3) +  
                 I(shucked_weight^3) + I(viscera_weight^3))

f4 <- formula(rings ~ length + diameter + height + shell_weight +
                 whole_weight + shucked_weight + viscera_weight + 
                 I(length^2) + I(diameter^2) +  I(height^2) +
                 I(shell_weight^2) + I(whole_weight^2) +  
                 I(shucked_weight^2) + I(viscera_weight^2) + 
                 I(length^3) + I(diameter^3) +  I(height^3) +
                 I(shell_weight^3) + I(whole_weight^3) +  
                 I(shucked_weight^3) + I(viscera_weight^3)+ 
                 I(length^4) + I(diameter^4) +  I(height^4) +
                 I(shell_weight^4) + I(whole_weight^4) +  
                 I(shucked_weight^4) + I(viscera_weight^4))


f5 <- formula(rings ~ length + diameter + height + shell_weight +
                 whole_weight + shucked_weight + viscera_weight + 
                 I(length^2) + I(diameter^2) +  I(height^2) +
                 I(shell_weight^2) + I(whole_weight^2) +  
                 I(shucked_weight^2) + I(viscera_weight^2) + 
                 I(length^3) + I(diameter^3) +  I(height^3) +
                 I(shell_weight^3) + I(whole_weight^3) +  
                 I(shucked_weight^3) + I(viscera_weight^3)+ 
                 I(length^4) + I(diameter^4) +  I(height^4) +
                 I(shell_weight^4) + I(whole_weight^4) +  
                 I(shucked_weight^4) + I(viscera_weight^4)+ 
                 I(length^5) + I(diameter^5) +  I(height^5) +
                 I(shell_weight^5) + I(whole_weight^5) +  
                 I(shucked_weight^5) + I(viscera_weight^5))


f6 <- formula(rings ~ length + diameter + height + shell_weight +
                 whole_weight + shucked_weight + viscera_weight + 
                 I(length^2) + I(diameter^2) +  I(height^2) +
                 I(shell_weight^2) + I(whole_weight^2) +  
                 I(shucked_weight^2) + I(viscera_weight^2) + 
                 I(length^3) + I(diameter^3) +  I(height^3) +
                 I(shell_weight^3) + I(whole_weight^3) +  
                 I(shucked_weight^3) + I(viscera_weight^3)+ 
                 I(length^4) + I(diameter^4) +  I(height^4) +
                 I(shell_weight^4) + I(whole_weight^4) +  
                 I(shucked_weight^4) + I(viscera_weight^4)+ 
                 I(length^5) + I(diameter^5) +  I(height^5) +
                 I(shell_weight^5) + I(whole_weight^5) +  
                 I(shucked_weight^5) + I(viscera_weight^5)+ 
                 I(length^6) + I(diameter^6) +  I(height^6) +
                 I(shell_weight^6) + I(whole_weight^6) +  
                 I(shucked_weight^6) + I(viscera_weight^6))

f7 <- formula(rings ~ length + diameter + height + shell_weight +
                 whole_weight + shucked_weight + viscera_weight + 
                 I(length^2) + I(diameter^2) +  I(height^2) +
                 I(shell_weight^2) + I(whole_weight^2) +  
                 I(shucked_weight^2) + I(viscera_weight^2) + 
                 I(length^3) + I(diameter^3) +  I(height^3) +
                 I(shell_weight^3) + I(whole_weight^3) +  
                 I(shucked_weight^3) + I(viscera_weight^3)+ 
                 I(length^4) + I(diameter^4) +  I(height^4) +
                 I(shell_weight^4) + I(whole_weight^4) +  
                 I(shucked_weight^4) + I(viscera_weight^4)+ 
                 I(length^5) + I(diameter^5) +  I(height^5) +
                 I(shell_weight^5) + I(whole_weight^5) +  
                 I(shucked_weight^5) + I(viscera_weight^5)+ 
                 I(length^6) + I(diameter^6) +  I(height^6) +
                 I(shell_weight^6) + I(whole_weight^6) +  
                 I(shucked_weight^6) + I(viscera_weight^6) + 
                 I(length^7) + I(diameter^7) +  I(height^7) +
                 I(shell_weight^7) + I(whole_weight^7) +  
                 I(shucked_weight^7) + I(viscera_weight^7))

f8 <- formula(rings ~ length + diameter + height + shell_weight +
                 whole_weight + shucked_weight + viscera_weight + 
                 I(length^2) + I(diameter^2) +  I(height^2) +
                 I(shell_weight^2) + I(whole_weight^2) +  
                 I(shucked_weight^2) + I(viscera_weight^2) + 
                 I(length^3) + I(diameter^3) +  I(height^3) +
                 I(shell_weight^3) + I(whole_weight^3) +  
                 I(shucked_weight^3) + I(viscera_weight^3)+ 
                 I(length^4) + I(diameter^4) +  I(height^4) +
                 I(shell_weight^4) + I(whole_weight^4) +  
                 I(shucked_weight^4) + I(viscera_weight^4)+ 
                 I(length^5) + I(diameter^5) +  I(height^5) +
                 I(shell_weight^5) + I(whole_weight^5) +  
                 I(shucked_weight^5) + I(viscera_weight^5)+ 
                 I(length^6) + I(diameter^6) +  I(height^6) +
                 I(shell_weight^6) + I(whole_weight^6) +  
                 I(shucked_weight^6) + I(viscera_weight^6)+ 
                 I(length^7) + I(diameter^7) +  I(height^7) +
                 I(shell_weight^7) + I(whole_weight^7) +  
                 I(shucked_weight^7) + I(viscera_weight^7)+ 
                 I(length^8) + I(diameter^8) +  I(height^8) +
                 I(shell_weight^8) + I(whole_weight^8) +  
                 I(shucked_weight^8) + I(viscera_weight^8))

formulas <- c(f1, f2, f3, f4, f5, f6, f7, f8)
MAE_train_array <- c(length(formulas))
MAE_val_array <- c(length(formulas))
i <- 1
for (f in formulas) {
    model <- lm(formula = f, data=trainData)

    trainPred <- predict(model, trainData)
    MAE_train_array[i] <- getMAE(trainPred, trainData$rings)
        
    valPred <- predict(model, valData)
    MAE_val_array[i] <- getMAE(valPred, valData$rings)
    i <- i+1
}

#Ploting only MAE of train
plot(MAE_train_array, type="o", col="blue")

#Ploting MAE of train and validation
plot(MAE_train_array, type="o", col="blue", 
     ylim=c(min(min(MAE_train_array), min(MAE_val_array)),
            max(max(MAE_train_array), max(MAE_val_array))))
points(MAE_val_array, col="red", pch="*")
lines(MAE_val_array, col="red",lty=2)
