#############################
# MDC - Machine Learning	#
# Logistic Regression		#
# Breast Cancer				#
#############################
set.seed(42)

#Install glmnet
install.packages("glmnet")
install.packages("DMwR")

# Load the data
trainData <- read.csv("breastCancer_train_unbalanced.data", header=TRUE) 
testData <- read.csv("breastCancer_test_unbalanced.data", header=TRUE) 


############ Inspect the Data ############
dim(trainData[trainData$class == 2,]) # Number of neg samples
dim(trainData[trainData$class == 4,]) # Number of pos samples


########### Removing 'id' collum
trainData[,"id"] <- NULL
testData[,"id"] <- NULL

# Change class = 2 to class = 0
# and class = 4 to class = 1
trainData[trainData$class == 2, "class"] <- 0
trainData[trainData$class == 4, "class"] <- 1
testData[testData$class == 2, "class"] <- 0
testData[testData$class == 4, "class"] <- 1


########### Transform bare.nuclei from factor to numeric
trainData$bare.nuclei <- as.numeric(trainData$bare.nuclei)
testData$bare.nuclei <- as.numeric(testData$bare.nuclei)


####### Normalizing the data
maxTrainFeatures <- apply(trainData[,1:9], 2, max) #max of each feature
minTrainFeatures <- apply(trainData[,1:9], 2, min) #min of each feature

maxTrainFeatures
minTrainFeatures

minMaxDiffTrain <- (maxTrainFeatures - minTrainFeatures)
minMaxDiffTrain

trainData[,1:9] <- sweep(trainData[,1:9], 2, minTrainFeatures, "-")
trainData[,1:9] <- sweep(trainData[,1:9], 2, minMaxDiffTrain, "/")

testData[,1:9] <- sweep(testData[,1:9], 2, minTrainFeatures, "-")
testData[,1:9] <- sweep(testData[,1:9], 2, minMaxDiffTrain, "/")


####### Training Logistic Regression
formula <- as.formula("class ~ .")

logRegModel <- glm(formula, trainData, family=binomial(link="logit"))
summary(logRegModel)

# probabilities of being the class 0
testPred <- predict(logRegModel, testData[,1:9], type="response")

#converting to class
testPred[testPred >= 0.5] <- 1
testPred[testPred < 0.5] <- 0


#confusion matrix
library("caret")
cm <- confusionMatrix(data = as.factor(testPred), 
                     reference = as.factor(testData$class), 
                     positive='1')

cm$table
cm$overall["Accuracy"]
cm$byClass["Balanced Accuracy"]




##################################
####### Oversampling #############
##################################

positiveData <- trainData[trainData$class == 1,]
negativeData <- trainData[trainData$class == 0,]

dim(positiveData)
dim(negativeData)

selectedIndex <- sample(1:nrow(positiveData), 50*nrow(positiveData), replace=TRUE)
oversampledPosData <- positiveData[selectedIndex,]
dim(oversampledPosData)

newTrainData <- rbind(oversampledPosData, negativeData)


logRegModel <- glm(formula, newTrainData, family=binomial(link="logit"))
testPred <- predict(logRegModel, testData[,1:9], type="response")
testPred[testPred >= 0.5] <- 1
testPred[testPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(testPred), 
                     reference = as.factor(testData$class), 
                     positive='1')
cm$table
cm$overall["Accuracy"]
cm$byClass["Balanced Accuracy"]





##################################
####### Undersampling ############
##################################
positiveData <- trainData[trainData$class == 1,]
negativeData <- trainData[trainData$class == 0,]

dim(positiveData)
dim(negativeData)

selectedIndex <- sample(1:nrow(negativeData), 2*nrow(positiveData), replace=FALSE)
undersampledNegData <- negativeData[selectedIndex,]
dim(undersampledNegData)

newTrainData <- rbind(positiveData, undersampledNegData)


logRegModel <- glm(formula, newTrainData, family=binomial(link="logit"))
testPred <- predict(logRegModel, testData[,1:9], type="response")
testPred[testPred >= 0.5] <- 1
testPred[testPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(testPred), 
                     reference = as.factor(testData$class), 
                     positive='1')
cm$table
cm$overall["Accuracy"]
cm$byClass["Balanced Accuracy"]





##################################
############ SMOTE ###############
##################################

library(DMwR)

help(SMOTE)

# Be carefull!!!! SMOTE only works with factor as labels
trainData$class <- as.factor(trainData$class)
newTrainData <- SMOTE(formula, trainData, 
                     perc.over = 100,  
                     perc.under = 200, 
                     k=2)
#per.over/100 is the number of new cases (smoted cases) generated for each rare case
#perc.under/100 is the number of "normal" cases that are randomly selected for each smoted case

dim(newTrainData)
table(newTrainData$class)


logRegModel <- glm(formula, newTrainData, family=binomial(link="logit"))

testPred <- predict(logRegModel, testData[,1:9], type="response")
testPred[testPred >= 0.5] <- 1
testPred[testPred < 0.5] <- 0


cm <- confusionMatrix(data = as.factor(testPred), 
                     reference = as.factor(testData$class), 
                     positive='1')

cm$table
cm$overall["Accuracy"]
cm$byClass["Balanced Accuracy"]
