#############################
# MDC - Machine Learning	#
# Logistic Regression		#
# Breast Cancer				#
#############################

#Install pROC & caret
install.packages("pROC")
install.packages("caret")

# Load the data
trainData <- read.csv("breastCancer_train.data", header=TRUE) 
testData <- read.csv("breastCancer_test.data", header=TRUE) 



############ Inspect the Data ############
summary(trainData)


########### Removing 'id' collum
trainData[,"id"] <- NULL
testData[,"id"] <- NULL


########### Transform bare.nuclei from factor to numeric
trainData$bare.nuclei <- as.numeric(trainData$bare.nuclei)
testData$bare.nuclei <- as.numeric(testData$bare.nuclei)

summary(trainData)


# Print the correlation between features
cor(trainData)


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

trainData[1:5,]
summary(trainData)




####### Training Logistic Regression
help(glm)

formula <- as.formula("class ~ .")

#### error!!! class must be a 0 <= class <= 1
model <- glm(formula, trainData, family=binomial(link="logit"))

trainData$class

# Change class = 2 to class = 0
#and class = 4 to class = 1
trainData[trainData$class == 2, "class"] <- 0
trainData[trainData$class == 4, "class"] <- 1

trainData$class

testData[testData$class == 2, "class"] <- 0
testData[testData$class == 4, "class"] <- 1



# Let's try again
logRegModel <- glm(formula, trainData, family=binomial(link="logit"))
summary(logRegModel)

# probabilities of being the class 0 (removing the "class" collumn)
testPred <- predict(logRegModel, testData[,1:9], type="response")
testPred

#converting to class
testClassPred <- testPred
# testClassPred[testPred >= 0.5] <- 1
# testClassPred[testPred < 0.5] <- 0

testClassPred[testPred >= 0.9] <- 1
testClassPred[testPred < 0.9] <- 0


##### Let's see how well we did

#confusion matrix
cm <- as.matrix(table(Actual = testData$class, Predicted = testClassPred))
cm

#ACC = (TP + TN) / total
ACC <- (cm[1,1] + cm[2,2]) / sum(cm)
ACC

#TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
TPR <- cm[2,2] / (cm[2,2] + cm[2,1])
# or TPR = cm[2,2] / sum(cm[2,])
TPR

#TNR <- (TN) / (TN + FP)		ou		TNR = TN / nNeg 
TNR <- cm[1,1] / (cm[1,1] + cm[1,2])
# or TNR = cm[1,1] / sum(cm[1,])
TNR

#ACC balanced by class
#takes into account the number of samples for each class
BalancedACC_glm <- mean(c(TPR, TNR))

ACC
BalancedACC_glm



#confusion matrix v2
library("caret")
cm <- confusionMatrix(data = as.factor(testClassPred), 
                     reference = as.factor(testData$class), 
                     positive='1')

cm
cm$table
cm$overall
cm$byClass


# ROC Curve
library("pROC")

ROC <- roc(testData$class, testPred, direction="<")
ROC

plot(ROC, col="blue", lwd=2, main="ROC")
