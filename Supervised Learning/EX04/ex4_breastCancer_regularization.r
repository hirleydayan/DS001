#############################
# MDC - Machine Learning	#
# Logistic Regression + Reg	#
# Breast Cancer				#
#############################

#Install glmnet
install.packages("glmnet")
install.packages("caret")


library(glmnet)   #for logreg with regularization
library(caret)  #for cm

# Load and format the data 
trainData <- read.csv("breastCancer_train_regularization.data", header=TRUE) 
testData <- read.csv("breastCancer_test_regularization.data", header=TRUE) 

dim(trainData)
dim(testData)

trainData[,"id"] <- NULL
testData[,"id"] <- NULL

trainData$bare.nuclei <- as.numeric(trainData$bare.nuclei)
testData$bare.nuclei <- as.numeric(testData$bare.nuclei)

trainData[trainData$class == 2, "class"] <- 0
trainData[trainData$class == 4, "class"] <- 1

testData[testData$class == 2, "class"] <- 0
testData[testData$class == 4, "class"] <- 1

####### Normalizing the data
maxTrainFeatures <- apply(trainData[,1:9], 2, max) #max of each feature
minTrainFeatures <- apply(trainData[,1:9], 2, min) #min of each feature

minMaxDiffTrain <- (maxTrainFeatures - minTrainFeatures)

trainData[,1:9] <- sweep(trainData[,1:9], 2, minTrainFeatures, "-")
trainData[,1:9] <- sweep(trainData[,1:9], 2, minMaxDiffTrain, "/")

testData[,1:9] <- sweep(testData[,1:9], 2, minTrainFeatures, "-")
testData[,1:9] <- sweep(testData[,1:9], 2, minMaxDiffTrain, "/")





##################################################
##################################################
# Without regularization   - Complex model       #
##################################################
##################################################
f <- as.formula(class ~ . + 0 + I(clump.thickness^2) + I(unif.cell.size^2) +
                    I(unif.cell.shape^2) + I(marginal.adhesion^2) +
                    I(epithelial.cell.size^2) + I(bare.nuclei^2) +
                    I(bland.chromatin^2) + I(normal.nucleoli^2) + I(mitoses^2) +
                    I(clump.thickness^3) + I(unif.cell.size^3) +
                    I(unif.cell.shape^3) + I(marginal.adhesion^3) +
                    I(epithelial.cell.size^3) + I(bare.nuclei^3) +
                    I(bland.chromatin^3) + I(normal.nucleoli^3) + I(mitoses^3))


#Transform input data into matrix
x_train <- model.matrix(f, trainData)


#and separate class info
y_train <- trainData$class




#Train a logReg with lambda (regularization weight) = 0.0
#the alpha control the type of regularization (alpha=0 -> L1 norm, alpha=1 -> L2 norm)
# https://cran.r-project.org/web/packages/glmnet/glmnet.pdf
model <- glmnet(x_train, y_train,  family="binomial", alpha=1, lambda = 1.0)



#Let's see how we did in the training set
trainPred <- predict(model,newx = x_train, type="response") #response outputs a probability

trainClassPred <- trainPred
trainClassPred[trainPred >= 0.5] <- 1
trainClassPred[trainPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(trainData$class), 
                      positive='1')
cm$table
cm$byClass["Balanced Accuracy"]




#Let's see how we did in the test set
x_test <- model.matrix(f, testData)

testPred <- predict(model,newx = x_test, type="response") #response outputs a probability
testClassPred <- testPred
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testData$class), 
                      positive='1')

cm$table
cm$byClass["Balanced Accuracy"]










##################################################
##################################################
# With regularization   - Complex model          #
##################################################
##################################################

# Testing different lambdas
lambda <- c(0, 1e-07, 1e-06, 1e-05, 1e-04, 1e-03, 1e-02, 0.1, 1, 10)
accTrain <- c(length(lambda))
accTest <- c(length(lambda))

i<-1
for (l in lambda){
    model <- glmnet(x_train, y_train,  family="binomial", alpha=0, lambda = l)
    
    # Train
    trainPred <- predict(model,newx = x_train, type="response")
    trainClassPred <- trainPred
    trainClassPred[trainPred >= 0.5] <- 1
    trainClassPred[trainPred < 0.5] <- 0
    
    cm <- confusionMatrix(data = as.factor(trainClassPred), 
                          reference = as.factor(trainData$class), 
                          positive='1')
    
    BalancedACC <- cm$byClass["Balanced Accuracy"]
    accTrain[i] <- BalancedACC
    
    # Test
    testPred <- predict(model,newx = x_test, type="response")
    testClassPred <- testPred
    testClassPred[testPred >= 0.5] <- 1
    testClassPred[testPred < 0.5] <- 0
    
    cm <- confusionMatrix(data = as.factor(testClassPred), 
                          reference = as.factor(testData$class), 
                          positive='1')
    
    
    BalancedACC <- cm$byClass["Balanced Accuracy"]
    accTest[i] <- BalancedACC

    i <- i + 1
}



plot(accTrain, type="o", col="blue", xaxt="n", xlab="Lambda", ylim=c(0.7, 1.0), main="Balanced ACC x Lambda")
axis(1, at=1:length(lambda), labels=lambda, cex.axis=0.5, las=2)
points(accTest, col="red", pch="*")
lines(accTest, col="red",lty=2)
