#################################
# Aprendizado Supervisionado I	#
# DT & RF						#
# Kyphosis						#
#################################
library("caret")
set.seed(42)

# Predict data using model and evaluate
predictAndEvaluate <- function(model, data, isDecisionTree = TRUE){
    prediction <- predict(model, data)
    
    if (isDecisionTree){
        prediction <- as.numeric(prediction[,"present"] >= 0.5)
        prediction[prediction==0] <- "absent"
        prediction[prediction==1] <- "present"
    }
    
    cm <- confusionMatrix(data = as.factor(prediction), 
                          reference = as.factor(data$Kyphosis), 
                          positive='present')
    
    return(list(CM=cm$table, STATS=cm$byClass, ACC=cm$overall["Accuracy"]))
}




# Load the data
trainData <- read.csv("kyphosis_train.data")
valData <- read.csv("kyphosis_val.data")

summary(trainData)
summary(valData)

#Decision Tree
library(rpart)
library(rpart.plot)
help(rpart)


#minsplit = min # of samples in a node in order to perform a split
#cp = factor whereby a new split must decrease the overall lack of fit

#If we want to use Entropy + Gain of Information
treeModel <- rpart(formula=Kyphosis~Age + Number + Start, 
                   data=trainData, method="class",
                   #control=rpart.control(minsplit=2, cp=0.0),
                   parms= list(split="information"))

#If we want to use Gini to select features
# treeModel <- rpart(formula=Kyphosis~Age + Number + Start, 
#                    data=trainData, method="class",
#                    control=rpart.control(minsplit=2, cp=0.0), 
#                    parms= list(split="gini"))



summary(treeModel)


#Plot using prp
prp(treeModel)

#Plot using rpart.plot
rpart.plot(treeModel,
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)



######### POST PRUNE ########

#Print the table with complexity parameters
printcp(treeModel)

#Prune the tree based on the complexity parameter that minimizes 
#the error in cross-validation (xerror)
minCP <- treeModel$cptable[which.min(treeModel$cptable[,"xerror"]),"CP"]
minCP

ptree <- prune(treeModel, cp=minCP)
summary(ptree)


#Plot the pruned tree
rpart.plot(ptree, 
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)




######### EVALUATION ##########

#Let's see how we do in the valSet
#non-prunned tree
treeEval <- predictAndEvaluate(treeModel, valData)
treeEval$CM
treeEval$STATS["Balanced Accuracy"]

#prunned tree
ptreeEval <- predictAndEvaluate(ptree, valData)
ptreeEval$CM
ptreeEval$STATS["Balanced Accuracy"]





########## ACC Vs Depth 
# Let's see how the acc varies as we increase the tree's depth
accPerDepth <- data.frame(depth=numeric(15), accTrain=numeric(15), accVal=numeric(15))
for (maxDepth in 1:15){
    treeModel <- rpart(formula=Kyphosis~Age + Number + Start, 
                       data=trainData, method="class",
                       control=rpart.control(minsplit=2, cp=0.0, maxdepth=maxDepth),
                       parms= list(split="information"))
    
    trainResults <- predictAndEvaluate(treeModel, trainData)
    valResults <- predictAndEvaluate(treeModel, valData)
    
    accPerDepth[maxDepth,] = c(maxDepth, 
                               trainResults$STATS["Balanced Accuracy"], 
                               valResults$STATS["Balanced Accuracy"])
}

#Plot
library("reshape2")
library("ggplot2")

accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line()










############# RANDOM FOREST
install.packages('randomForest')
library(randomForest)
help(randomForest)


#Train RF model
rfModel <- randomForest(formula=Kyphosis~Age + Number + Start, 
                        data= trainData, ntree=10)



#Plotting the error
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rfModel, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rfModel$err.rate),col=1:4,cex=0.8,fill=1:4)


sizes = treesize(rfModel)
minSize = min(sizes)
maxSize = max(sizes)
# Check the tree sizes
hist(sizes,
     main="Histogram of Tree Depths",
     xlab="Depth",
     ylab="Frequency",
     xlim=c(minSize,maxSize),
     ylim=c(0,maxSize),
     las=1, 
     breaks=maxSize - minSize,
     xaxt="n")
axis(1, at=seq(minSize, maxSize, by=1), labels=seq(minSize, maxSize, by=1))


#Confusion Matrix
rfEval <- predictAndEvaluate(rfModel, valData, isDecisionTree=FALSE)
rfEval$CM
rfEval$STATS["Balanced Accuracy"]




# Acc vs NTrees
set.seed(42)
nTreeList = c(1, 5, 10, 25, 50, 100, 250, 500, 1000)
accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))



for (i in 1:length(nTreeList)){
    rfModel <- randomForest(formula=Kyphosis~Age + Number + Start, 
                            data= trainData, ntree=nTreeList[i])
    
    trainResults <- predictAndEvaluate(rfModel, trainData, isDecisionTree = FALSE)
    valResults <- predictAndEvaluate(rfModel, valData, isDecisionTree = FALSE)
    
    accPerNTrees[i,] = c(nTreeList[i], 
                               trainResults$STATS["Balanced Accuracy"], 
                               valResults$STATS["Balanced Accuracy"])
}

accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line()
