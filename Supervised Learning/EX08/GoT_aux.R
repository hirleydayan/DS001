library(rpart)
library(randomForest)
library(caret)

#######################################
#    Aux Method for metrics           #
#######################################
getBalancedAcc <- function(testPred, testLabels){
    prediction <- ifelse(testPred >= 0.5, "alive", "dead")
    prediction <- as.factor(prediction)
    cm <- confusionMatrix(data = prediction, 
                          reference = testLabels, 
                          positive='alive')
    
    return(list(acc=cm$byClass["Balanced Accuracy"],table=cm$table) )
}

getMajVote <- function(testPred){
    prediction <- round(testPred)
    sumOfVotes <- sum(prediction)/length(prediction)
    return(ifelse(sumOfVotes>=0.5, 1.0, 0.0))
}


################################
#    Pre-process features      #
################################

processNA <- function(feature){
    feature[is.na(feature)] <- 0
    return(feature)
}




cleanDataset <- function(data){
    featureNames <- c("isAliveSpouse", "isAliveHeir", "isAliveFather", "isAliveMother")
    data[, featureNames] <- apply(data[, featureNames], 2, processNA)
    
    data[, "father"] <- NULL
    data[, "mother"] <- NULL
    data[, "heir"] <- NULL
    data[, "age"] <- NULL
    data[, "spouse"] <- NULL
    data[, "dateOfBirth"] <- NULL
    
    levels(data$title)[levels(data$title)==""] <- "no_title"
    levels(data$house)[levels(data$house)==""] <- "no_house"
    levels(data$culture)[levels(data$culture)==""] <- "no_culture"

    return(data)
}



rpartRF <- function(dataset, target, predictor_names, percent_predictors, percent_obs, num_trees,
                    complex_param = 0.0005, min_split = 10, min_bucket = 3, max_depth = 20) {
    obs_weights <- rep(1, nrow(dataset))
    # stick the weights to the dataset
    dataset <- cbind(dataset, obs_weights)   
    # an empty list for our trees
    rforest <- list()
    # build trees
    for(i in 1:num_trees) {
        # take a percent_predictors sample of the predictors
        tree_predictors <- sample(predictor_names, length(predictor_names) * percent_predictors, replace = FALSE)
        # take a sample of the observations (not stratified over class)
        in_bag <- apply(dataset, MARGIN = 1, FUN = function(v) ifelse(runif(n = 1, min = 0, max = 1) <= percent_obs, 1, 0))  
        ds <- cbind(dataset, in_bag)
        # which observations are in bag
        in_bag <- which(ds$in_bag == 1)
        # which observations are out of bag
        out_bag <- which(ds$in_bag == 0)   
        # set the rpart.control object
        t_control <- rpart.control(minsplit = min_split, minbucket = min_bucket, cp = complex_param, maxdepth = max_depth) 
        
        # build a tree
        tree <- rpart(formula = ds[in_bag,target] ~ ., data = ds[in_bag,tree_predictors], weights = ds[in_bag,"obs_weights"], control = t_control) 
        # add our tree to the forest
        rforest[[i]] <- tree
    }
    # return our list of trees
    return(rforest)
}


