# Homework 9_2------------------------------------------------------------

# Pre-Flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(caret)
library(pROC)

# Source training sets
source("homework9.R")

# Set train control for SVM model
fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           repeats = 5)

# Set train control for NB and NN models
fitControl2 <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           repeats = 5, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)

# Train the SVM model
svm_mod <- train(spam ~ ., 
                 data = training_set_no_id,
                 method = "svmLinearWeights2",
                 trControl = fitControl,
                 tuneGrid = data.frame(cost = 1, 
                                       Loss = 0, 
                                       weight = 1))

# Predict against test set 
svm_pred <- test_set %>% select(-spam) %>% predict(svm_mod, newdata = .)

# Get confusion matrix, agreement, and accuracy values 
confusionMatrix(svm_pred,as.factor(test_set$spam))

# Train the KNN model
knnfit <- train(spam ~ ., 
                data = training_set_no_id,
                method = "knn",
                tuneLength = 7)

# Predict against test set 
knn_pred <- test_set %>% select(-spam) %>% predict(knnfit, newdata = .)

# Get confusion matrix, agreement, and accuracy values 
confusionMatrix(knn_pred,as.factor(test_set$spam))

# Train the NB Model
nb_mod = train(spam ~ ., 
               data = training_set_no_id, 
               method="naive_bayes", 
               trControl = fitControl2, 
               tuneGrid = expand.grid(usekernel=TRUE,laplace=0,adjust=1))

# Predict against test set
nb_pred <- test_set %>% select(-spam) %>% predict(nb_mod, newdata = .)

# Get confusion matrix, agreement, and accuracy values
confusionMatrix(nb_pred,as.factor(test_set$spam))

# Train the NN Model 
nnetFit <- train(spam ~ ., 
                 data = training_set_no_id,
                 method = "nnet",
                 metric = "ROC",
                 trControl = fitControl2,
                 tuneLength = 3,
                 verbose = FALSE)

# Predict against test set 
nn_pred <- test_set %>% select(-spam) %>% predict(nnetFit, newdata = .)

# Get confusion matrix, agreement, and accuracy values
confusionMatrix(nn_pred,as.factor(test_set$spam))

# Get ROCs ----------------------------------------------------------------
knn_pred <- test_set %>% select(-spam) %>% predict(knnfit, newdata = ., type = 'prob')
knn_roc <- roc(test_set$spam,knn_pred$yes)
knn_roc

nb_pred <- test_set %>% select(-spam) %>% predict(nb_mod, newdata = ., type = 'prob')
nb_roc <- roc(test_set$spam,nb_pred$yes)
nb_roc

nn_pred <- test_set %>% select(-spam) %>% predict(nnetFit, newdata = ., type = 'prob')
nn_roc <- roc(test_set$spam,nn_pred$yes)
nn_roc

# Compare Curves ----------------------------------------------------------
ggroc(list(knn=knn_roc,nb=nb_roc,nnet=nn_roc), legacy.axes = TRUE)

# It can be concluded that because (in this particular test) all of these models (SVM, KNN, NB, and NN) have a Kappa value greater than 0.8, they are all relatively good options for predicting what content is spam. In this test, the SVM model and the NN model had the highest Kappa values, and thus they are likely the best predictive models. The curves and areas under the curves of the three applicable models are all similar, as they all have an area under the curve over 0.9
# Overall, based on this test, the SVM model and the NN model appear to be the best options
