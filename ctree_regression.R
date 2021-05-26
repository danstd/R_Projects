#This file is a test for caret::ctree usage on the prostate cancer attribute prediciton question for the IS675 Final

library(caret)
library(party)
library(partykit)


#Compare Tree and Regression analyses for predicting both lpsa and lcavol. So there will be 4 separate analyses that you compare and discuss.

prostate <- read.csv("prostate.csv")


ctree_control <- trainControl(method = "cv")

set.seed(742)
ctree_classifier <- train(lcavol ~ ., data = prostate,method='ctree', metric = "Correlation", trControl = ctree_control)
ctreeVarImp = varImp(ctree_classifier)