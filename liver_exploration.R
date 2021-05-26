library(kernlab)
library(caret)
#This function wraps the caret::svmbag$pred function to fix problems with the current version. 
#From https://stackoverflow.com/questions/31844742/caret-function-train-failing-for-bagged-svm


normalize <- function(x){
    return((x-min(x))/ (max(x) - min(x)))
}

#This function wraps the caret::svmbag$pred function to fix problems with the current version. 
#From https://stackoverflow.com/questions/31844742/caret-function-train-failing-for-bagged-svm
predfunct <-function (object, x)
{
    loadNamespace("kernlab")
    if (is.character(lev(object))) {
        out <- kernlab::predict(object, as.matrix(x), type = "probabilities")
        colnames(out) <- lev(object)
        rownames(out) <- NULL
    }
    else out <- kernlab::predict(object, as.matrix(x))[, 1]
    out
}
classifier_control <- trainControl(method = "cv")

# create a bag control object using svmBag
bagctrl <- bagControl(fit = svmBag$fit, predict = predfunct, aggregate = svmBag$aggregate)

grid <- expand.grid(vars = c(1,3,5))

# fit the bagged svm model
set.seed(742)
svm_bag_results <- train(Dataset ~ ., liver_data_n, "bag", trControl = classifier_control, bagControl = bagctrl, tuneGrid = grid)

svm_bag_results

liver_data <- read.csv("liver.csv", sep = ",", stringsAsFactors = FALSE)
liver_data <- liver_data[-(which(is.na(liver_data$Albumin_and_Globulin_Ratio))),]
liver_data$Gender <- ifelse(liver_data$Gender == "Female", 1, 0)
liver_data[which(liver_data$Dataset == 2), "Dataset"] <- 0
liver_data <- liver_data[,c(3:7,11)]
liver_data_n <- as.data.frame(normalize(liver_data[,-6]))
liver_data_n$Dataset <- ifelse(liver_data_n$Dataset == 1, "yes", "no")
liver_data_n$Dataset <- as.factor(liver_data_n$Dataset)

#liver_data$Dataset <- as.factor(liver_data$Dataset)

classifier_control <- trainControl(method = "cv")

linear_grid <- expand.grid( C = c(2^-5, 2^-3, 2^-1, 2, 2^3, 2^5, 2^9, 2^11, 2^15))

grid <- expand.grid(sigma = c(2^-5, 2^-3, 2^-1, 2, 2^3, 2^5), C = c(2^-5, 2^-3, 2^-1, 2, 2^3, 2^5, 2^9, 2^11, 2^15))

#The svm models are fitted.
set.seed(742)
linear_svm <- train(Dataset ~ ., liver_data_n, method="svmLinear", 
                    trControl = classifier_control, preProcess = c("center", "scale"), tuneGrid = linear_grid, metric = "Kappa")

linear_svm


set.seed(742)
radial_svm <- train(Dataset ~ ., liver_data_n, method="svmRadial",
                    trControl = classifier_control, tuneGrid = grid, preProcess = c("center", "scale"), metric = "Kappa")

radial_svm
