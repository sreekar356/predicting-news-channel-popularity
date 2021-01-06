library(e1071)
library(caret)
library(randomForest)
library(C50)
library(klaR)
library(kernlab)
data <- read.csv("NewsAnalysis_final.csv")
View(data)
str(data)
data$Popularity <- as.factor(data$Popularity)
data$Views.of.the.Show <- NULL

# Naive bayes 
smp_floor <- floor(0.50*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_NB <- train(Popularity~ ., method = 'nb', data = train, trControl = control)
Pred_NB <- predict(modelFit_NB, test, type= 'raw')
CM_NB <- table(Pred_NB, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_NB)
library(MLeval)
ROC_eval <- evalm(modelFit_NB,'nb')


smp_floor <- floor(0.66*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_NB <- train(Popularity~., method = 'nb', data = train, trControl = control)
Pred_NB <- predict(modelFit_NB, test, type= 'raw')
CM_NB <- table(Pred_NB, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_NB)
ROC_eval <- evalm(modelFit_NB,'nb')


smp_floor <- floor(0.80*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_NB <- train(Popularity~., method = 'nb', data = train, trControl = control)
Pred_NB <- predict(modelFit_NB, test, type= 'raw')
CM_NB <- table(Pred_NB, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_NB)
ROC_eval <- evalm(modelFit_NB,'nb')

# SVM POLY

smp_floor <- floor(0.50*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_SVM <- train(Popularity~., method = 'svmPoly', data = train, trControl = control)
Pred_SVM <- predict(modelFit_SVM, test, type= 'raw')
CM_SVM <- table(Pred_SVM, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_SVM)
ROC_eval <- evalm(modelFit_SVM, 'svm')


smp_floor <- floor(0.66*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_SVM <- train(Popularity~., method = 'svmPoly', data = train, trControl = control)
Pred_SVM <- predict(modelFit_SVM, test, type= 'raw')
CM_SVM <- table(Pred_SVM, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_SVM)
library(MLeval)
ROC_eval <- evalm(modelFit_SVM,'svmP')


smp_floor <- floor(0.80*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_SVM <- train(Popularity~., method = 'svmPoly', data = train, trControl = control)
Pred_SVM <- predict(modelFit_SVM, test, type= 'raw')
CM_SVM <- table(Pred_SVM, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_SVM)
ROC_eval <- evalm(modelFit_SVM,'svmP')






smp_floor <- floor(0.50*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_RF <- train(Popularity~., method = 'rf', data = train, trControl = control)
Pred_RF <- predict(modelFit_RF, test, type= 'raw')
CM_RF <- table(Pred_RF, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_RF)
library(MLeval)
ROC_eval <- evalm(modelFit_RF,'rf')


smp_floor <- floor(0.66*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_RF <- train(Popularity~., method = 'rf', data = train, trControl = control)
Pred_RF <- predict(modelFit_RF, test, type= 'raw')
CM_RF <- table(Pred_RF, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_RF)
library(MLeval)
ROC_eval <- evalm(modelFit_RF,'rf')



smp_floor <- floor(0.80*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_RF <- train(Popularity~., method = 'rf', data = train, trControl = control)
Pred_RF <- predict(modelFit_RF, test, type= 'raw')
CM_RF <- table(Pred_RF, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_RF)
ROC_eval <- evalm(modelFit_RF,'rf')



smp_floor <- floor(0.50*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_DT <- train(Popularity~., method = 'C5.0', data = train, trControl = control)
Pred_DT <- predict(modelFit_DT, test, type= 'raw')
CM_DT <- table(Pred_DT, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_DT)
ROC_eval <- evalm(modelFit_DT,'dt')



smp_floor <- floor(0.66*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_DT <- train(Popularity~., method = 'C5.0', data = train, trControl = control)
Pred_DT <- predict(modelFit_DT, test, type= 'raw')
CM_DT <- table(Pred_DT, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_DT)
library(MLeval)
ROC_eval <- evalm(modelFit_DT,'dt')


smp_floor <- floor(0.80*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_DT <- train(Popularity~., method = 'C5.0', data = train, trControl = control)
Pred_DT <- predict(modelFit_DT, test, type= 'raw')
CM_DT <- table(Pred_DT, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_DT)
library(MLeval)
ROC_eval <- evalm(modelFit_DT,'dt')


# After feature selection using WEKA

# Naive bayes 
smp_floor <- floor(0.50*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_NB <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'nb', data = train, trControl = control)
Pred_NB <- predict(modelFit_NB, test, type= 'raw')
CM_NB <- table(Pred_NB, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_NB)
library(MLeval)
ROC_eval <- evalm(modelFit_NB,'nb')


smp_floor <- floor(0.66*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_NB <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'nb', data = train, trControl = control)
Pred_NB <- predict(modelFit_NB, test, type= 'raw')
CM_NB <- table(Pred_NB, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_NB)
ROC_eval <- evalm(modelFit_NB,'nb')


smp_floor <- floor(0.80*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_NB <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'nb', data = train, trControl = control)
Pred_NB <- predict(modelFit_NB, test, type= 'raw')
CM_NB <- table(Pred_NB, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_NB)
ROC_eval <- evalm(modelFit_NB,'nb')

# SVM POLY

smp_floor <- floor(0.50*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_SVM <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'svmPoly', data = train, trControl = control)
Pred_SVM <- predict(modelFit_SVM, test, type= 'raw')
CM_SVM <- table(Pred_SVM, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_SVM)
ROC_eval <- evalm(modelFit_SVM,'svmP')


smp_floor <- floor(0.66*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_SVM <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'svmPoly', data = train, trControl = control)
Pred_SVM <- predict(modelFit_SVM, test, type= 'raw')
CM_SVM <- table(Pred_SVM, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_SVM)
library(MLeval)
ROC_eval <- evalm(modelFit_SVM,'svmP')


smp_floor <- floor(0.80*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_SVM <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'svmPoly', data = train, trControl = control)
Pred_SVM <- predict(modelFit_SVM, test, type= 'raw')
CM_SVM <- table(Pred_SVM, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_SVM)
ROC_eval <- evalm(modelFit_SVM,'svmP')






smp_floor <- floor(0.50*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_RF <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'rf', data = train, trControl = control)
Pred_RF <- predict(modelFit_RF, test, type= 'raw')
CM_RF <- table(Pred_RF, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_RF)
library(MLeval)
ROC_eval <- evalm(modelFit_RF,'rf')


smp_floor <- floor(0.66*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_RF <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'rf', data = train, trControl = control)
Pred_RF <- predict(modelFit_RF, test, type= 'raw')
CM_RF <- table(Pred_RF, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_RF)
library(MLeval)
ROC_eval <- evalm(modelFit_RF,'rf')



smp_floor <- floor(0.80*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_RF <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'rf', data = train, trControl = control)
Pred_RF <- predict(modelFit_RF, test, type= 'raw')
CM_RF <- table(Pred_RF, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_RF)
ROC_eval <- evalm(modelFit_RF,'rf')



smp_floor <- floor(0.50*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_DT <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'C5.0', data = train, trControl = control)
Pred_DT <- predict(modelFit_DT, test, type= 'raw')
CM_DT <- table(Pred_DT, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_DT)
ROC_eval <- evalm(modelFit_DT,'dt')



smp_floor <- floor(0.66*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_DT <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'C5.0', data = train, trControl = control)
Pred_DT <- predict(modelFit_DT, test, type= 'raw')
CM_DT <- table(Pred_DT, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_DT)
library(MLeval)
ROC_eval <- evalm(modelFit_DT,'dt')


smp_floor <- floor(0.80*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_DT <- train(Popularity~ Anchor + English.Hindi + BARC.Rating.for.the.week + Urban.Rural, method = 'C5.0', data = train, trControl = control)
Pred_DT <- predict(modelFit_DT, test, type= 'raw')
CM_DT <- table(Pred_DT, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_DT)
library(MLeval)
ROC_eval <- evalm(modelFit_DT,'dt')

###after wrapper

smp_floor <- floor(0.66*nrow(data))
set.seed(123)
train_s <- sample(seq_len(nrow(data)),size = smp_floor)
train <- data[train_s,]
test <- data[-train_s,]
control <-trainControl(method = "cv", summaryFunction = twoClassSummary, classProbs = T, savePredictions = T )
modelFit_DT <- train(Popularity~ Anchor + BARC.Rating.for.the.week, method = 'C5.0', data = train, trControl = control)
Pred_DT <- predict(modelFit_DT, test, type= 'raw')
CM_DT <- table(Pred_DT, test$Popularity, dnn = c("predicted","Actual"))
confusionMatrix(CM_DT)
library(MLeval)
ROC_eval <- evalm(modelFit_DT,'dt')


