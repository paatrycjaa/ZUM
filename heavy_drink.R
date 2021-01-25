library(randomForest)
library(caret)
library(tidyverse)
library(Metrics)
library(e1071)
library(mlbench)


if(!exists("vectorize", mode="function")) source("vectorize.R")

#Load train data - 12 people
subwindow = TRUE
if (subwindow){
  df_1 = read.csv("drink/2windowed/pidBK7610.csv", sep =",", header = TRUE)
  df_2 = read.csv("drink/2windowed/pidCC6740.csv", sep =",", header = TRUE)
  df_3 = read.csv("drink/2windowed/pidDC6359.csv", sep =",", header = TRUE)
  df_4 = read.csv("drink/2windowed/pidDK3500.csv", sep =",", header = TRUE)
  df_5 = read.csv("drink/2windowed/pidHV0618.csv", sep =",", header = TRUE)
  df_6 = read.csv("drink/2windowed/pidHV0618.csv", sep =",", header = TRUE)
  df_7 = read.csv("drink/2windowed/pidJB3156.csv", sep =",", header = TRUE)
  df_8 = read.csv("drink/2windowed/pidJR8022.csv", sep =",", header = TRUE)
  df_9 = read.csv("drink/2windowed/pidMC7070.csv", sep =",", header = TRUE)
  df_10 = read.csv("drink/2windowed/pidMJ8002.csv", sep =",", header = TRUE)
  df_11 = read.csv("drink/2windowed/pidPC6771.csv", sep =",", header = TRUE)
  df_12 = read.csv("drink/2windowed/pidSA0297.csv", sep =",", header = TRUE)
  df_13 = read.csv("drink/2windowed/pidSF3079.csv", sep =",", header = TRUE)

  #Load labels
  label_df_1 = read.csv("drink/2windowed/label_pidBK7610.csv", sep =",", header = TRUE)
  label_df_2 = read.csv("drink/2windowed/label_pidCC6740.csv", sep =",", header = TRUE)
  label_df_3 = read.csv("drink/2windowed/label_pidDC6359.csv", sep =",", header = TRUE)
  label_df_4 = read.csv("drink/2windowed/label_pidDK3500.csv", sep =",", header = TRUE)
  label_df_5 = read.csv("drink/2windowed/label_pidHV0618.csv", sep =",", header = TRUE)
  label_df_6 = read.csv("drink/2windowed/label_pidHV0618.csv", sep =",", header = TRUE)
  label_df_7 = read.csv("drink/2windowed/label_pidJB3156.csv", sep =",", header = TRUE)
  label_df_8 = read.csv("drink/2windowed/label_pidJR8022.csv", sep =",", header = TRUE)
  label_df_9 = read.csv("drink/2windowed/label_pidMC7070.csv", sep =",", header = TRUE)
  label_df_10 = read.csv("drink/2windowed/label_pidMJ8002.csv", sep =",", header = TRUE)
  label_df_11 = read.csv("drink/2windowed/label_pidPC6771.csv", sep =",", header = TRUE)
  label_df_12 = read.csv("drink/2windowed/label_pidSA0297.csv", sep =",", header = TRUE)
  label_df_13 = read.csv("drink/2windowed/label_pidSF3079.csv", sep =",", header = TRUE)
}else{
  df_1 = read.csv("drink/pidBK7610.csv", sep =",", header = TRUE)
  df_2 = read.csv("drink/pidCC6740.csv", sep =",", header = TRUE)
  df_3 = read.csv("drink/pidDC6359.csv", sep =",", header = TRUE)
  df_4 = read.csv("drink/pidDK3500.csv", sep =",", header = TRUE)
  df_5 = read.csv("drink/pidHV0618.csv", sep =",", header = TRUE)
  df_6 = read.csv("drink/pidHV0618.csv", sep =",", header = TRUE)
  df_7 = read.csv("drink/pidJB3156.csv", sep =",", header = TRUE)
  df_8 = read.csv("drink/pidJR8022.csv", sep =",", header = TRUE)
  df_9 = read.csv("drink/pidMC7070.csv", sep =",", header = TRUE)
  df_10 = read.csv("drink/pidMJ8002.csv", sep =",", header = TRUE)
  df_11 = read.csv("drink/pidPC6771.csv", sep =",", header = TRUE)
  df_12 = read.csv("drink/pidSA0297.csv", sep =",", header = TRUE)
  df_13 = read.csv("drink/pidSF3079.csv", sep =",", header = TRUE)

  #Load labels
  label_df_1 = read.csv("drink/label_pidBK7610.csv", sep =",", header = TRUE)
  label_df_2 = read.csv("drink/label_pidCC6740.csv", sep =",", header = TRUE)
  label_df_3 = read.csv("drink/label_pidDC6359.csv", sep =",", header = TRUE)
  label_df_4 = read.csv("drink/label_pidDK3500.csv", sep =",", header = TRUE)
  label_df_5 = read.csv("drink/label_pidHV0618.csv", sep =",", header = TRUE)
  label_df_6 = read.csv("drink/label_pidHV0618.csv", sep =",", header = TRUE)
  label_df_7 = read.csv("drink/label_pidJB3156.csv", sep =",", header = TRUE)
  label_df_8 = read.csv("drink/label_pidJR8022.csv", sep =",", header = TRUE)
  label_df_9 = read.csv("drink/label_pidMC7070.csv", sep =",", header = TRUE)
  label_df_10 = read.csv("drink/label_pidMJ8002.csv", sep =",", header = TRUE)
  label_df_11 = read.csv("drink/label_pidPC6771.csv", sep =",", header = TRUE)
  label_df_12 = read.csv("drink/label_pidSA0297.csv", sep =",", header = TRUE)
  label_df_13 = read.csv("drink/label_pidSF3079.csv", sep =",", header = TRUE)
}


df_drink_ = rbind(df_1, df_3, df_4 ,df_6, df_5,df_7,df_9 ,df_10,df_11, df_12, df_13)
df_drink_test_ = rbind(df_2, df_8)

df_drink_label = rbind(label_df_1, label_df_3, label_df_4 ,label_df_6, label_df_5,label_df_7,label_df_9 ,label_df_10,label_df_11, label_df_12, label_df_13)
df_drink_test_label = rbind(label_df_2, label_df_8)

#removing index column
df_drink_label<-df_drink_label[-1]
df_drink_test_label <- df_drink_test_label [-1]
df_drink_<-df_drink_[-1]
df_drink_test_ <- df_drink_test_[-1]

df_drink = cbind(df_drink_,df_drink_label)
df_drink_test = cbind(df_drink_test_,df_drink_test_label)

length(df_drink[,1])
length(df_drink_test[,1])

View(df_drink_test)

#check if any NA
indx <- apply(df_drink, 2, function(x) any(is.na(x) | is.infinite(x)))
indx
indx <- apply(df_drink_test, 2, function(x) any(is.na(x) | is.infinite(x)))
indx
#fill 0 if NA
df_drink[is.na(df_drink)] <- 0
df_drink_test[is.na(df_drink_test)] <- 0

#delete index column
df_drink <- df_drink[-1]
df_drink_test <-df_drink_test[-1]
df_drink <- df_drink[-1]
df_drink_test <-df_drink_test[-1]
length(df_drink[,1])

#rename label
colnames(df_drink)[colnames(df_drink) == "label.first"] <- "label"
colnames(df_drink_test)[colnames(df_drink_test) == "label.first"] <- "label"
colnames(df_drink)[colnames(df_drink) == "label.first.first"] <- "label"
colnames(df_drink_test)[colnames(df_drink_test) == "label.first.first"] <- "label"
#View(df_drink)

#shuffle data
#label to factor
df_drink$label = factor(df_drink$label)
df_drink_test$label = factor(df_drink_test$label)

rows <- sample(nrow(df_drink))
train_df =df_drink[rows,]

#feature selection
importance_mean = features_importance(train_df[seq(1,length(train_df), length(train_df)/10000)], "label")
importance_mean
length(importance_mean[,1])
df_drink_filtered = select_features(train_df,importance_mean, length(importance_mean[,1]), "label")

#df_stress <- df_stress[-11]
#df_stress_test <- df_stress_test[-11]
###############
#k-fold cross validation

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

control <- trainControl(method="cv", number=5)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(50,100,200,300))
seed <- 7
metric <- c("Accuracy")
set.seed(seed)
custom <- train(label~., data=train_df, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)
custom
#############################################
#Random Forest
rf_classifier = randomForest(label ~ ., data = train_df, mtry=4, ntree=200)

#Prediction
prediction = predict(rf_classifier, train_df)
cm <- caret::confusionMatrix(prediction, train_df$label, mode="prec_recall")
cm

prediction = predict(rf_classifier, df_drink_test)
cm <- caret::confusionMatrix(prediction, df_drink_test$label, mode="prec_recall")

#mtry = 2; ntree= 200;
#############################################
#SVM
## 10-fold cross validation
obj = tune.svm(label ~ ., data=train_df, kernel='radial',type="C-classification", cost=seq(from=0.1, to=1,by=0.1), gamma = seq(from=0.1, to =1, by=0.1))
obj

svm_classifier = svm(label ~ ., data=train_df, kernel="radial", type="C-classification", gamma =0.3, cost=0.9, scale=TRUE )
#svm_classifier$SV

plot(svm_classifier, train_df, eda.sd ~ X.sd)

#Prediction
prediction = predict(svm_classifier, df_drink_test)
#prediction

cm <- caret::confusionMatrix(prediction, df_drink_test$label, mode="prec_recall")
cm


