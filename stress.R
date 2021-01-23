library(randomForest)
library(caret)
library(tidyverse)
library(Metrics)
library(e1071)

if(!exists("vectorize", mode="function")) source("vectorize.R")

#Load train data - 12 people
df_S2 = read.csv("stress_data/S2_window_10.csv", sep =",", header = TRUE)
df_S3 = read.csv("stress_data/S3_window_10.csv", sep =",", header = TRUE)
df_S4 = read.csv("stress_data/S4_window_10.csv", sep =",", header = TRUE)
df_S5 = read.csv("stress_data/S5_window_10.csv", sep =",", header = TRUE)
df_S6 = read.csv("stress_data/S6_window_10.csv", sep =",", header = TRUE)
df_S7 = read.csv("stress_data/S7_window_10.csv", sep =",", header = TRUE)
df_S8 = read.csv("stress_data/S8_window_10.csv", sep =",", header = TRUE)
df_S10 = read.csv("stress_data/S10_window_10.csv", sep =",", header = TRUE)
df_S11 = read.csv("stress_data/S11_window_10.csv", sep =",", header = TRUE)

df_stress = rbind(df_S2, df_S3, df_S4 ,df_S5 ,df_S6 ,df_S7,df_S8 ,df_S10,df_S11)

#delete NA column
df_stress = df_stress[ , ! apply( df_stress , 2 , function(x) all(is.na(x)) ) ]
#check if any NA
indx <- apply(df_stress, 2, function(x) any(is.na(x) | is.infinite(x)))
indx

#delete index column
df_stress <- df_stress[-1]
length(df_stress[,1])

#rename label
colnames(df_stress)[colnames(df_stress) == "label.first"] <- "label"
View(df_stress)

#shuffle data
#label to factor
df_stress$label = factor(df_stress$label)

rows <- sample(nrow(df_stress))
train_df =df_stress[rows,]

#feature selection
importance_mean = features_importance(train_df, "label")
importance_mean

df_stress_filtered = select_features(train_df,importance_mean, 5, "label")

#############################################
#Random Forest
rf_classifier = randomForest(label ~ ., data = train_df)

#Prediction
prediction = predict(rf_classifier, train_df)
prediction

cm <- confusionMatrix(prediction, train_df$label)
cm


#############################################
#SVM

svm_classifier = svm(label ~ ., data=train_df, kernel='radial', type="C-classification", gamma =0.1, cost=10 )
svm_classifier$SV 

plot(svm_classifier, train_df, eda.sd ~ X.sd)

#Prediction
prediction = predict(svm_classifier, train_df)
prediction

cm <- confusionMatrix(prediction, train_df$label)
cm
