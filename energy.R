# Advanced machine learning
# Authors: Patrycja Cieplicka, Pawel Zakieta

# Predicting energy usage
# Load library
library(randomForest)
library(caret)
library(tidyverse)
library(Metrics) 
if(!exists("vectorize", mode="function")) source("vectorize.R")

# Load data
df_energy = read.csv("energydata_complete.csv", sep =",", header = TRUE)
#View(df_energy)

# Check if any NA 
indx <- apply(df_energy, 2, function(x) any(is.na(x) | is.infinite(x)))
indx

#Number of rows
length(df_energy[,1])

#Histogram of labels
hist <- df_energy %>% ggplot() + geom_histogram(aes(x=Appliances), color="blue", fill='lightblue',binwidth=10) + theme_classic() + ggtitle("Histogram odczytów zużycia energii") + labs(x="Zużycie")
hist

# Vectorize data
## JAK WYGLADA TA FUNKCJA?

# Load vectorized data
df_energy = read.csv("energy_win3.csv", sep =",", header = TRUE)
#View(df_energy)

df_energy <- df_energy %>% select(-contains("max"))
df_energy <- df_energy %>% select(-contains("min"))
df_energy <- df_energy %>% select(-X)
#View(df_energy)

# Remove highly correlated variables

df_tmp = cor(df_energy, method="pearson")
hc <- findCorrelation(df_tmp, cutoff=0.85)
hc <- sort(hc)
df_energy_filtered <- df_energy[,-c(hc)]

#View(df_energy_filtered)

# Divide into test and train data
shuffle=sort(sample(nrow(df_energy_filtered), nrow(df_energy_filtered)*0.8))
train_df = df_energy_filtered[shuffle,]
test_df = df_energy_filtered[-shuffle,]

View(train_df)

length(train_df[,1])
length(test_df[,1])

# All features

# Linear regreesion - train model 
linearMod = lm(Appliances.mean ~ ., data = train_df)
print(linearMod)

#Test data
prediction = predict(linearMod, test_df)
prediction

#Calculate RMSE
scores = rmse(test_df$Appliances.mean, prediction)
scores

#MultiLayer Perceptron
library(RSNNS)
library(scales)

#Scale data to (0,1)
max_Appliances = max(df_energy_filtered$Appliances.mean)
min_Appliances = min(df_energy_filtered$Appliances.mean)

df_scaled <-  as.data.frame(apply(df_energy_filtered,2, rescale))
View(df_scaled)

#Divide data into train and test an prepare data

shuffle=sort(sample(nrow(df_scaled), nrow(df_scaled)*0.8))
train_df = df_scaled[shuffle,]
test_df = df_scaled[-shuffle,]

train_df_ <- train_df[,-1]
test_df_ <- test_df[,-1]

View(train_df_)

train_df$Appliances.mean

#Build model
model_mlp <- mlp(x=train_df_, y=train_df$Appliances.mean, maxit=200, size=16, learnFuncParams=c(0.05,0))

#summary(model_mlp)
#model_mlp
#weightMatrix(model_mlp)
#extractNetInfo(model_mlp)

par(mfrow=c(2,2))
plotIterativeError(model_mlp)

prediction <- predict(model_mlp, test_df_)

#reverse rescale
prediction <- min_Appliances + (max_Appliances - min_Appliances) * prediction
actuals <- min_Appliances + (max_Appliances - min_Appliances) * test_df$Appliances.mean

scores = rmse(actuals, prediction)
scores

