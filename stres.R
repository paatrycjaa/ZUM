library(tidyverse)
library(reticulate)
library(randomForest)


## Load WESAD data for only one participant (S10) from pickle file
pd <- reticulate::import("pandas")
s1 <- pd$read_pickle("WESAD/S10/S10.pkl")

View(s1)

#Create dataframe with data regarding chest signal
s1_chest = s1$signal$chest
s1_labels = s1$label

View(s1_chest)

acc = data.matrix(s1_chest$ACC)
colnames(acc) <- c("X", "Y", "Z")

ecg = data.matrix(s1_chest$ECG)
colnames(ecg) <- c("ecg")

emg = data.matrix(s1_chest$EMG)
colnames(emg) <- c("emg")

eda = data.matrix(s1_chest$EDA)
colnames(eda) <- c("eda")

temp = data.matrix(s1_chest$Temp)
colnames(temp) <- c("temperature")

respi = data.matrix(s1_chest$Resp)
colnames(respi) <- c("resp")

label = data.matrix(s1$label)
colnames(label) <- c("label")

df = cbind(acc,ecg, emg, eda, temp, respi, label)
df <- cbind(df, "time" = 1:nrow(df)/700 )

#Filter only labels - baseline, stress, amusement
data <- as.data.frame(df)
data_filtered <- data %>% filter(label == "1" | label == "2" | label == "3")
View(data_filtered)

#Load only 10000 rows 
data_filtered_2000 = sample_n(data_filtered,10000)
data_filtered_2000 = select(data_filtered_2000, -time)
data_filtered_2000$label = factor(data_filtered_2000$label)

#fucntion returning the features importance
features_importance <- function(df, label){
  columnName <- label
  modelFormula <- paste(columnName, " ~ .")
  
  rf_classifier = randomForest(as.formula(modelFormula), data = df, importance = TRUE)
  
  features_value = as.data.frame(importance(rf_classifier, type=1))
  features_value = rownames_to_column(features_value)
  colnames(features_value) <- c("col", "importance")
  features_value <- features_value %>% arrange(desc(importance))
  
  return(features_value)
}

#function returning dataframe with n the most important value
select_features <- function(df,features_value, number_of_features, label){
  
  features_value_selected = head(features_value, number_of_features)
  selected_features = factor(features_value_selected$col)
  df_selected = select(df, all_of(selected_features), all_of(label))
  
  return(df_selected)
}

df_values = features_importance(data_filtered_2000, "label")
df_values

select_features(data_filtered_2000,df_values, 3, "label")

