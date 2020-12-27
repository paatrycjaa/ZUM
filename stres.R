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

#Load data for heavy drinking detection
#jesli odczyt z telefonu jest pomiedzy odczytami i i i+1 tac, to label przyjmuje wartosc tac i/ timestamp w milisekundach w pid? w target w sek?
#https://github.com/p-nath/detection_heavy__drinking_episodes/blob/master/report.pdf

tac_reading_BK7610 = read.table("clean_tac/BK7610_clean_TAC.csv", sep = ",", header = TRUE)
tac_reading_BK7610 = tac_reading_BK7610 %>% mutate(label = case_when( TAC_Reading >= 0.08 ~ 1, TAC_Reading < 0.08 ~ 0))
View(tac_reading_BK7610)

df_drinking = read.table("all_accelerometer_data_pids_13.csv", sep = ",", header = TRUE)
df_drinking_BK7610 = df_drinking %>% filter(pid == "BK7610" & time > 0) %>% arrange(time)
df_drinking_BK7610 = df_drinking_BK7610 %>% mutate(label = 2)


## add coressponding tac label to features
tt = 1
for( i in 1:dim(df_drinking_BK7610)[1]){
  completed = TRUE
  while(completed){
    if(tt > dim(tac_reading_BK7610)[1]){
      break
    }
    #print(tac_reading_BK7610$time[tt])
    #print( df_drinking_BK7610$time[i] / 1000)
    if( df_drinking_BK7610$time[i] / 1000 > tac_reading_BK7610$timestamp[tt] & df_drinking_BK7610$time[i] / 1000 < tac_reading_BK7610$timestamp[tt + 1]){
      df_drinking_BK7610$label[i] = tac_reading_BK7610$label[tt]
      completed = FALSE
    }
    else{
      tt = tt + 1
    }
  }
}

View(df_drinking_BK7610)
