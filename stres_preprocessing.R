# Advanced machine learning
# Authors: Patrycja Cieplicka, Pawel Zakieta

library(tidyverse)
library(reticulate)
py_install("pandas")
library(randomForest)
library(caret)
library(chron)
library(tsvectorization)


results = c()
file_indices=c(2:8,10,11,13:17)
for (file_index in file_indices){
  ## Load WESAD data for only one participant (S10) from pickle file
  pd <- reticulate::import("pandas")
  path = paste("WESAD/S",file_index,"/S",file_index,".pkl", sep="")
  s1 <- pd$read_pickle(path)
  
  #View(s1)
  
  #Create dataframe with data regarding chest signal
  s1_chest = s1$signal$chest
  s1_labels = s1$label
  
  #View(s1_chest)
  
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

  
  get_intervals <- function(signal, threshold=0.3){
    above_threshold = signal>threshold
    crosses = which((!above_threshold[1:(length(above_threshold)-1)]) * (above_threshold[2:length(above_threshold)])==1)
    if (length(crosses)<2){
      return(c())
      }
    intervals = crosses[2:length(crosses)] - crosses[1:(length(crosses)-1)]
    return(intervals)
  }
  hr_info <-function(signal){
    intervals = get_intervals(signal, 0.3)
    intervals = intervals[which((intervals<700)*(intervals>200)==1)]
    frequencies = 1/intervals * 700
    return(c(mean(frequencies), sd(frequencies)))
  }
  
  breath_info <-function(signal){
    intervals = get_intervals(signal, 0)
    intervals = intervals[which(intervals>500)]
    frequencies = 1/intervals * 700
    return(c(mean(frequencies), sd(frequencies)))
  }
  
  first<-function(signal){
    return(signal[1])
  }
  aggregations = list(X=c("mean", "sd"),
                      Y=c("mean", "sd"),
                      Z=c("mean", "sd"),
                      eda=c("mean", "sd", "min", "max"),
                      ecg=c("hr_info"),
                      emg=c("mean", "sd"),
                      resp=c("breath_info"),
                      temperature=c("mean", "sd"),
                      label=c("first")
                      )
  
  # 10s, 20s, 30s, 1m, 2m
  winow_sizes = c(7000, 14000, 21000, 42000, 84000)
  # winow_sizes = c(7000)
  for (window_size in winow_sizes){
    print(paste("processing windowsize ", window_size))
    result = vectorize2(data_filtered, window_size, window_size, aggregations)
    filename = paste("stres/S",file_index, "_window_", window_size/700, ".csv", sep = '')
    write.csv(result, filename)
  }
}