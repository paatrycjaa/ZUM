# Advanced machine learning
# Authors: Patrycja Cieplicka, Pawel Zakieta

#Load data for heavy drinking detection
library(tsvectorization)
library(tidyverse)
library(reticulate)
library(caret)

df_drinking = read.table("all_accelerometer_data_pids_13.csv", sep = ",", header = TRUE)
for (pid_value in unique(df_drinking$pid)){
  filename = paste("drinking/pid", pid_value, ".csv", sep='')
  print(pid_value)
  data = df_drinking %>% filter(pid == pid_value & time > 0) %>% arrange(time)
  data = data %>% mutate(label = 2)
  write.csv(data, filename)
}

insert_labels<-function(dataset, tac_reading){
  previous_window_ends = 0
  for(i in 1:dim(tac_reading)[1]){
    reading_time = tac_reading$timestamp[i]*1000
    last = which(dataset$time>reading_time)[1]
      if (is.na(last) || last>=length(dataset$label)){
        dataset$label[(previous_window_ends+1) : length(dataset$label)] = tac_reading$label[i]
        break
      }
      dataset$label[(previous_window_ends+1) : last] = tac_reading$label[i]
      previous_window_ends = last

  }
  if (previous_window_ends<length(dataset$label)){
    dataset$label[(previous_window_ends+1) : length(dataset$label)] = tac_reading$label[dim(tac_reading)[1]]
  }
  return(dataset)
}

spectrum_info<-function(signal){
  spectrum = abs(fft(signal))
  return(c(mean(spectrum), sd(spectrum)))
}

aggregations = list(x=c("mean", "sd", "median", "max", "min", "spectrum_info"),
                    y=c("mean", "sd", "median", "max", "min", "spectrum_info"),
                    z=c("mean", "sd", "median", "max", "min", "spectrum_info"),
                    label=c("first")
)

pids = unique(df_drinking$pid)
#pids = pids[1:1]
for (pid in pids){
  print(paste("reading", pid))
  data_filename = paste("drinking/pid", pid, ".csv", sep='')
  labels_filename = paste("clean_tac/", pid, "_clean_TAC.csv", sep='')
  df_data = read.csv(data_filename)
  df_labels = read.table(labels_filename, sep = ",", header = TRUE)
  df_labels = df_labels %>% mutate(label = case_when( TAC_Reading >= 0.08 ~ 1, TAC_Reading < 0.08 ~ 0))
  df_data = insert_labels(df_data, df_labels)
  print(unique(df_data$label))
  dt = median(df_data$time[2:10001] - df_data$time[1:10000])
  #print(dt)
  window_size = as.integer(1000/dt)
  vectorized = vectorize(df_data, window_size, window_size, aggregations)
  #vectorized_labels = vectorize2(df_data, window_size, window_size, list(label=c("first")))
  vectorized2 = vectorize(vectorized, 10, 10, all=c("mean", "sd", "max", "min"))
  #vectorized2_labels = vectorize2(vectorized_labels, 10, 10, all=c("first"))
  filename = paste("drinking/windowed/pid", pid, ".csv", sep='')
  filename = paste("drinking/windowed/label_pid", pid, ".csv", sep='')
  write.csv(vectorized_labels, filename)
  filename = paste("drinking/2windowed/pid", pid, ".csv", sep='')
  filename = paste("drinking/2windowed/label_pid", pid, ".csv", sep='')
  write.csv(vectorized2_labels, filename)
  print(paste("read", pid))
}



