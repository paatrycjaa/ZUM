library(tidyverse)
library(randomForest)
library(caret)
library(chron)
source ("vectorize.R") 

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


df_energy = read.csv("energydata_complete.csv", sep =",", header = TRUE)
# cropping out to full days
df_energy = df_energy[43:19626,]
df_energy_filtered = select(sample_n(df_energy,nrow(df_energy)), -date)
df_energy_scaled = data.frame(scale(df_energy_filtered))
df_values = features_importance(df_energy_filtered,"Appliances" )

# 1       lights  94.916009
# 2  Press_mm_hg  53.632412
# 3         RH_5  49.038540
# 4         RH_8  44.190941
# 5         RH_9  40.463952
# 6         RH_1  39.401690
# 7         RH_3  38.546811
# 8           T5  37.238943
# 9       RH_out  36.712892
# 10          T8  36.548204
# 11          T2  35.968342
# 12  Visibility  35.933638
# 13          T7  34.815275
# 14   Tdewpoint  34.470642
# 15          T3  33.954845
# 16        RH_7  33.530282
# 17        RH_6  32.720062
# 18          T1  32.443242
# 19       T_out  31.849345
# 20        RH_4  30.747574
# 21          T4  29.885264
# 22          T9  28.063390
# 23        RH_2  27.804554
# 24   Windspeed  27.473486
# 25          T6  27.435485
# 26         rv2   7.501337
# 27         rv1   7.020106

# VECTORIZATION

dates = t(as.data.frame(strsplit(df_energy$date, ' ')))
dates = as.numeric(chron(dates[,1], format=c('y-m-d')))
test_percentage = 0.2
test_dates = sample(unique(dates), length(unique(dates))*test_percentage)

get_day <- function(values){
  #print(values[1])
  #print(values[length(values)])
  date_start_str = data.frame(strsplit(values[1], ' '))[[1,1]]
  date_end_str = data.frame(strsplit(values[length(values)], ' '))[[1,1]]
  date_start = chron(date_start_str, format=c('y-m-d'))
  date_end = chron(date_end_str, format=c('y-m-d'))
  #print(date_start)
  #print(date_end)
  if (date_start == date_end){
    return(as.numeric(date_start))
  }
  return(0)
}

functions = list(mean=mean, min=min, max=max, sd=sd, q=quantile, sum=sum, date=get_date)

aggregations = list(Appliances=c("sum", "mean"),
                    lights=c("mean", "sd", "q"), 
                    T1 = c("mean", "sd", "q"),
                    T2 = c("mean", "sd", "q"),
                    T3 = c("mean", "sd", "q"),
                    T4 = c("mean", "sd", "q"),
                    T5 = c("mean", "sd", "q"),
                    T6 = c("mean", "sd", "q"),
                    T7 = c("mean", "sd", "q"),
                    T8 = c("mean",  "sd", "q"),
                    T9 = c("mean", "sd", "q"),
                    T_out=c("mean", "sd", "q"),
                    RH_1 = c("mean", "sd", "q"),
                    RH_2 = c("mean", "sd", "q"),
                    RH_3 = c("mean", "sd", "q"),
                    RH_4 = c("mean", "sd", "q"),
                    RH_5 = c("mean", "sd", "q"),
                    RH_6 = c("mean", "sd", "q"),
                    RH_7 = c("mean", "sd", "q"),
                    RH_8 = c("mean", "sd", "q"),
                    RH_9 = c("mean", "sd", "q"),
                    RH_out=c("mean", "sd", "q"),
                    Press_mm_hg=c("mean", "sd", "q"),
                    Windspeed=c("mean", "sd", "q"),
                    Visibility=c("mean", "sd", "q"),
                    Tdewpoint=c("mean", "sd", "q"),
                    rv1=c("mean", "sd", "q"),
                    rv2=c("mean", "sd", "q"),
                    date=c("date")
                    )
# aggregations = list(date=c("date"))
aggregated_df3 = vectorize(df_energy, 3, aggregations, functions)
write.csv(aggregated_df3, "energy_win3.csv")
aggregated_df6 = vectorize(df_energy, 6, aggregations, functions)
write.csv(aggregated_df6, "energy_win6.csv")
aggregated_df12 = vectorize(df_energy, 12, aggregations, functions)
write.csv(aggregated_df12, "energy_win12.csv")

aggregated_df24 = vectorize(df_energy, 24, aggregations, functions)
aggregated_df24_test = aggregated_df24[which(aggregated_df24$`date date` %in% test_dates),]
aggregated_df24_train = aggregated_df24[which(!(aggregated_df24$`date date` %in% test_dates)),]
write.csv(aggregated_df24, "energy_win24.csv")




