library(tidyverse)
library(randomForest)
library(caret)
library(chron)
library(tsvectorization)


# VECTORIZATION

dates = t(as.data.frame(strsplit(df_energy$date, ' ')))
dates = as.numeric(chron(dates[,1], format=c('y-m-d')))
test_percentage = 0.2
test_dates = sample(unique(dates), length(unique(dates))*test_percentage)

get_day <- function(values){
  date_start_str = data.frame(strsplit(values[1], ' '))[[1,1]]
  date_end_str = data.frame(strsplit(values[length(values)], ' '))[[1,1]]
  date_start = chron(date_start_str, format=c('y-m-d'))
  date_end = chron(date_end_str, format=c('y-m-d'))
  if (date_start == date_end){
    return(as.numeric(date_start))
  }
  return(0)
}

functions = list(mean=mean, min=min, max=max, sd=sd, q=quantile, sum=sum, day=get_day)

aggregations = list(Appliances=c("sum", "mean", "max"),
                    lights=c("mean", "sd", "quantile"),
                    T1 = c("mean", "sd", "quantile"),
                    T2 = c("mean", "sd", "quantile"),
                    T3 = c("mean", "sd", "quantile"),
                    T4 = c("mean", "sd", "quantile"),
                    T5 = c("mean", "sd", "quantile"),
                    T6 = c("mean", "sd", "quantile"),
                    T7 = c("mean", "sd", "quantile"),
                    T8 = c("mean",  "sd", "quantile"),
                    T9 = c("mean", "sd", "quantile"),
                    T_out=c("mean", "sd", "quantile"),
                    RH_1 = c("mean", "sd", "quantile"),
                    RH_2 = c("mean", "sd", "quantile"),
                    RH_3 = c("mean", "sd", "quantile"),
                    RH_4 = c("mean", "sd", "quantile"),
                    RH_5 = c("mean", "sd", "quantile"),
                    RH_6 = c("mean", "sd", "quantile"),
                    RH_7 = c("mean", "sd", "quantile"),
                    RH_8 = c("mean", "sd", "quantile"),
                    RH_9 = c("mean", "sd", "quantile"),
                    RH_out=c("mean", "sd", "quantile"),
                    Press_mm_hg=c("mean", "sd", "quantile"),
                    Windspeed=c("mean", "sd", "quantile"),
                    Visibility=c("mean", "sd", "quantile"),
                    Tdewpoint=c("mean", "sd", "quantile"),
                    rv1=c("mean", "sd", "quantile"),
                    rv2=c("mean", "sd", "quantile"),
                    date=c("day")
                    )
# aggregations = list(date=c("date"))
aggregated_df3 = vectorize(df_energy, 3, 3, aggregations)
aggregated_df3_test = aggregated_df3[which(aggregated_df3$`date day` %in% test_dates),]
aggregated_df3_train = aggregated_df3[which(!(aggregated_df3$`date day` %in% test_dates)),]
write.csv(aggregated_df3_train, "aggregated_df3_train.csv")
write.csv(aggregated_df3_test, "aggregated_df3_test.csv")

aggregated_df6 = vectorize(df_energy, 6, 6, aggregations)
aggregated_df6_test = aggregated_df6[which(aggregated_df6$`date day` %in% test_dates),]
aggregated_df6_train = aggregated_df6[which(!(aggregated_df6$`date day` %in% test_dates)),]
write.csv(aggregated_df6_train, "aggregated_df6_train.csv")
write.csv(aggregated_df6_test, "aggregated_df6_test.csv")

aggregated_df12 = vectorize(df_energy, 12, 12, aggregations)
aggregated_df12_test = aggregated_df12[which(aggregated_df12$`date day` %in% test_dates),]
aggregated_df12_train = aggregated_df12[which(!(aggregated_df12$`date day` %in% test_dates)),]
write.csv(aggregated_df12_train, "aggregated_df12_train.csv")
write.csv(aggregated_df12_test, "aggregated_df12_test.csv")

aggregated_df24 = vectorize(df_energy, 24, 24, aggregations)
aggregated_df24_test = aggregated_df24[which(aggregated_df24$`date day` %in% test_dates),]
aggregated_df24_train = aggregated_df24[which(!(aggregated_df24$`date day` %in% test_dates)),]
write.csv(aggregated_df24_train, "energy_win24_train.csv")
write.csv(aggregated_df24_test, "energy_win24_test.csv")



aggregations = list(Appliances=c("sum", "mean", "max"),
                    date=c("day")
)
real_values = vectorize(df_energy, 144, 144, aggregations)
real_values_test = real_values[which(real_values$`date day` %in% test_dates),]
real_values_train = real_values[which(!(real_values$`date day` %in% test_dates)),]
write.csv(real_values_test, "real_values_train.csv")
write.csv(real_values_train, "real_values_test.csv")




