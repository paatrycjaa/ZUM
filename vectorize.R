library(ramify)
library(purrr)

letters <- LETTERS[1:24]
numbers <- 1:24
numbers_random <- runif(24,0,1.0)

df = data.frame(letters, numbers, numbers_random);
# df = data.frame(letters, numbers);


# SPLITS DATAFRAME INTO A NUMBER OF SMALLER DATAFRAMES WITH CONTINUOUS DATA
split_dataframe <-function(dataframe, time_column){
  #datetimes = t(as.data.frame(strsplit(dataframe[[time_column_name]], ' ')))
  #datetimes = chron(datetimes[,1], datetimes[,2], format=c('y-m-d','h:m:s'))
  #time_differences = as.numeric(datetimes[2:length(datetimes)] - datetimes[1:length(datetimes)-1])
  time_differences = as.numeric(time_column[2:length(datetimes)] - time_column[1:length(time_column)-1])
  dt = as.numeric(names(sort(table(time_differences),decreasing=TRUE)[1]))
  margin = 0.2
  chunk_borders = append(which(time_differences>dt*(1+margin)), length(time_differences)+1)
  result = c()
  chunk_begining = 1
  idx = 1
  for (i in chunk_borders){
    result[[idx]] = dataframe[chunk_begining:i-1,]
    idx = idx + 1
    chunk_begining = i
  }
  return(result)
}

vectorize<-function(dataframe, window_size, aggregations, functions){
  attr_names = names(aggregations)
  result = c()
  col_idx = 1
  for (attr_name in attr_names){
    print(attr_name)
    column = dataframe[attr_name]
    windowed = resize(column, nrow(column)/window_size, window_size, byrow=TRUE)
    attr_aggreagtions = aggregations[[attr_name]]
    for (attr_aggregation_name in attr_aggreagtions){
      attr_aggregation = functions[[attr_aggregation_name]]
      aggregated_values = data.frame(apply(windowed, 1, attr_aggregation))
      if (ncol(aggregated_values)>1){
        aggregated_values = t(aggregated_values)
      }
      if (length(result)>0){
        result = cbind(result, aggregated_values)
      }
      else{
        result = data.frame(aggregated_values)
      }
      for (i in 1:ncol(aggregated_values)){
        if (ncol(aggregated_values)>1) colnames(result)[col_idx] <- paste(attr_name, attr_aggregation_name, i)
        else colnames(result)[col_idx] <- paste(attr_name, attr_aggregation_name)
        col_idx = col_idx + 1
      }
    }
  }
  return(result)
}

count<- function(array, value){
  return (sum(array==value))
}

count_occurances<-function(values, sample){
  return(apply(values, 1, count, array=sample))
}

# names of functions used to describe columns
functions <- list(sum=sum, mean=mean, nic=multi_fun, count=partial(count_occurances, values=data.frame(unique(df$letters))))

# Assigning function abbreviations to each attribute
#aggregations<-list(letters=list("frequent", "frequent"), numbers = list("sum", "mean", "nic"))
aggregations<-list(letters = c("count"), numbers=c("mean", "sum"), numbers_random=c("mean", "sum"))
result = vectorize(df, 3, aggregations, functions)
a = 1
