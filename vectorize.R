library(ramify)
library (roxygen2)
library(purrr)

letters <- LETTERS[1:24]
numbers <- 1:24
numbers_random <- runif(24,0,1.0)

df = data.frame(letters, numbers, numbers_random);
# df = data.frame(letters, numbers);

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
        aggregated_values = t(aggregated_values)}
      if (length(result)>0){
        result = cbind(result, aggregated_values)
      }
      else{
        result = data.frame(aggregated_values)
      }
      for (i in 1:ncol(aggregated_values)){
        colnames(result)[col_idx] <- paste(attr_name, attr_aggregation_name)
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
aggregations<-list(letters = list("count"), numbers=list("mean", "sum"), numbers_random=list("mean", "sum"))
result = vectorize(df, 3, aggregations, functions)
a = 1
