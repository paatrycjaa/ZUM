library(zoo)
#' Aggregate windows in a dataframe
#'
#' Calculates values of a set of aggregating functions for dataframe column values contained in a sliding window.
#'
#' @param dataframe Source dataframe with time series data in columns
#' @param window_size Size of the window for aggregations
#' @param slide Number of rows by which the window will slide. Use the same value as window_size for no intersections and no skipped values
#' @param aggregations list with named values. Each element of the list is a vector of strings with function names that will be applied to the column with the same name as the name of the element
#' @param all If specified, theese aggregation functions will be applied to all columns
#' @param ...
#'
#' @return Vectorized dataframe. Name of columns are extended by the aggregation function name. If aggregation function returns multiple values, columns are additionally numbered
#' @export
#'
#' @examples
#'
#' df = data.frame(1:12)
#' names(df) = c("column1")
#' aggregations <- list(column1=c("mean", "min", "max"))
#' result = vectorize(df, 3, 3, aggregations)
#'
#'   column1 mean column1 min column1 max
#'1            2           1           3
#'2            5           4           6
#'3            8           7           9
#'4           11          10          12
vectorize<-function(dataframe, window_size, slide, aggregations=list(), all=c(), replace_na=NA,...){

  if (length(all)>0){
    for(attr_name in names(dataframe)){
      aggregations[[attr_name]] = all
    }
  }
  if (!is.na(replace_na)){
    dataframe[is.na(dataframe)] = replace_na
  }
  attr_names = names(aggregations)
  result = c()
  col_idx = 1
  for (attr_name in attr_names){
    column = dataframe[attr_name]
    attr_aggreagtions = aggregations[[attr_name]]
    for (attr_aggregation_name in attr_aggreagtions){
      attr_aggregation = get(attr_aggregation_name)
      aggregated_values = data.frame(rollapply(column[,], window_size, attr_aggregation, by=slide))
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


#' Split dataframe by time
#'
#' Divides a dataset into a vector of datasets with continuous time series'.
#' Devision occurs if the sampling rate changes or suddenly changes values.
#'
#' @param dataframe Dataframe that contains time series information in a number of continuous chunks
#' @param time_column A column containing time information
#' @param min_len A minimum length of a chunk to be outputted
#' @param margin Minimum number of local periods between 2 continuous chunks needed to treat them as separate chunks
#' @param rate_rate_change_margin Minimum relative change of sample rate between 2 chunks to treat them as separate chunks
#' @param ...
#'
#' @return A vector of dataframes continuous in time and with constant rate
#' @export
#'
#' @examples
#' df = data.frame(c(1:10, seq(11,20,2), 1:10),c(1:25))
#' names(df) = c("time", "data")
#' split_dataframe_by_time(df, df$time)
#'
#' [[1]]
#'time data
#'1     1    1
#'2     2    2
#'3     3    3
#'4     4    4
#'5     5    5
#'6     6    6
#'7     7    7
#'8     8    8
#'9     9    9
#'10   10   10

#'[[2]]
#'time data
#'11   11   11
#'12   13   12
#'13   15   13
#'14   17   14
#'15   19   15

#'[[3]]
#'time data
#'16    1   16
#'17    2   17
#'18    3   18
#'19    4   19
#'20    5   20
#'21    6   21
#'22    7   22
#'23    8   23
#'24    9   24
#'25   10   25
split_dataframe_by_time <-function(dataframe, time_column, min_len=0, margin=5, rate_rate_change_margin=0.5,...){

  time_differences = as.numeric(time_column[2:length(time_column)] - time_column[1:length(time_column)-1])
  local_dt = rollapply(time_differences, 5, median, by=1, fill=c(time_differences[1], 0, time_differences[length(time_differences)]))
  rate_change = local_dt[2:length(local_dt)]-local_dt[1:(length(local_dt)-1)]
  relative_rate_change = rate_change/local_dt[1:length(rate_change)]
  margin = 5
  rate_change_margin = 0.5

  chunk_borders = append(which(abs(time_differences)>(local_dt*(1+margin))), which(abs(relative_rate_change)>(rate_change_margin)))
  chunk_borders = append(chunk_borders, length(time_differences)+1)
  chunk_borders = sort(unique(chunk_borders))

  result = c()
  chunk_begining = 1
  idx = 1
  for (i in chunk_borders){
    if (i-chunk_begining>min_len){
      result[[idx]] = dataframe[chunk_begining:i,]
      idx = idx + 1
    }
    chunk_begining = i+1
  }
  return(result)
}

count<- function(array, value){
  return (sum(array==value))
}

first<-function(signal){
  return(signal[1])
}

count_occurances<-function(values, sample){
  return(apply(values, 1, count, array=sample))
}

count_letters<-function(signal){
  return(count_occurances(data.frame(LETTERS), signal))
}

