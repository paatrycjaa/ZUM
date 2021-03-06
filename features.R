# Advanced machine learning
# Authors: Patrycja Cieplicka, Pawel Zakieta

library(tidyverse)
library(randomForest)

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
