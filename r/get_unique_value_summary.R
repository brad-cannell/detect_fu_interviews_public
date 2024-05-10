get_unique_value_summary <- function(df,cols){
  
  # Input: 
  #     df (data frame) - original source data frame
  #     cols (list) - list of target column names as strings
  # Output:
  #     unique_summary (data frame) - summary counts of each unique value in
  #           each of the target columns
  
  # Get list of unique values in all target columns
  
  val <- unique(as.factor(as.vector(as.matrix(df[cols]))))
  
  # Initialize output data frame with unique value row
  
  unique_summary <- data.frame("value"=val)
  
  # Get counts of unique values in original data frame
  
  for (i in cols){
    
    # utilizes table to get summary count of each column
    
    table <- as.data.frame(table(df[i]))
    
    # sets column names to "value" and "freq"
    
    colnames(table) <- c("value","freq")
    
    # adds count of missing values in each column
    
    table<- add_row(table, value = NA, freq = sum(is.na(df[i])))
    
    # readjusts names of columns to "value" and the name of the target column
    
    colnames(table) <- c("value",i)
    
    # joins table's summary counts to complete the count values
    
    unique_summary <- left_join(unique_summary,table, by="value")
  }
  
  # returns completed, but unordered, data frame
  
  unique_summary
}