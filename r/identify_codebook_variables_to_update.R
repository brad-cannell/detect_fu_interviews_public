# The purpose of the function in this file is to compare the variables included in the last run of the codebook file to the variables in the 
# last run of the data cleaning file so it's easier to determine which variables need to be included and which need to be removed from the codebook.

vars_to_update <- function(df, desc_rds_file_name){
  # Load variable description file
  
  desc_file_path <- here::here("codebooks", "variable_descriptions", {{desc_rds_file_name}})
  var_desc <- readr::read_rds(desc_file_path) 
  
  # Create list of variable names in previous version of the code book
  var_names_cb <- var_desc[['name']] %>% sort()
  
  # Create list of variable names in current version of the the cleaned data set
  var_names_df <- df %>% names() %>% sort()
  
  # Get list of values that differ between the two lists
  old <- setdiff(var_names_cb, var_names_df) %>% as.data.frame() # in old, not in new
  new <- setdiff(var_names_df, var_names_cb) %>% as.data.frame() # in new, not in old
  out <- list(var_names_cb, var_names_df, old, new)
  return(out)
}