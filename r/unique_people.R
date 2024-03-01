# Load relevant libraries
library(dplyr)
library(tidyverse)

# Create a function that: 
  # 1.  Adds new columns to this data set that indicate the number of times each person occurs and what the visit number is for each Medstar ID.
  # 2.  Creates a data set containing information from only the first visit for each unique person.
  # 3.  Creates a table with counts of unique names and number of names that occur once or more than once 

unique_people <- function(df) {
  # Add new columns
  df_part <- df %>%
    group_by(unique_id_par) %>%
    mutate(
      # Column that shows the number of times each person occurs
      count               = n()
    ) %>%
    group_by(x_created_timestamp) %>%
    arrange() %>%
    ungroup() %>%
    group_by(unique_id_par) %>%
    mutate(
      # Column that gives each visit a number that represents the chronological order
      visit_no = row_number()
    ) %>%
    ungroup()
  # Create data frame that contains only the details of the first visit for each person.
  df_unique <- df_part %>% filter(visit_no == 1)
  
  # Create data frame that contains only information for people who appear more than once.
  df_not_unique <- df_part %>% filter(count > 1)
  
  df_unique_summary <- df_part %>%
    reframe(
      Unique_people               = length(unique_id_par[visit_no == 1]),
      People_with_only_one_visit  = length(unique_id_par[count == 1]),
      People_with_multiple_visits = length(unique(unique_id_par[count  > 1])),
      Total_number_of_rows = length(unique_id_par)
      ) %>% 
    t() %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    mutate(
      rowname = gsub("_", " ", rowname)
    )
  
  output  <- list(df_count_visit_no = df_part, df_first_visit_only = df_unique, df_not_unique = df_not_unique, 
                  df_unique_summary = df_unique_summary)
  
}


# test <- unique_people(soc_dem)
# one <- test$df_unique_summary
