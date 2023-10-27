# Load relevant libraries
library(dplyr)
library(tidyverse)

# Create a function that: 
  # 1.  Merges identifying information from the participant dataset to the a dataset with non-unique people to create a new dataset.
  # 2.  Adds new columns to this data set that indicate the number of times each person occurs and what the visit number is for each Medstar ID.
  # 3.  Creates a data set containing information from only the first visit for each unique person.
  # 3.  Creates a table with counts of unique names and number of names that occur once or more than once 

unique_people <- function(df) {
  ## Select the relevant variables from the participant dataset
  part_id <- participant %>% 
    select(medstar_id, name_first, name_middle_initial, name_last, dob, sex_3cat_f, x_address_original)
  
  # Merge name columns into new full name column in the participant dataset that does not include the middle initial.
  part_id <- part_id %>% unite("name_full", c('name_first', 'name_last'), sep = ' ', na.rm = TRUE, remove = F) %>% relocate(name_full, .after = name_last)
  
  # Create a street number column that will be one of the variables used in identifying unique people.
  # part_id <- part_id %>% extract(x_address_original, c('house_number'), regex = "(\\d+)", remove = FALSE) 
  
  # Remove the name_full variable so it can be replaced with a new name variable from the participant dataset  since some of the names are 
  # missing from the other DETECT F/U datasets.
  df<- df %>% select(-c(name_full))
  
  # Merge the data sets
  df_part <- left_join(df, part_id, by = 'medstar_id')
  
  # Add new columns
  df_part <- df_part %>%
    group_by(name_full, dob, sex_3cat_f
             #, house_number
             ) %>%
    mutate(
      # Column that shows the number of times each person occurs
      count               = n()
    ) %>%
    group_by(x_created_timestamp) %>%
    arrange() %>%
    ungroup() %>%
    group_by(name_full, dob, sex_3cat_f
             #, house_number
             ) %>%
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
      Unique_people               = length(name_full[visit_no == 1]),
      People_with_only_one_visit  = length(name_full[count == 1]),
      People_with_multiple_visits = length(unique(name_full[count  > 1])),
      Total_number_of_rows = length(name_full)
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
