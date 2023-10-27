# Create a function that produces the count and proportion for each unique ID grouped by a given variable

unique_case <- function(df, ID, group) {
  # Compute overall count of unique IDs
  unique_id_count_overall <- df %>% select({{ID}}) %>% n_distinct()
  
  df %>% 
    group_by({{group}}) %>% 
    drop_na({{group}}) %>%
    reframe(
      group                   = unique({{group}}),
      unique_id_count         = length(unique({{ID}})),
      unique_id_count_overall = cbind(unique_id_count_overall),
      unique_id_proportion    = format(round((unique_id_count/unique_id_count_overall), digits = 2), nsmall = 2)
    ) %>%
    select(-c(group, unique_id_count_overall))
} 
