# Panelist multiple assessment data error problem description

## There is an error in the LEAD panel assessment data where there are multiple rows of data for certain panelists in the same assessment 
## of the same participant.
## For instance, medstar_id 47311d550da4471297501ae2b3b03b02 has multiple rows with Jason Burnett as the LEAD panelist in the initial assessment
## within a short time frame. 

# Identify Medstar IDs with data error and remove rows based on certain criteria.

## Create a function that identifies the specific issues, indicates whether a row should be excluded or not and then removes the identified rows based
## on the following criteria:
## 1. Identical duplicates: Keep the most recent row.
## 2. Additional entries with all values missing : Remove rows with all missing vote values
## 3. Additional entries where not all values are missing: Keep only the most recent

error_fix <- function(lead_panel_assessment_df) {
  
  # Create a vector of columns that will be used for grouping below.
  group <- c('assessment_type_3cat_f', 'physical_abuse_2cat', 'sexual_abuse_2cat', 'emotional_psycho_abuse_2cat', 'neglect_2cat', 
             'self_neglect_2cat', 'financial_exploitation_2cat', 'abandonment_2cat')
  
  # Add a column to the data frame -- multiple_count -- that is equal to the number of rows in the data frame for each combination of medstar ID 
  # and panelist name.
  multiple_assessment <- lead_panel_assessment_df %>%
    group_by(medstar_id, panelist_name_10cat_f, assessment_type_3cat_f) %>% 
    mutate(
      multiple_count = length(panelist_name_10cat_f)
    ) %>%
    ungroup()
  
  # For each MedStar ID, are all the scores assigned by the same panelist identical or different (identifies whether or not there is more than one 
  # row per group).
  multiple_assessment <- multiple_assessment |> 
    group_by(
      medstar_id, panelist_name_10cat_f, assessment_type_3cat_f, sexual_abuse_2cat, emotional_psycho_abuse_2cat, neglect_2cat,
      self_neglect_2cat, financial_exploitation_2cat, abandonment_2cat
    ) %>%
    mutate(
      same_score = case_when(
        multiple_count   >1 & n() > 1  ~ TRUE,
        multiple_count   >1 & n() == 1 ~ FALSE,
        TRUE                           ~ NA 
      )
    ) %>%
    ungroup()
  
  # Are all, some. or none of the assessment values missing for the specified row?
  multiple_assessment <- multiple_assessment |> 
    mutate(
      all_na = case_when(
        if_all(all_of(group), ~ is.na(.))  ~ "all",
        if_all(all_of(group), ~ !is.na(.)) ~ "none",
        TRUE                               ~ "some"
      )
    ) %>%
    ungroup()
  
  # For each MedStar ID with multiple assessments for the same panelist, were 
  # the assessments performed within one day of each other?
  multiple_assessment <- multiple_assessment |>
    group_by(medstar_id, panelist_name_10cat_f, assessment_type_3cat_f) %>%
    mutate(
      over_24_hours = ifelse(
        (as.numeric(difftime(last(x_created_timestamp), first(x_created_timestamp), units = "hours")) > 24), 1, 0
      )
    )
  
  # For each MedStar ID with multiple assessments for the same panelist, was the entry for the specified row the most recent?
  multiple_assessment <- multiple_assessment |>
    mutate(
      most_recent = ifelse(x_created_timestamp == max(x_created_timestamp), "Yes", "No")
    )
  
  # Should this row be removed or is further review required to determine this 
  # based on the rules below?
  multiple_assessment <- multiple_assessment |>
    mutate(
      remove  = case_when(
        # Case when the error is present and the scores are not identical. All of the rows being compared have one or more missing
        # scores and at least one of the rows being compared does not have all of the scores missing. Marks such cases
        # for individual review
        multiple_count > 1 & same_score == FALSE & all(all_na != "none") & any(all_na != "all") ~ "Individual review",
        
        # Case when the error is present, the scores are not identical and at least one of the rows being compared has some but not all 
        # scores missing. Any of the other rows being compared could have none or all missing values. Marks such cases for
        # individual reveiw.
        multiple_count > 1 & same_score == FALSE & any(all_na == "some")                        ~ "Individual review",
        
        # Case when the error is present, the scores are not identical and all the rows being compared have no missing scores.
        multiple_count > 1 & same_score == FALSE & all(all_na == "none")                        ~ "Individual review",
        
        # case when there is no error
        multiple_count == 1                                                                     ~ "No",
        
        # Marks only the most recent of identical rows where error is present for keeping while the other rows are removed.
        multiple_count > 1 & same_score == TRUE & most_recent == "Yes"                          ~ "No",
        
        # Case when there is an error and the scores being compared are not the same. Marks rows where there are no missing scores
        # for keeping
        multiple_count > 1 & same_score == FALSE & all_na == "none"                             ~ "No",
        
        # Case when error is present but all the rows are identical in terms of assigned scores. Marks rows that are not the most
        # recent for removal
        multiple_count > 1 & same_score == TRUE & most_recent == "No"                           ~ "Yes",
        
        # Case when the error is present and the scores are not identical. Marks rows where all the scores are missing for removal
        multiple_count > 1 & same_score == FALSE & all_na == "all"                              ~ "Yes"
      )
    ) %>% 
    ungroup()
  # Remove rows marked by the error_check function for removal.
  lead_panel_clean <-  multiple_assessment %>% filter(!(remove == "Yes")) 
  
  # Remove rows marked by the error_check function for individual review that are not the most recent.
  lead_panel_clean <-  lead_panel_clean %>% filter(!(remove == "Individual review" & most_recent == "No") | remove == "No")
}