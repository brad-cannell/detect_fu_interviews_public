# Create a data frame that summarizes the lead panel data by including columns that indicate:
# - The total number of positive votes
# - The proportion of positive votes
# - Whether there are any positive votes for each type of abuse for each assessment.
# - Whether there are any positive votes across all subtypes of abuse for each assessment.
# - The final LEAD Assessment determination based on majority vote for each assessment.

## Create summary dataframes for each assessment within the LEAD panel assessment data that include calculated columns indicating the total number of 
## positive votes, the proportion of positive votes, whether or not there were any positive determinations at each assessment for each MedStar ID.

pos_votes <- function(lead_panel_cleaned) {
  pos_votes <- lead_panel_cleaned %>% 
    group_by(medstar_id, assessment_type_3cat_f) %>% 
    reframe(
      # compute the sum of positive votes for each MedStar ID in each assessment
      across(
        .cols  = c(physical_abuse_2cat : abandonment_2cat, xc_assessment_screened_2cat),
        .fns   = ~ sum(.x),
        .names = "{col}_t"
      ),
      assessment_type_3cat_f  = unique(assessment_type_3cat_f),
      # compute the proportion of positive votes for each MedStar ID
      across(
        .cols  = physical_abuse_2cat_t : xc_assessment_screened_2cat_t,
        .fns   = ~ case_when(
          .x   == 0  ~ 0,
          .x  !=  0  ~ .x/n()
        ),
        .names = "{col}_p"
      )
    ) %>%
    # rename the new columns
    rename_with(
      .cols   = ends_with("_t"), 
      .fn     = ~ gsub("2cat_t", "total", .x)
    ) %>% 
    rename_with(
      .cols   = ends_with("_p"), 
      .fn     = ~ gsub("2cat_t_p", "prop", .x)
    )
  
  # Create a dichotomous variable that indicates if there were _any_ positive determinations at each assessment (initial, secondary, and post-DETECT) 
  # for each subtype of abuse.
  any_pos <- pos_votes %>% 
    group_by(assessment_type_3cat_f) %>%
    mutate(
      across(
        .cols   = physical_abuse_total : xc_assessment_screened_total,
        .fn     = ~ case_when(
          .x    == 0 ~ 0,
          .x    >  0 ~ 1
        ),
        .names  = "{col}_any"
      ),
      across(
        .cols   = ends_with("_any"),
        .fns    = ~ factor(.x, 
                           levels = c(0,1),
                           labels = c("No", "Yes")),
        .names = "{col}"
      )
    ) %>%
    rename_with(
      .cols   = ends_with("_any"), 
      .fn     = ~ gsub("total_any", "any", .x)
    ) %>%
    ungroup()
  
  # Create columns that indicate the final LEAD Assessment determination based on majority vote. 
  final_det <- any_pos %>% 
    group_by(assessment_type_3cat_f) %>%
    mutate(
      across(
        .cols   = physical_abuse_prop : xc_assessment_screened_prop,
        .fn     = ~ case_when(
          .x    <= 0.5         ~ 0,
          .x    >  0.5         ~ 1
        ),
        .names  = "{col}_det"
      ),
      across(
        .cols   = ends_with("_det"),
        .fns    = ~ factor(.x, 
                      levels = c(0,1),
                      labels = c("No", "Yes")),
      .names = "{col}"
      )
    ) %>%
    rename_with(
      .cols   = ends_with("_det"), 
      .fn     = ~ gsub("prop_det", "det", .x)
    ) %>%
      ungroup()

    # Create a dichotomous variable that indicates if there were _any_ positive determinations at each assessment (initial, secondary, and post-DETECT) 
    # across _all_ subtypes of EM.
    abuse_any <- final_det %>%
      group_by(assessment_type_3cat_f) %>%
      mutate(
        abuse_any = case_when(
          if_any(physical_abuse_det : abandonment_det, ~. == "Yes")  ~ 1,
          if_all(physical_abuse_det : abandonment_det, ~. == "No")   ~ 0,
          TRUE                                                       ~ NA
        )
      ) %>%
      mutate(
        abuse_any = factor(
          abuse_any,
          levels = c(0, 1),
          labels = c("No", "Yes")
        )
      ) %>%
      ungroup()
    abuse_any
}
