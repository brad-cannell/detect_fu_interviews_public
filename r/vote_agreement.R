# A function that generates variables that indicate whether or not each vote for each case agrees with other votes for the same case.
# The MedStar IDs that did not have voter congruence are identified by comparing the number of observations when the data is grouped by 
# medstar_id and the abuse columns to when the data is only grouped by medstar_id.

panelist_vote_agreement <- function(lpac){
  # lpac is the lead panel assessment dataset after the data error has been fixed using the fix_lead_data_error function
  vote_agreement <- lpac %>%
    
    # Create variable that indicates for each row within each case, how many rows in total have that exact same voting pattern across all abuse types.
    group_by(
      assessment_type_3cat_f, medstar_id, physical_abuse_2cat, sexual_abuse_2cat, emotional_psycho_abuse_2cat, neglect_2cat,
      self_neglect_2cat, financial_exploitation_2cat, abandonment_2cat
    ) %>%
    mutate(
      identical_all_length = n()
    ) %>%
    ungroup() %>%
    # Create a variable that indicates the number of total voters per MedStar ID
    group_by(assessment_type_3cat_f, medstar_id) %>%
    mutate(
      voter_length = n() 
    )%>%
    # Create a variable that indicates whether or not there is 100% congruence in votes for each case.
    mutate(
      congruent_votes = case_when(
        identical_all_length == voter_length  ~ 1,
        identical_all_length != voter_length  ~ 0
      )
    ) %>%
    
    # Create a variable that indicates for each row within each case, how many total rows contain the same physical abuse vote.
    group_by(assessment_type_3cat_f, medstar_id, physical_abuse_2cat) %>%
    mutate(
      identical_phys_length = n() 
    )%>%
    ungroup() %>%
    # Create a variable that indicates whether or not each physical abuse vote is in agreement with half or more of all the votes for each case.
    mutate(
      phys_agree = case_when(
        identical_phys_length >= 0.5*voter_length  ~ 1,
        identical_phys_length < 0.5*voter_length  ~ 0
      )
    ) %>% 
    
    # Create a variable that indicates for each row within each case, how many total rows contain the same sexual abuse vote.
    group_by(assessment_type_3cat_f, medstar_id, sexual_abuse_2cat) %>%
    mutate(
      identical_sex_length = n() 
    )%>%
    ungroup() %>%
    # Create a variable that indicates whether or not each sexual abuse vote is in agreement with half or more of all the votes for each case.
    mutate(
      sex_agree = case_when(
        identical_sex_length >= 0.5*voter_length  ~ 1,
        identical_sex_length < 0.5*voter_length  ~ 0
      )
    ) %>% 
    
    # Create a variable that indicates for each row within each case, how many total rows contain the same emotional-psychological abuse vote.
    group_by(assessment_type_3cat_f, medstar_id, emotional_psycho_abuse_2cat) %>%
    mutate(
      identical_emo_length = n() 
    )%>%
    ungroup() %>%
    # Create a variable that indicates whether or not each emotional-psychological abuse vote is in agreement with half or more of all the votes for each case.
    mutate(
      emo_agree = case_when(
        identical_emo_length >= 0.5*voter_length  ~ 1,
        identical_emo_length < 0.5*voter_length  ~ 0
      )
    ) %>% 
    
    # Create a variable that indicates for each row within each case, how many total rows contain the same neglect abuse vote.
    group_by(assessment_type_3cat_f, medstar_id, neglect_2cat) %>%
    mutate(
      identical_neg_length = n() 
    )%>%
    ungroup() %>%
    # Create a variable that indicates whether or not each neglect vote is in agreement with half or more of all the votes for each case.
    mutate(
      neg_agree = case_when(
        identical_neg_length >= 0.5*voter_length  ~ 1,
        identical_neg_length < 0.5*voter_length  ~ 0
      )
    ) %>% 
    
    # Create a variable that indicates for each row within each case, how many total rows contain the same self-neglect vote.
    group_by(assessment_type_3cat_f, medstar_id, self_neglect_2cat) %>%
    mutate(
      identical_self_length = n() 
    )%>%
    ungroup() %>%
    # Create a variable that indicates whether or not each self-neglect vote is in agreement with half or more of all the votes for each case.
    mutate(
      self_agree = case_when(
        identical_self_length >= 0.5*voter_length  ~ 1,
        identical_self_length < 0.5*voter_length  ~ 0
      )
    ) %>% 
    
    # Create a variable that indicates for each row within each case, how many total rows contain the same financial abuse vote.
    group_by(assessment_type_3cat_f, medstar_id, financial_exploitation_2cat) %>%
    mutate(
      identical_fin_length = n() 
    )%>%
    ungroup() %>%
    # Create a variable that indicates whether or not each financial exploitation vote is in agreement with half or more of all the votes for each case.
    mutate(
      fin_agree = case_when(
        identical_fin_length >= 0.5*voter_length  ~ 1,
        identical_fin_length < 0.5*voter_length  ~ 0
      )
    ) %>%
    
    # Create a variable that indicates for each row within each case, how many total rows contain the same financial abuse vote.
    group_by(assessment_type_3cat_f, medstar_id, abandonment_2cat) %>%
    mutate(
      identical_aban_length = n() 
    )%>%
    ungroup() %>%
    # Create a variable that indicates whether or not each abandonment vote is in agreement with half or more of all the votes for each case.
    mutate(
      aban_agree = case_when(
        identical_aban_length >= 0.5*voter_length  ~ 1,
        identical_aban_length < 0.5*voter_length  ~ 0
      )
    )
  
  # A function that will generate a summary dataframe containing the vote counts for each panelist discipline under 
  # each abuse type as well as the overall vote counts for each discipline across all abuse types.
  
  
  
  discipline_vote_agreement <- function(abuse_agree, a_type) {
    # Create frequency tables with the panelist discipline and each abuse agreement variables. Convert tables to data frames
    tab <- as.data.frame(table(vote_agreement$panelist_discipline_5cat_f, vote_agreement$assessment_type_3cat_f, vote_agreement[[abuse_agree]]))
    
    # Make data frames tidy
    df <- tab %>%
      pivot_wider(names_from = Var3, values_from = Freq) %>%
      rename(
        panelist_discipline_5cat_f = Var1, 
        n_dissenting_votes     = "0",
        n_non_dis_votes        = "1",
        assessment_type_3cat_f = Var2
      ) %>%
      mutate(
        n_votes =  (n_dissenting_votes + n_non_dis_votes) 
      ) %>%
      select(-c(n_non_dis_votes)) %>%
      mutate(
        abuse_type = a_type
      )
  }
  
  # Physical abuse
  phys_disc <- discipline_vote_agreement("phys_agree", "Physical Abuse")
  
  # Sexual abuse
  sex_disc <- discipline_vote_agreement("sex_agree", "Sexual Abuse")
  
  # Emotional-psychological abuse
  emo_disc <- discipline_vote_agreement("emo_agree", "Emotional-Psychological Abuse")
  
  # Neglect
  neg_disc <- discipline_vote_agreement("neg_agree", "Neglect")
  
  # Self-neglect
  self_disc <- discipline_vote_agreement("self_agree", "Self-neglect")
  
  # Financial Explotation
  fin_disc <- discipline_vote_agreement("fin_agree", "Financial Exploitation")
  
  # Abandonment
  aban_disc <- discipline_vote_agreement("aban_agree", "Abandonment")
  
  # Merge dataframes for each abuse type to create one data frame for all abuse types
  discipline_agree <- purrr::reduce(list(phys_disc, sex_disc, emo_disc, neg_disc, self_disc, fin_disc, aban_disc), rbind)
  
  # Compute "Overall" rows for each panelist discipline in each assessment from sum of votes and dissenting votes for all the abuse types 
  discipline_agree <- discipline_agree %>%
    group_by(assessment_type_3cat_f, panelist_discipline_5cat_f) %>%
    bind_rows(
      reframe(
        .,
        across(
          .cols = c(n_dissenting_votes, n_votes),
          .fns  = ~sum(.x)
        ),
        abuse_type = "Overall",
        panelist_discipline_5cat_f = panelist_discipline_5cat_f
      )
    ) %>%
    distinct()
  
  # Replace dissenting votes column with a column that shows dissenting vote count and percentage over the number of votes
  discipline_agree <- discipline_agree %>%
    mutate(
      n_perc_diss_votes = paste0(n_dissenting_votes, " (", format(round((n_dissenting_votes/ n_votes)*100, digits = 4),nsmall = 4), "%)")
    ) %>%
    select(-c(n_dissenting_votes)) %>%
    
  # Convert table into a wider form
    pivot_wider(names_from = assessment_type_3cat_f,
                values_from = c(n_votes, n_perc_diss_votes)) 
    
    # Relocate columns so that the n_votes column for each assessment is next to the n_perc_diss_votes for the same assessment
  discipline_agree <- discipline_agree %>%
    reduce(
      .x = list(c('n_perc_diss_votes_Initial assessment','n_votes_Initial assessment'), 
                c('n_perc_diss_votes_Secondary assessment','n_votes_Secondary assessment'),
                c('n_perc_diss_votes_Post-detect assessment','n_votes_Post-detect assessment')
                ), 
      .f = ~ relocate(.x, .y[1], .after = .y[2]),
      .init = discipline_agree
    )
  
  return(discipline_agree)
}

# test <- panelist_vote_agreement(lpac)

