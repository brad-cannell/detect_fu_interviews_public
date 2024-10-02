# The function in this file does the following:
# - Creates a data frame with two variables: 
#   + The final abuse determination made by the LEAD panel  and;
#   + The abuse determination made using the DETECT tool at the initial visit (for a specified DETECT tool item or the aggregate of all items).
# - Creates a formatted confusion matrix.

# Required libraries: tidyverse (tidyr, tibble, dplyr), flextable

compare_det_item <- function(lead_final_det, detect_tool, detect_tool_item){
  
  
  # Select the any_abuse column from the final determination dataframe
  abuse_final_det <- lead_final_det %>% select(medstar_id, abuse_any) %>%
    
    # convert factor variables to binary factor variable
    mutate(
      abuse_any =  case_when(
        abuse_any == "Yes" ~ "positive",
        abuse_any == "No"  ~ "negative"
      ),
      abuse_any = factor(abuse_any, levels = c("positive", "negative"))
    )
  
  # Determine the initial MedStar Medic DETECT tool abuse determination for the specific item
  detect_tool <- detect_tool %>% 
    mutate(
      # Create a column that has a value of "positive" if the DETECT tool variable is positive
      detect_tool_det = case_when(
        {{detect_tool_item}} == "Yes"                                            ~ "positive",
        if_all(ends_with("_5cat_f"), ~ !is.na(.)) & {{detect_tool_item}} == "No" ~ "negative",
        TRUE                                                                     ~ NA
      ),
      # Convert variable type to factor
      detect_tool_det = factor(detect_tool_det, levels = c("positive", "negative"))
    )
  
  # Combine the two variables into one df
  tool_vs_lead <- abuse_final_det %>% left_join(detect_tool, by = "medstar_id")
  
  
  # Create contingency table
  conf_con <- tool_vs_lead %>% select(abuse_any, detect_tool_det) %>% drop_na() %>% table(dnn = c("abuse_any", "detect_tool_det")) %>% as.data.frame()
  
  
  # Create a frequency data frame with columns structured appropriately for flextable
  
  col_1_label <- c("positive", "negative", "Total")
  col_2_actual_positive <- c(conf_con$Freq[conf_con$abuse_any == "positive" & conf_con$detect_tool_det == "positive"], 
                             conf_con$Freq[conf_con$abuse_any == "positive" & conf_con$detect_tool_det == "negative"],
                             sum(conf_con$Freq[conf_con$abuse_any == "positive"]))
  col_3_actual_negative <- c(conf_con$Freq[conf_con$abuse_any == "negative" & conf_con$detect_tool_det == "positive"], 
                             conf_con$Freq[conf_con$abuse_any == "negative" & conf_con$detect_tool_det == "negative"],
                             sum(conf_con$Freq[conf_con$abuse_any == "negative"]))
  col_4_total <- c(sum(conf_con$Freq[conf_con$detect_tool_det == "positive"]), sum(conf_con$Freq[conf_con$detect_tool_det == "negative"]), sum(conf_con$Freq)) 
  
  con_flex <- data.frame(col_1_label, col_2_actual_positive, col_3_actual_negative, col_4_total) %>%
    
    # Convert to flextable and format flextable
    flextable() %>%
    # Format flextable
    set_header_labels(
      col_1_label = "Detect Tool",
      col_2_actual_positive = "positive",
      col_3_actual_negative = "negative",
      col_4_total = "Total"
    )%>%
    border_remove() %>%
    bg(part = "body", i = 1, j = 2, bg = "#8cc0ae") %>%
    bg(part = "body", i = 2, j = 3, bg = "#8cc0ae") %>%
    bg(part = "body", i = 2, j = 2, bg = "#9dcfbc") %>%
    bg(part = "body", i = 1, j = 3, bg = "#9dcfbc") %>%
    color(j = c(2,3), i = c(1,2), color = "white") %>%
    bold(j = 1, part = "header") %>%
    bold(i = 1, j = 1, part = "header") %>%  
    height(i = c(1,2), height = 1.5, unit = "in") %>%
    hrule(rule = "exact") %>%
    width(j = -c(1,4), width = 1.5, unit = "in") %>%
    width(j = 1, width = 1) %>%
    align(align = "center") %>%
    align(j = 1, align = "right") %>%
    align(j = 4, align = "left", part = c("body")) %>%
    align(j = 4, align = "left", part = c("header")) %>%
    valign(valign = c("center")) %>%
    vline(j = 3, border = fp_border(color = "black")) %>%
    hline(i = 2, border = fp_border(color = "black")) %>%
    add_header_lines("LEAD Panel Assessment") %>%
    align(i = 1, align = "center", part = "header") %>%
    align(i = 2, j = c(1,2,3), align = "center", part = "header")
  
  # Create table with specificity, sensitivity and prevalence
  conf_calc <- conf_con %>%
    summarise(
      Sensitivity = paste0(format(round((Freq[abuse_any == "positive" & detect_tool_det == "positive"]/sum(Freq[abuse_any == "positive"]))*100,
                                        digits = 2), nsmall = 2), " %"),
      Specificity = paste0(format(round((Freq[abuse_any == "negative" & detect_tool_det == "negative"]/sum(Freq[abuse_any == "negative"]))*100,
                                        digits = 2), nsmall = 2), " %"),
      Prevalence = paste0(format(round((sum(Freq[abuse_any == "positive"])/sum(Freq))*100, digits = 2), nsmall = 2), " %")
      
    ) %>% t() %>% as.data.frame() %>% rownames_to_column() %>% 
    flextable() %>%
    delete_part(part = "header") %>%
    hline_top() %>%
    hline_bottom() %>%
    bold(j = 1) %>%
    width(width = 1, unit = "in")
  
  out <- list(con_flex, conf_calc, tool_vs_lead)              # Store output in list
  return(out)
}