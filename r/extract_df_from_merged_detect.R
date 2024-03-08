# Create function that extracts columns for a specified data frame within the DETECT FU interview data from the merged data set
# The suffixes that correspond to each data frame are as follows:
# - Participant: "_par",
# - APS Investigations: "_aps"
# - Clutter scale: "_cls"
# - General health: "_glh"
# - LEAD Panel Assessment(without data error): "_lpc"
# - Observational measures: "_obs"
# - Self report: "_sfr"
# - Sociodemographic information "_soc"

filter_merged_df <- function(detect_fu_merge, suffix) {
  df_extract <- detect_fu_merge %>%
    # select columns to include in the desired df
    select(medstar_id, unique_id_par, ends_with(suffix)) %>%
    # remove rows where all columns in desired df are empty
    filter(!if_all(ends_with(suffix), is.na)) %>%
    # remove suffix
    rename_with(~ sub(paste(noquote(suffix), "$", sep = ""), "", .x), everything()) %>%
    distinct()
}
