#' Get a 2x2 Chi-Square and Fisher Tests for Response Patterns, Item-Wise
#'
#' @description This function generates a human-legible text table that 
#'   displays the test, statistic value, parameter, and p-value for 
#'   Chi-Square and Fisher's Exact testing of DETECT Item Response Pattern 
#'   values, with testing performed at the Item level for select columns.
#'   
#'   Dependencies: dplyr, stats, get_chi_fisher()
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); stats (4.4.1)
#'
#' @param .dfa The first response pattern table. Must have 'var_vals' and 
#'      'text_var_val'. This table orders the DETECT items in the final table 
#' @param .dfb The second response pattern table. Must have 'var_vals'.
#' @param .target_vals A vector or list that gives the target columns in 
#'        .dfa and .dfb, such as "n_yes". Must be string values for names.
#'
#' @return A data frame
#' @export
#'

get_dt_rp_chi_fisher <- function(.dfa, .dfb, .target_vals, .omit_vals = NA){
  # Parse Inputs
  # =========================================================================
  # Ensure .dfa is a data frame
  # -------------------------------------------------------------------------
  if (!is.data.frame(.dfa)) {
    message <- paste(
      "The value entered into .dfa is not a data frame.", 
      "Please check input and retry."
    )
    stop(message)
  }
  # Ensure .dfb is a data frame
  # -------------------------------------------------------------------------
  if (!is.data.frame(.dfb)) {
    message <- paste(
      "The value entered into .dfb is not a data frame.", 
      "Please check input and retry."
    )
    stop(message)
  }
  # Validate if .target_vals is reasonable
  # -------------------------------------------------------------------------
  # If .target_vals is given...
  if(sum(!is.na(.target_vals)) > 0){
    ## make sure it is given as a string
    if (!is.character(.target_vals)){
      message <- paste(
        "Parameter .target_vals requires a character column/vector name(s).", 
        "Check input and try again."
      )
      stop(message)
    }
    # If .target_vals was given, make sure values exist in .dfa
    if(sum(!(.target_vals %in% colnames(.dfa))) > 0){
      message <- paste0(
        "There are values in .target_vals that are not in .dfa",
        "Check input and try again."
      )
      stop(message)
    }
    # If .target_vals was given, make sure values exist in .dfb
    if(sum(!(.target_vals %in% colnames(.dfb))) > 0){
      message <- paste0(
        "There are values in .target_vals that are not in .dfb",
        "Check input and try again."
      )
      stop(message)
    }
  }
  # If .target_vals does not exist, raise an stop
  if (sum(!is.na(.target_vals)) == 0) {
    message <- paste(
      ".target_vals is required.", 
      "Check input and try again."
    )
    stop(message)
  }
  # Check and Lightly Process .omit_vals
  # -------------------------------------------------------------------------
  ## If .omit_vals exists but is neither a list nor a vector, revert to NA
  if(
    (sum(is.na(.omit_vals)) > 0) & 
    !(is.list(.omit_vals) | is.vector(.omit_vals))
  ){
    message <- paste0(
      ".omit_vals must be a list or vector. Ignoring passed value."
    )
    .omit_vals = NA
    warning(message)
  }
  ## If .omit_vals exists but does not contain a string, revert to NA
  if(
    (sum(!is.na(.omit_vals)) > 0) & (sum(is.character(.omit_vals)) < 1)
  ){
    message <- paste0(
      ".omit_vals must have character values. Ignoring passed value."
    )
    .omit_vals = NA
    warning(message)
  }  
  
  # Perform Calculations
  # =========================================================================
  ## Obtain a temporary table containing target values for both pattern tables
  ## Each row is an Item-Target Value pair, with Target Value as "Response"
  ## ------------------------------------------------------------------------
  .obs <- dplyr::left_join(
    .dfa |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(.target_vals), 
        names_to = "response", 
        values_to = "dfa"
      ),
    .dfb |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(.target_vals), 
        names_to = "response", 
        values_to = "dfb"
      ),
    by = c("var_val", "response")
  )
  
  ## Initiate the output table
  ## ------------------------------------------------------------------------
  output <- data.frame(
    test = character(), dt_item = character(), dt_item_text = character(),
    stat = character(), param = character(), pval = character(), 
    sig = character()
  )
  
  ## For each DETECT Item...
  ## ------------------------------------------------------------------------
  for (i in seq(1, length(.dfa$var_val))){
    
    ### Obtain the Statistical Tests. Label with Item Name, add to Output
    output <- output |>
      dplyr::bind_rows(
        get_chi_fisher(
          .df = .obs[.obs$var_val == .dfa$var_val[i],], 
          .counts_a_var = "dfa", 
          .counts_b_var = "dfb", 
          .probs_b_var = NA_character_, 
          .omit_vals = .omit_vals, 
          .value_var = "response"
        ) |>
          dplyr::mutate(
            dt_item = .dfa$var_val[i], 
            dt_item_text = .dfa[i,]$text_var_val
          )
      )
  }
  
  ## Format if .omit_vals was used
  ## ------------------------------------------------------------------------
  
  if (!is.na(.omit_vals)){
    output <- output |>
      dplyr::relocate(test, dt_item, method)  
  }

  ## Return output table
  ## ------------------------------------------------------------------------
  output
}