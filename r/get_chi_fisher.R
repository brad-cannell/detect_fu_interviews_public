#' Get a 2x2 Chi-Square and Fisher Tests, human-legible table
#'
#' @description This function generates a human-legible text table that 
#'   displays the test, statistic value, parameter, and p-value for 
#'   Chi-Square and Fisher's Exact test on a pre-formatted summary table. It
#'   allows for the use of counts or probabilities for the second group, as
#'   well as sensitivity testing for the omission of select values.
#'   
#'   Dependencies: dplyr, stats
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); stats (4.4.1)
#'
#' @param .df A data frame that contains summary count data of the two groups.
#' @param .counts_a_var Column of counts for "Group A" in .df. Must be numeric.
#' @param .counts_b_var Column of counts for "Group B" in .df. Must be numeric.
#' @param .probs_b_var Column of probabilities for "Group B" in .df. 
#'      Must be numeric.
#' @param .value_var Column that contains the values or levels in .df. Used 
#'      to subset .df when processing omissions. Default is NA, which
#'      skips sensitivity testing for omitting levels.
#' @param n_decimal Number of decimal places to display in values. Default
#'   value is 5 
#'
#' @return A data frame
#' @export
#'
#' @examples
#' get_chi_fisher(
#' .df = dplyr::left_join(
#'    mtcars |> 
#'      dplyr::filter(am == 0) |> 
#'      dplyr::select(cyl) |> 
#'      table() |> 
#'      as.data.frame() |>
#'      dplyr::rename_at('Freq', ~'AM_0'),
#'    mtcars |> 
#'      dplyr::filter(am == 0) |> 
#'      dplyr::select(cyl) |> 
#'      table() |> 
#'      as.data.frame() |>
#'      dplyr::rename_at('Freq', ~'AM_1'),
#'    by = 'cyl'),
#'  .counts_a_var = 'AM_0',
#'  .counts_b_var = 'AM_1',
#'  .value_var = 'cyl',
#'  .omit_vals = '4',
#'  n_decimal = 8
#'  )
#'

get_chi_fisher <- function(
    .df, .counts_a_var, .counts_b_var, .probs_b_var = NA_character_, 
    .value_var = NA_character_, .omit_vals = NA, n_decimal = 5
){
  # Set a random seed, as fisher test uses "simulate.p.value" to avoid hanging
  # on large sets
  set.seed(42)
  # Parse Inputs
  # =========================================================================
  # Ensure .df is a data frame
  # -------------------------------------------------------------------------
  if (!is.data.frame(.df)) {
    message <- paste(
      "The value entered into .df is not a data frame.", 
      "Please check input and retry."
    )
    stop(message)
  }
  # Validate if .counts_a_var is reasonable
  # -------------------------------------------------------------------------
  # If .counts_a_var is given...
  if(sum(!is.na(.counts_a_var)) > 0){
  ## make sure it was given as a string
    if (!is.character(.counts_a_var)){
      message <- paste(
        "Parameter .counts_a_var requires a character column/vector name.", 
        "Check input and try again."
      )
      stop(message)
    }
  # If there's more than one value for .counts_a_var, raise an stop
    if (length(.counts_a_var) > 1) {
      message <- paste(
        "Multiple values in .counts_a_var argument. Only one column allowed.", 
        "Check input and try again."
      )
      stop(message)
    }
  # If .counts_a_var was given, make sure it exists in .df
    if(!(.counts_a_var %in% colnames(.df))){
      message <- paste0(
        .counts_a_var, " was given for .counts_a_var, but it is not in .df! ",
        "Check input and try again."
      )
      stop(message)
    }
  # If .counts_a_var was given, be sure it's numeric.
    if (!is.numeric(.df[[.counts_a_var]])) {
      message <- paste(
        ".counts_a_var must be numeric, but it is not.", 
        "Check input and try again."
      )
      stop(message)
    }
  }
  # If .counts_a_var does not exist, raise an stop
  if (is.na(.counts_a_var)) {
    message <- paste(
      ".counts_a_var is required.", 
      "Check input and try again."
    )
    stop(message)
  }
  # Validate if .counts_b_var is reasonable
  # -------------------------------------------------------------------------
  # If .counts_b_var is given...
  if (sum(!is.na(.counts_b_var))>0){
    ## make sure it was given as a string
    if(!is.character(.counts_b_var)){
      message <- paste(
        "Parameter .counts_b_var requires a character column/vector name.", 
        "Check input and try again."
      )
      stop(message)
    }
    # If there's more than one value for .counts_b_var, raise an stop
    if (length(.counts_b_var) > 1) {
      message <- paste(
        "Multiple values in .counts_b_var argument. Only one column allowed.", 
        "Check input and try again."
      )
      stop(message)
    }
    # If .counts_b_var was given, make sure it exists in .df
    if(!(.counts_b_var %in% colnames(.df))){
      message <- paste0(
        .counts_b_var, " was given for .counts_b_var, but it is not in .df! ",
        "Check input and try again."
      )
      stop(message)
    }
    # If .counts_b_var was given, be sure it's numeric.
    if (!is.numeric(.df[[.counts_b_var]])) {
      message <- paste(
        ".counts_b_var must be numeric, but it is not.", 
        "Check input and try again."
      )
      stop(message)
    }
  }
  # If .counts_b_var does not exist, raise an stop
  if (is.na(.counts_b_var)) {
    message <- paste(
      ".counts_b_var is required.", 
      "Check input and try again."
    )
    stop(message)
  }
  # If .counts_a_var is identical to .counts_b_var, raise warning 
  # because why are you running that?
  if (.counts_a_var == .counts_b_var) {
    message <- paste(
      ".counts_a_var and .counts_b_var are identical.", 
      "Are you sure this is the analysis you want to run?"
    )
    warning(message)
  }
  # Check .probs_b_var, if given. Remove if invalid
  # -------------------------------------------------------------------------
  # If .probs_b_var is given... 
  if (sum(!is.na(.probs_b_var)) > 0 ){
    # make sure it was given as a string
    if(!is.character(.probs_b_var)){
    message <- paste(
      "Parameter .probs_b_var requires a character column/vector name.", 
      "Reverting to NA."
    )
    .probs_b_var = NA_character_
    warning(message)
    }
    # If there's more than one value for .probs_b_var, remove them all
    if (length(.probs_b_var) > 1) {
      message <- paste(
        "Multiple values in .probs_b_var argument. Only one column allowed.", 
        "Reverting to NA."
      )
      .probs_b_var = NA_character_
      warning(message)
    }
    # If .probs_b_var was given, make sure it exists in .df
    if(!(.probs_b_var %in% colnames(.df))){
      message <- paste0(
        .probs_b_var, " was given for .probs_b_var, but it is not in .df! ",
        "Reverting to NA."
      )
      .probs_b_var = NA_character_
      warning(message)
    }
    # If .probs_b_var was given, be sure it's numeric.
    if (!is.na(.probs_b_var)) {
      if(!is.numeric(.df[[.probs_b_var]])){
        message <- paste(
          ".probs_b_var must be numeric, but it is not.", 
          "Reverting to NA."
        )
        .probs_b_var = NA_character_
        warning(message)
      }
    }
  }
  # Chi-Square integer checks
  # ------------------------------------------------------------------------
  # If .probs_b_var was NOT given and counts_b_var does not only contain 
  # integers, raise a warning 
  if (
    sum(as.integer(.df[[.counts_b_var]]) == .df[[.counts_b_var]]) != 
    length(.df[[.counts_b_var]]) & is.na(.probs_b_var)
  ){
    message <- paste(
      "Values in .counts_b_var are not all integers, and no .probs_b_var",
      "was given. Chi-square does not like non-integers. Interpret with care."
    )
    warning(message)
  }
  # If .counts_a_var does not only contain integers, raise a warning 
  if (
    sum(as.integer(.df[[.counts_a_var]]) == .df[[.counts_a_var]]) != 
    length(.df[[.counts_a_var]])
  ){
    message <- paste(
      "Values in .counts_a_var are not all integers. ",
      "Chi-square does not like non-integers. Interpret with care."
    )
    warning(message)
  }
  # Validate if .value_var is reasonable
  # -------------------------------------------------------------------------
  # If .value_var is given... make sure it was given as a string pr ignore it
  if (sum(!is.na(.value_var)) > 0) {
    if(!is.character(.value_var)){
      message <- paste(
        "Parameter .value_var requires a character column/vector name.", 
        "Ignoring input. If .omit_vals was also given, it's also ignored."
      )
      .value_var = NA_character_
      .omit_vals = NA
      warning(message)
    }
  # If there's more than one value for .value_var, raise an warning and ignore
    if (length(.value_var) > 1) {
      message <- paste(
        "Multiple values in .value_var argument. Only one column allowed.", 
        "Ignoring input. If .omit_vals was also given, it's also ignored."
      )
      .value_var = NA_character_
      .omit_vals = NA
      warning(message)
    }
  # If .value_var was given, make sure it exists in .df
    if(!(.value_var %in% colnames(.df))){
      message <- paste0(
        .value_var, " was given for .value_var, but it is not in .df! ",
        "Ignoring input. If .omit_vals was also given, it's also ignored."
      )
      .value_var = NA_character_
      .omit_vals = NA
      warning(message)
    }
  }
  # Check and Lightly Process .omit_vals
  # -------------------------------------------------------------------------
  ## If .omit_vals exists but .value_var does not, revert to NA
  if((sum(!is.na(.omit_vals)) > 0) & is.na(.value_var)){
    message <- paste0(
      ".omit_vals requires .value_var, but no .value_var given. ", 
      "Ignoring passed value for .omit_vals."
    )
    .omit_vals = NA
    warning(message)
  }  
  
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
  ## If .omit_vals does not contain values that exist in .value_var's values,
  ## raise warning for the user.
  if (
    sum(!is.na(.omit_vals) > 0) & 
    sum(!(.omit_vals %in% .df[[.value_var]])) > 0
    ) {
    message <- paste0(
      ".omit_vals contains values which are not in .df[[.value_var]]: ",
      paste(setdiff(.omit_vals, .df[[.value_var]]), sep = ', '),
      ". Did you make a typo?"
    )
    warning(message)
  }
  
  # Perform Calculations and Form Output
  # =========================================================================
  ## Initial Calculations
  ## ------------------------------------------------------------------------
  ### Chi-Square
  #### If .probs_b_var is given, use probabilities for Chi-Square and to
  #### calculate expected values for Fisher's Exact
  if (!is.na(.probs_b_var)){
    chisq_vals <- suppressWarnings(stats::chisq.test(
      .df[[.counts_a_var]], 
      p = .df[[.probs_b_var]]
    ))
    fisher_vals <- suppressWarnings(stats::fisher.test(
      data.frame(
        a = .df[[.counts_a_var]], 
        b = sum(.df[[.counts_a_var]], na.omit = T) * .df[[.probs_b_var]]
        ),
      simulate.p.value = T
    ))
  } else {
    #### Otherwise just use given values
    chisq_vals <- suppressWarnings(stats::chisq.test(
      data.frame(
        a = .df[[.counts_a_var]], 
        b = .df[[.counts_b_var]]
        )
    ))
    fisher_vals <- suppressWarnings(stats::fisher.test(
      data.frame(
        a = .df[[.counts_a_var]], 
        b = .df[[.counts_b_var]]
        ),
      simulate.p.value = T
    ))
  }
  
  ## Place results into a formatted summary table for human legibility
  ## ------------------------------------------------------------------------
  output <- data.frame(
    test = c("Chi-Square", "Fisher's Exact"),
    stat = c(chisq_vals$statistic[['X-squared']], NA_real_),
    param = c(chisq_vals$parameter[['df']], NA_integer_),
    pval = c(chisq_vals$p.value, fisher_vals$p.value)
  ) |>
    dplyr::mutate(
      sig = dplyr::case_when(
        pval < 0.001 ~ "***",
        pval < 0.01 ~ "**",
        pval < 0.05 ~ "*",
        TRUE ~ ""
      )
    ) |>
    ### Convert numeric values to scientific notation if they'd require more
    ### than n_decimal digits after the decimal to display
    dplyr::mutate(
      stat = ifelse(
        stat < 10^(-n_decimal),
        format(stat, digits = n_decimal + 1, scientific = T),
        format(round(stat, n_decimal), scientific = F)
        ),
      param = ifelse(
        param < 10^(-n_decimal),
        format(param, digits = n_decimal + 1, scientific = T),
        format(round(param, n_decimal), scientific = F)
      ),
      pval = ifelse(
        pval < 10^(-n_decimal),
        format(pval, digits = n_decimal + 1, scientific = T),
        format(round(pval, n_decimal), scientific = F)
      ),
    )
  
  ## If there are .omit_vals, repeat analyses with these values omitted.
  ## ------------------------------------------------------------------------
  if(sum(!is.na(.omit_vals) > 0)){
    omit_df <- .df |>
      dplyr::filter(!(!!rlang::sym(.value_var) %in% .omit_vals))
    
    ### Calculate statistical tests
    #### Chi-Square
    ##### If .probs_b_var is given, use probabilities for Chi-Square and to
    ##### calculate expected values for Fisher's Exact
    if (!is.na(.probs_b_var)){
      chisq_vals <- suppressWarnings(stats::chisq.test(
        omit_df[[.counts_a_var]], 
        p = omit_df[[.probs_b_var]]
      ))
      fisher_vals <- suppressWarnings(stats::fisher.test(
        data.frame(
          a = omit_df[[.counts_a_var]], 
          b = sum(
            omit_df[[.counts_a_var]], na.omit = T
            ) * omit_df[[.probs_b_var]]
          ),
        simulate.p.value = T
      ))
    } else {
      #### Otherwise just use given values
      chisq_vals <- suppressWarnings(stats::chisq.test(
        data.frame(
          a = omit_df[[.counts_a_var]], 
          b = omit_df[[.counts_b_var]]
          )
      ))
      fisher_vals <- suppressWarnings(stats::fisher.test(
        data.frame(
          a = omit_df[[.counts_a_var]], 
          b = omit_df[[.counts_b_var]]
        ),
        simulate.p.value = T
      ))
    }
    
    ### Add results into the formatted summary table for human legibility
    output <- data.frame(
      test = c("Chi-Square", "Fisher's Exact"),
      #### Specify method
      method = rep("Post-Omissions", 2),
      stat = c(chisq_vals$statistic[['X-squared']], NA_real_),
      param = c(chisq_vals$parameter[['df']], NA_integer_),
      pval = c(chisq_vals$p.value, fisher_vals$p.value)
    ) |>
      dplyr::mutate(
        sig = dplyr::case_when(
          pval < 0.001 ~ "***",
          pval < 0.01 ~ "**",
          pval < 0.05 ~ "*",
          TRUE ~ ""
        )
      ) |>
      #### Convert numeric values to scientific notation if they'd require 
      #### more than n_decimal digits after the decimal to display
      dplyr::mutate(
        stat = ifelse(
          stat < 10^(-n_decimal),
          format(stat, digits = n_decimal + 1, scientific = T),
          format(round(stat, n_decimal), scientific = F)
        ),
        param = ifelse(
          param < 10^(-n_decimal),
          format(param, digits = n_decimal + 1, scientific = T),
          format(round(param, n_decimal), scientific = F)
        ),
        pval = ifelse(
          pval < 10^(-n_decimal),
          format(pval, digits = n_decimal + 1, scientific = T),
          format(round(pval, n_decimal), scientific = F)
        ),
      ) |>
      #### Add method to existing rows
      dplyr::bind_rows(
        output |>
          dplyr::mutate(
            method = rep("All Values", 2)
          )
      ) |>
      #### Arrange to display by method, then test
      dplyr::arrange(method, test)
    
  }
  
  ## Return output table
  ## ------------------------------------------------------------------------
  output
}