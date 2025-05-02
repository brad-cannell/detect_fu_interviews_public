#' Produce human-legible agreement statistics summary table
#'
#' @description This function generates a table giving the results
#'	of statistical tests for agreement between two columns with
#'	identical potential values, where each column represents a
#'	single "rater" and each row is a single subject that the
#'	raters are providing the values for, formatted for human 
#'	legibility as text. Uses Chi-Square, McNemar (if possible), 
#'	Correlation Coefficients, and Kappa Agreement.
#'    
#'   Dependencies: stats, irr, dplyr, stringr
#'   
#'   Built: R (4.4.1); stats (4.4.1); irr (0.84.1); dplyr (1.1.4);
#'	    stringr (1.5.1)
#'
#' @param .df A data frame
#' @param .var_a Variable in .df with two values. Must be in .df.
#'	Must be given as a string. Will be the first column of table.
#' @param .var_b Variable in .df with two values. Must be in .df.
#'	Must be given as a string. Will be the columns.
#' @param .var_a_labs A vector or list that gives the desired values
#'	in .df[[.var_a]]. Use naming to give labels, such as 
#'	list(t = "true label", f = "false label"). Must be values in
#'	.df[[.var_a]], labels as strings.
#' @param .var_b_labs A vector or list that gives the desired values
#'	in .df[[.var_b]]. Use naming to give labels, such as 
#'	list(t = "true label", f = "false label"). Must be values in
#'	.df[[.var_b]], labels as strings. Names must be
#'	identical to names of .var_a_labs
#' @param .corr_methods A vector giving the character names of 
#'	desired methods for correlation calculations. Values include
#'	"pearson", "kendall", "spearman". Uses stats::corr(). Default
#'	uses all valid values.
#' @param .kapa_methods A vector giving the character names of
#'	desired methods for Kappa calculations. Values include
#'	"cohen", "fleiss", "light". Uses irr methods. Default uses
#'	all valid values.
#'
#' @return A data frame
#' @export
#'
get_agreement_stats_table <- function(
    .df, .var_a, .var_b, .var_a_labs = NA, .var_b_labs = NA, 
    .corr_methods = c("pearson", "kendall", "spearman"), 
    .kappa_methods = c("cohen", "fleiss", "light"), 
    .n_decimal = 4
){
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
  # Validate if .var_a is reasonable
  # -------------------------------------------------------------------------
  # If .var_a is given...
  if(sum(!is.na(.var_a)) > 0){
    ## make sure it was given as a string
    if (!is.character(.var_a)){
      message <- paste(
        "Parameter .var_a requires a character column/vector name.", 
        "Check input and try again."
      )
      stop(message)
    }
    # If there's more than one value for .var_a, raise an stop
    if (length(.var_a) > 1) {
      message <- paste(
        "Multiple values in .var_a argument. Only one allowed.", 
        "Check input and try again."
      )
      stop(message)
    }
    # If .var_a was given, make sure it exists in .df
    if(!(.var_a %in% colnames(.df))){
      message <- paste0(
        .var_a, " was given for .var_a, but it is not ",
        "in .df! Check input and try again."
      )
      stop(message)
    }
  }
  # If .var_a does not exist, raise an stop
  if (is.na(.var_a)) {
    message <- paste(
      ".var_a is required.", 
      "Check input and try again."
    )
    stop(message)
  }
  # Validate if .var_b is reasonable
  # -------------------------------------------------------------------------
  # If .var_b is given...
  if(sum(!is.na(.var_b)) > 0){
    ## make sure it was given as a string
    if (!is.character(.var_b)){
      message <- paste(
        "Parameter .var_b requires a character column/vector name.", 
        "Check input and try again."
      )
      stop(message)
    }
    # If there's more than one value for .var_b, raise an stop
    if (length(.var_b) > 1) {
      message <- paste(
        "Multiple values in .var_b argument. Only one allowed.", 
        "Check input and try again."
      )
      stop(message)
    }
    # If .var_b was given, make sure it exists in .df
    if(!(.var_b %in% colnames(.df))){
      message <- paste0(
        .var_b, " was given for .var_b, but it is not ",
        "in .df! Check input and try again."
      )
      stop(message)
    }
  }
  # If .var_b does not exist, raise an stop
  if (is.na(.var_b)) {
    message <- paste(
      ".var_b is required.", 
      "Check input and try again."
    )
    stop(message)
  }
  # Check and Lightly Process .var_a_labs
  # -------------------------------------------------------------------------
  ## Get list of unique non-missing values in .df[[.var_a]]
  
  var_a_values <- unique(na.omit(.df[[.var_a]]))
  
  ## If .var_a_labs exists and is neither a list nor a vector...
  if(
    (sum(is.na(.var_a_labs)) > 0) & 
    !(is.list(.var_a_labs) | is.vector(.var_a_labs))
  ){
    ### If there are 2 unique non-NA values in .var_a, use those...
    if(length(var_a_values) == 2){
      message <- paste0(
        ".var_a_labs must be a list or vector. Ignoring passed value."
      )
      .var_a_labs = c(paste(.var_a, var_a_values, sep = "_"))
      names(.var_a_labs) <- var_a_values
      warning(message)
    } else {
      ### Otherwise, raise an error 
      message <- paste0(
        ".var_a_labs must be a list or vector, and .var_a has more than 2",
        " values. Check input and try again."
      )
      stop(message)
    }
  }
  ## If .var_a_labs does not have names, make names identical to the values. 
  ## (eg. "a" = "a")
  if(sum(!is.na(.var_a_labs)) > 0 & is.null(names(.var_a_labs))){
    message <- paste0(
      ".var_a_labs entered without names. Using values for names.",
      " This may garble presentation."
    )
    names(.var_a_labs) = as.character(.var_a_labs)
    warning(message)
  }
  ## If names(.var_a_labs) contains values not in .df[[.var_a]], raise an 
  ## error
  if(
    sum(!is.na(.var_a_labs)) > 0 & 
    sum(!names(.var_a_labs) %in% as.character(var_a_values)) > 0
  ) {
    message <- paste0(
      ".var_a_labs contains values that are not in .var_a. ",
      "Check input and try again. "
    )
    stop(message)
  }
  ## If .var_a_labs is not exactly 2 entries, raise an error
  if (length(.var_a_labs) != 2){
    message <- paste0(
      ".var_a_labs must have 2 values, one for 'TRUE' and one for 'FALSE'. ",
      "Check input and try again."
    )
    stop(message)
  }
  ## If .var_a_labs somehow does not exist at this point, raise an error
  if(
    sum(is.na(.var_a_labs)) > 0|sum(is.null(.var_a_labs)) > 0
  ){
    message <- paste0(
      "Unexpected error: .var_a_labs does not exist"
    )
    stop(message)
  }
  # Check and Lightly Process .var_b_labs
  # -------------------------------------------------------------------------
  ## Get list of unique non-missing values in .df[[.var_b]]
  
  var_a_values <- unique(na.omit(.df[[.var_b]]))
  
  ## If .var_b_labs exists and is neither a list nor a vector...
  if(
    (sum(is.na(.var_b_labs)) > 0) & 
    !(is.list(.var_b_labs) | is.vector(.var_b_labs))
  ){
    ### If there are 2 unique non-NA values in .var_b, use those...
    if(length(var_a_values) == 2){
      message <- paste0(
        ".var_b_labs must be a list or vector. Ignoring passed value."
      )
      .var_b_labs = c(paste(.var_b, var_a_values, sep = "_"))
      names(.var_b_labs) <- var_a_values
      warning(message)
    } else {
      ### Otherwise, raise an error 
      message <- paste0(
        ".var_b_labs must be a list or vector, and .var_b has more than 2",
        " values. Check input and try again."
      )
      stop(message)
    }
  }
  ## If .var_b_labs does not have names, make names identical to the values. 
  ## (eg. "a" = "a")
  if(sum(!is.na(.var_b_labs)) > 0 & is.null(names(.var_b_labs))){
    message <- paste0(
      ".var_b_labs entered without names. Using values for names.",
      " This may garble presentation."
    )
    names(.var_b_labs) = as.character(.var_b_labs)
    warning(message)
  }
  ## If names(.var_b_labs) contains values not in .df[[.var_b]], raise an 
  ## error
  if(
    sum(!is.na(.var_b_labs)) > 0 & 
    sum(!names(.var_b_labs) %in% as.character(var_a_values)) > 0
  ) {
    message <- paste0(
      ".var_b_labs contains values that are not in .var_b. ",
      "Check input and try again. "
    )
    stop(message)
  }
  ## If .var_b_labs somehow does not exist at this point, raise an error
  if(
    sum(is.na(.var_b_labs)) > 0|sum(is.null(.var_b_labs)) > 0
  ){
    message <- paste0(
      "Unexpected error: .var_b_labs does not exist"
    )
    stop(message)
  }
  ## If .var_b_labs is not exactly 2 entries, raise an error
  if (length(.var_b_labs) != 2){
    message <- paste0(
      ".var_b_labs must have 2 values, one for 'TRUE' and one for 'FALSE'. ",
      "Check input and try again."
    )
    stop(message)
  }
  ## If names(.var_b_labs) != names(.var_a_labs), raise an error!
  if (sum(names(.var_b_labs) %in% names(.var_b_labs)) < 2){
    message <- paste0(
      ".var_b_labs and .var_a_labs do not have the same names. These must ",
      "be the same for the table to calculate properly. Check input and ",
      "try again."
    )
    stop (message)
  }
  # Check if .corr_methods is reasonable
  # -------------------------------------------------------------------------
  .valid_corrs <- c("pearson", "kendall", "spearman")
  ## If .corr_methods somehow doesn't exist, replace with default
  if (sum(!is.na(.corr_methods)) == 0){
    .corr_methods <- .valid_corrs
  }
  ## Ensure .corr_methods is a character vector
  if (!is.character(.corr_methods)){
    .corr_methods <- .valid_corrs
    message <- paste0(
      ".corr_methods must be a character vector. Replacing with default."
    )
    warning(message)
  }
  ## Lightly clean .corr_methods and flag invalid values
  .temp_methods <- stringr::str_trim(
    stringr::str_to_lower(.corr_methods),
    side = 'both'
  )
  .invalid_methods <- setdiff(.temp_methods, .valid_corrs)
  .temp_methods <- unique(intersect(.temp_methods, .valid_corrs))
  ### If there are invalid methods, and at least one valid method...
  if (length(.invalid_methods) > 0 & length(.temp_methods) > 0){
    message <- paste0(
      as.character(.invalid_methods), " invalid correlations methods ",
      "passed to .corr_methods: ", paste(.invalid_methods, sep = '; '),
      ". Removing invalid methods."
    )
    warning(message)
    .corr_methods <- .temp_methods
  }
  ### If there are no valid methods, use default
  if (length(.temp_methods) == 0){
    message <- paste0(
      as.character(.invalid_methods), " invalid correlations methods ",
      "passed to .corr_methods: ", paste(.invalid_methods, sep = '; '),
      ". No valid methods. Using default."
    )
    warning(message)
    .corr_methods <- .valid_corrs
  }
  # Check if .kappa_methods is reasonable
  # -------------------------------------------------------------------------
  .valid_kappas <- c("cohen", "fleiss", "light")
  ## If .kappa_methods somehow doesn't exist, replace with default
  if (sum(!is.na(.kappa_methods)) == 0){
    .kappa_methods <- .valid_kappas
  }
  ## Ensure .kappa_methods is a character vector
  if (!is.character(.kappa_methods)){
    .kappa_methods <- .valid_kappas
    message <- paste0(
      ".kappa_methods must be a character vector. Replacing with default."
    )
    warning(message)
  }
  ## Lightly clean .kappa_methods and flag invalid values
  .temp_methods <- stringr::str_trim(
    stringr::str_to_lower(.kappa_methods),
    side = 'both'
  )
  .invalid_methods <- setdiff(.temp_methods, .valid_kappas)
  .temp_methods <- unique(intersect(.temp_methods, .valid_kappas))
  ### If there are invalid methods, and at least one valid method...
  if (length(.invalid_methods) > 0 & length(.temp_methods) > 0){
    message <- paste0(
      as.character(.invalid_methods), " invalid kappa methods ",
      "passed to .kappa_methods: ", paste(.invalid_methods, sep = '; '),
      ". Removing invalid methods."
    )
    warning(message)
    .kappa_methods <- .temp_methods
  }
  ### If there are no valid methods, use default
  if (length(.temp_methods) == 0){
    message <- paste0(
      as.character(.invalid_methods), " invalid kappa methods ",
      "passed to .kappa_methods: ", paste(.invalid_methods, sep = '; '),
      ". No valid methods. Using default."
    )
    warning(message)
    .kappa_methods <- .valid_kappas
  }
  # Check if .n_decimal is reasonable
  # -------------------------------------------------------------------------
  ## If .n_decimal somehow doesn't exist, replace with default (4)
  if (sum(!is.na(.n_decimal)) == 0){
    .n_decimal = 4
  }
  ## If more than one value was given for .n_decimal, replace with default (4)
  if(length(.n_decimal) > 1){
    .n_decimal = 4
    message <- paste0(
      "Only one value can be given for .n_decimal. Converting to default of 4."
    )
    warning(message)
  }
  ## If .n_decimal is not numeric....
  if(!is.numeric(.n_decimal)){
    ## If it can be converted to a numeric, do that.
    if(!is.na(.n_decimal)){
      .n_decimal = as.numeric(.n_decimal)
    } else {
      ## Otherwise replace with default (4)
      .n_decimal = 4
      message <- paste0(
        ".n_decimal must be numeric. Converting to default of 4."
      )
      warning(message)
    }
  }
  ## If .n_decimal is not an integer, but is numeric, round it
  if(is.numeric(.n_decimal) & as.integer(.n_decimal) != .n_decimal){
    message <- paste0(
      "Non-integer value for .n_decimal (", as.character(.n_decimal), 
      ") rounding to ", as.character(round(.n_decimal, 0))
    )
    .n_decimal = as.integer(round(.n_decimal,0))
    warning(message)
  }    
  ## Extract only necessary columns and values for processing
  # ========================================================================
  .test_df <- .df |>
    ### Reduce to desired columns
    dplyr::select(dplyr::all_of(c(.var_a, .var_b))) |>
    ### Filter to only include rows with the desired values
    dplyr::filter(
      !!rlang::sym(.var_a) %in% names(.var_a_labs) &
        !!rlang::sym(.var_b) %in% names(.var_b_labs)
    ) |>
    ### Rename variables to placeholders
    dplyr::rename_at(
      c(.var_a, .var_b), 
      ~c("var_a", "var_b")
    ) |>
    ### Ensure values are logical "T/F"
    dplyr::mutate(
      var_a = dplyr::case_when(
        var_a == names(.var_a_labs)[1] ~ T,
        var_a == names(.var_a_labs)[2] ~ F,
      ),
      var_b = dplyr::case_when(
        var_b == names(.var_b_labs)[1] ~ T,
        var_b == names(.var_b_labs)[2] ~ F
      )
    )
  
  ## Initialize output table
  # ========================================================================
  .output_cols <- c("test", "method", "corr", "stat", "param", "pval")
  output <- data.frame(
    matrix(
      vector(), 0, length(.output_cols), 
      dimnames = list(c(), .output_cols)
    ),
    stringsAsFactors=F
  )
  
  ## Perform Statistical Tests
  # ========================================================================
  
  ### Chi-Square Test
  # ------------------------------------------------------------------------
  #### With Yates' Continuity Correction 
  .test_out <- stats::chisq.test(.test_df |> table(), correct = T)
  
  output <- output |>
    rbind(
      data.frame(
        test = "Chi-Squared",
        method = "Yates' Continuity Correction",
        corr = NA_integer_,
        stat = as.numeric(.test_out$statistic),
        param = as.numeric(.test_out$parameter),
        pval = as.numeric(.test_out$p.value)
      )
    )
  #### Without Yates' Continuity Correction
  .test_out <- stats::chisq.test(.test_df |> table(), correct = F)
  
  output <- output |>
    rbind(
      data.frame(
        test = "Chi-Squared",
        method = "No Correction",
        corr = NA_integer_,
        stat = as.numeric(.test_out$statistic),
        param = as.numeric(.test_out$parameter),
        pval = as.numeric(.test_out$p.value)
      )
    )
  
  ### McNemar's Test
  # ------------------------------------------------------------------------
  #### Check Conditions: must have 2 "raters"
  if (ncol(.test_df) == 2){
    #### Check Conditions: Discordant count must be >= 25
    if (nrow(.test_df[.test_df$var_a != .test_df$var_b,]) >= 25){
      ##### With Continuity Correction 
      .test_out <- stats::mcnemar.test(
        .test_df |> table(), correct = T
      )   
      
      output <- output |>
        rbind(
          data.frame(
            test = "McNemar",
            method = "Continuity Correction",
            corr = NA_integer_,
            stat = as.numeric(.test_out$statistic),
            param = as.numeric(.test_out$parameter),
            pval = as.numeric(.test_out$p.value)
          )
        )
      ##### Without Continuity Correction 
      .test_out <- stats::mcnemar.test(
        .test_df |> table(), correct = F
      )   
      
      output <- output |>
        rbind(
          data.frame(
            test = "McNemar",
            method = "No Correction",
            corr = NA_integer_,
            stat = as.numeric(.test_out$statistic),
            param = as.numeric(.test_out$parameter),
            pval = as.numeric(.test_out$p.value)
          )
        )
      
    }
  }
  
  ### Correlation Coefficient(s)
  # ------------------------------------------------------------------------
  for (i in seq(1, length(.corr_methods))){
    .test_out <- stats::cor(.test_df, method = .corr_methods[i])
    ### Omit the diagonals (self-self, always 1)
    diag(.test_out) <- NA
    
    output <- output |>
      rbind(
        data.frame(
          test = paste0(
            stringr::str_to_title(.corr_methods[i]), 
            "'s Correlation"
          ),
          method = "",
          corr = as.numeric(mean(.test_out, na.rm = T)),
          stat = NA_integer_,
          param = NA_integer_,
          pval = NA_integer_
        )
      )
  }
  
  ### Kappa Agreement
  # ------------------------------------------------------------------------
  #### Cohen's Kappa
  if ("cohen" %in% .kappa_methods){
    ##### Unweighted
    .test_out <- irr::kappa2(.test_df, weight = c("unweighted"))
    
    output <- output |>
      rbind(
        data.frame(
          test = "Cohen Kappa",
          method = "Unweighted",
          corr = as.numeric(.test_out$value),
          stat = as.numeric(.test_out$statistic),
          param = as.numeric(.test_out$raters),
          pval = as.numeric(.test_out$p.value)
        )
      )
    ##### Equal Weights
    .test_out <- irr::kappa2(.test_df, weight = c("equal"))
    
    output <- output |>
      rbind(
        data.frame(
          test = "Cohen Kappa",
          method = "Equal Weight",
          corr = as.numeric(.test_out$value),
          stat = as.numeric(.test_out$statistic),
          param = as.numeric(.test_out$raters),
          pval = as.numeric(.test_out$p.value)
        )
      )
    ##### Squared Weights
    .test_out <- irr::kappa2(.test_df, weight = c("squared"))
    
    output <- output |>
      rbind(
        data.frame(
          test = "Cohen Kappa",
          method = "Squared Weight",
          corr = as.numeric(.test_out$value),
          stat = as.numeric(.test_out$statistic),
          param = as.numeric(.test_out$raters),
          pval = as.numeric(.test_out$p.value)
        )
      )
  }
  
  #### Fleiss Kappa - This is a SLOW function...
  if ("fleiss" %in% .kappa_methods){
    ##### Fleiss (1971), not exact
    .test_out <- irr::kappam.fleiss(.test_df, exact = F)
    
    output <- output |>
      rbind(
        data.frame(
          test = "Fleiss Kappa",
          method = "Fleiss (1971)",
          corr = as.numeric(.test_out$value),
          stat = as.numeric(.test_out$statistic),
          param = as.numeric(.test_out$raters),
          pval = as.numeric(.test_out$p.value)
        )
      )
    ##### Exact, Conger (1980)  
    .test_out <- irr::kappam.fleiss(.test_df, exact = T)
    
    output <- output |>
      rbind(
        data.frame(
          test = "Fleiss Kappa",
          method = "Exact, Conger (1980)",
          corr = as.numeric(.test_out$value),
          stat = NA_integer_,
          param = as.numeric(.test_out$raters),
          pval = NA_integer_
        )
      )
    
  }
  
  #### Light Kappa
  if ("light" %in% .kappa_methods){
    .test_out <- irr::kappam.light(.test_df)
    
    output <- output |>
      rbind(
        data.frame(
          test = "Light Kappa",
          method = "",
          corr = as.numeric(.test_out$value),
          stat = as.numeric(.test_out$statistic),
          param = as.numeric(.test_out$raters),
          pval = as.numeric(.test_out$p.value)
        )
      )
  }
  
  ## Format output
  # ========================================================================
  output <- output |>
    ### Add marker for significance level of p-values
    dplyr::mutate(
      sig = dplyr::case_when(
        pval < 0.001 ~ "***",
        pval < 0.01 ~ "**",
        pval < 0.05 ~ "*",
        TRUE ~ ""
      )
    ) |>
    ### Add interpretation of correlation coefficient scores
    dplyr::mutate(
      corr_interp = dplyr::case_when(
        is.na(corr) ~ "",
        abs(corr) > 0.9 ~ "Almost perfect",
        abs(corr) > 0.8 ~ "Very strong",
        abs(corr) > 0.6 ~ "Strong",
        abs(corr) > 0.4 ~ "Moderate",
        abs(corr) > 0.2 ~ "Weak",
        abs(corr) > 0 ~ "Very weak",
        corr == 0 ~ "None"
      )
    ) |>
    ### Format numeric values for text display. 
    ### Convert to scientific notation if they'd require more
    ### than .n_decimal digits after the decimal to display a digit.
    dplyr::mutate_if(is.numeric, ~ifelse(
      .x < 10^(-.n_decimal),
      format(.x, digits = .n_decimal + 1, scientific = T),
      format(round(.x, .n_decimal), scientific = F)
    )
    )
  
  ## Return output
  # ========================================================================
  output
}