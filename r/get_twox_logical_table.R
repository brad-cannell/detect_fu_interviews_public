#' Produce human-legible formatted 2x2 agreement table
#'
#' @description This function generates a table giving the agreement
#'	between two variables, formatted for human legibility as text.
#'    
#'   Dependencies: dplyr
#'   
#'   Built: R (4.4.1); dplyr (1.1.4)
#'
#' @param .df A data frame
#' @param .var_a Variable in .df with two values. Must be in .df.
#'	Must be given as a string. Will be the first column of table.
#' @param .var_b Variable in .df with two values. Must be in .df.
#'	Must be given as a string. Will be the columns.
#' @param .var_a_labs A vector or list that gives the desired values
#'	in .df[[.var_a]]. Use naming to give labels, such as 
#'	list(t = "true label", f = "false label"). Must be values in
#'	.df[[.var_a]], length of 2, labels as strings.
#' @param .var_b_labs A vector or list that gives the desired values
#'	in .df[[.var_b]]. Use naming to give labels, such as 
#'	list(t = "true label", f = "false label"). Must be values in
#'	.df[[.var_b]], length of 2, labels as strings. Names must be
#'	identical to names of .var_a_labs
#'
#' @return A data frame
#' @export
#'
get_twox_logical_table <- function(
    .df, .var_a, .var_b, .var_a_labs = NA, .var_b_labs = NA
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
  
  # Process into human-legible table
  # =========================================================================
  ## Get 2x2 Contingency Table Values
  ## ------------------------------------------------------------------------
  .count_cont <- .df |>
    ### Reduce to desired columns
    dplyr::select(dplyr::all_of(c(.var_a, .var_b))) |>
    ### Filter to only include rows with the desired values
    dplyr::filter(
      !!rlang::sym(.var_a) %in% names(.var_a_labs) &
        !!rlang::sym(.var_b) %in% names(.var_b_labs)
    ) |>
    table() |> 
    as.data.frame() |>
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
    ) |>
    ### Label type to extract
    dplyr::mutate(
      type = case_when(
        var_a & var_b ~ "t_t",
        var_a & !var_b ~ "t_f",
        !var_a & var_b ~ "f_t",
        !var_a & !var_b ~ "f_f"
      )
    ) 
  
  ## Pull values into a formatted 2x2 contingency table, formatted for
  ## human legibility
  ## ------------------------------------------------------------------------
  output <- data.frame(
    ### Left column: .var_a TRUE, .var_A FALSE, TOTAL
    val_a = c(
      as.character(.var_a_labs),
      "TOTAL"
    ),
    ### Second column: .var_b TRUE (matching first column)
    var_b_t =
      c(
        .count_cont[.count_cont$type == 't_t',]$Freq,
        .count_cont[.count_cont$type == 'f_t',]$Freq,
        sum(.count_cont[.count_cont$var_b,]$Freq)
      ),
    ### Third column: .var_b FALSE (matching first column)
    var_b_f =
      c(
        .count_cont[.count_cont$type == 't_f',]$Freq,
        .count_cont[.count_cont$type == 'f_f',]$Freq,
        sum(.count_cont[!.count_cont$var_b,]$Freq)
      ),
    ### Fourth column: Row-wise totals
    TOTAL = c(
      sum(.count_cont[.count_cont$var_a,]$Freq),
      sum(.count_cont[!.count_cont$var_a,]$Freq),
      sum(.count_cont$Freq)
    )
  ) |>
    ### Rename columns for .var_b using labels
    dplyr::rename_at(
      c('var_b_t', 'var_b_f'), ~as.character(.var_b_labs)
    ) |>
    ### Format all numeric cells as text 
    dplyr::mutate(
      dplyr::across(!val_a, ~format(.x, big.mark = ','))
    )
  
  ## Return output
  ## ------------------------------------------------------------------------
  output
}