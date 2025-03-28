#' Get a Summary of Two Sample Numeric Tests
#'
#' @description This function generates a human-legible text table that 
#'   displays the test, statistic value, and p-value for Student T-Test and 
#'   Wilcox Rank-Sum tests, using a boolean/logical vector to assign groups.
#'   
#'   Dependencies: dplyr, stats
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); stats (4.4.1)
#'
#' @param .df A data frame.
#' @param .var_name Column of numeric values in .df. Must be numeric.
#' @param .binary_var Column that indicates A/B group identity in .df. 
#'      Must be boolean/logical.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' get_twos_num_stats(
#'  .df = mtcars
#'    dplyr::mutate(am = am == 0),
#'  .var_name = 'mpg',
#'  .binary_var = 'am'
#'  )
#'

get_twos_num_stats <- function(.df, .var_name, .binary_var){
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
  # Validate if .var_name is reasonable
  # -------------------------------------------------------------------------
  # If .var_name is given, make sure it was given as a string
  if (!is.na(.var_name) & !is.character(.var_name)){
    message <- paste(
      "Parameter .var_name requires a character column/vector name.", 
      "Check input and try again."
    )
    stop(message)
  }
  # If there's more than one value for .var_name, raise an stop
  if (!is.na(.var_name) & length(.var_name) > 1) {
    message <- paste(
      "Multiple values in .var_name argument. Only one column allowed.", 
      "Check input and try again."
    )
    stop(message)
  }
  # If .var_name was given, make sure it exists in .df
  if(!is.na(.var_name) & !(.var_name %in% colnames(.df))){
    message <- paste0(
      .var_name, " was given for .var_name, but it is not in .df! ",
      "Check input and try again."
    )
    stop(message)
  }
  # If .var_name was given, be sure it's numeric.
  if (!is.na(.var_name) & !is.numeric(.df[[.var_name]])) {
    message <- paste(
      ".var_name must be numeric, but it is not.", 
      "Check input and try again."
    )
    stop(message)
  }
  # If .var_name does not exist, raise an stop
  if (is.na(.var_name)) {
    message <- paste(
      ".var_name is required.", 
      "Check input and try again."
    )
    stop(message)
  }
  # Validate if .binary_var is reasonable
  # -------------------------------------------------------------------------
  # If .binary_var is given, make sure it was given as a string
  if (!is.na(.binary_var) & !is.character(.binary_var)){
    message <- paste(
      "Parameter .binary_var requires a character column/vector name.", 
      "Check input and try again."
    )
    stop(message)
  }
  # If there's more than one value for .binary_var, raise an stop
  if (!is.na(.binary_var) & length(.binary_var) > 1) {
    message <- paste(
      "Multiple values in .binary_var argument. Only one column allowed.", 
      "Check input and try again."
    )
    stop(message)
  }
  # If .binary_var was given, make sure it exists in .df
  if(!is.na(.binary_var) & !(.binary_var %in% colnames(.df))){
    message <- paste0(
      .binary_var, " was given for .binary_var, but it is not in .df! ",
      "Check input and try again."
    )
    stop(message)
  }
  # If ..binary_var was given, be sure it's boolean/logical.
  if (!is.na(.binary_var) & !is.logical(.df[[.binary_var]])) {
    message <- paste(
      ".binary_var must be boolean/logical vector, but it is not.", 
      "Check input and try again."
    )
    stop(message)
  }
  # If .binary_var does not exist, raise an stop
  if (is.na(.binary_var)) {
    message <- paste(
      ".binary_var is required.", 
      "Check input and try again."
    )
    stop(message)
  }
  # Perform Calculations and Form Output
  # =========================================================================
  ## Extract group values
  ## ------------------------------------------------------------------------
  true_vals <- .df |>
    dplyr::filter(!!rlang::sym(.binary_var)) |>
    dplyr::select(!!rlang::sym(.var_name)) |>
    dplyr::pull()
  
  false_vals <- .df |>
    dplyr::filter(!(!!rlang::sym(.binary_var))) |>
    dplyr::select(!!rlang::sym(.var_name)) |>
    dplyr::pull()
  
  ## Perform Statistical Tests
  ## ------------------------------------------------------------------------
  ### Student T-Test of Means
  t_vals <- stats::t.test(
    x = true_vals, 
    y = false_vals,
    alternative = 'two.sided'
  )
  ### Wilcox Rank-Sum Test
  w_vals <- stats::wilcox.test(
    x = true_vals,
    y = false_vals,
    alternative = 'two.sided'
  )
  
  ## Place results into a formatted summary table for human legibility
  ## ------------------------------------------------------------------------
  output <- data.frame(
    test = c("Student T-Test", "Wilcox Rank-Sum"),
    stat = c(t_vals$statistic[['t']], w_vals$statistic[['W']]),
    param = c(t_vals$parameter[['df']], NA),
    pval = c(t_vals$p.value, w_vals$p.value)
  ) |>
    dplyr::mutate(
      sig = dplyr::case_when(
        pval < 0.001 ~ "***",
        pval < 0.01 ~ "**",
        pval < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  ## Return the output table
  ## ------------------------------------------------------------------------
  output
}