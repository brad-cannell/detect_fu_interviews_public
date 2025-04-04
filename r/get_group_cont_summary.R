#' Get a summary tibble for numeric values by group, human-focused
#'
#' @description This function generates a human-legible text tibble that
#'   displays both Mean (SD) and Median (IQR, Range) formatted text summary
#'   statistics for numeric values, by group, with a priority for human 
#'   legibility. The number of decimal places may be named for potential 
#'   non-integer values, default 2. Column order may be set, with columns
#'   renamed as desired using vector/list names.
#'   
#'   Dependencies: dplyr, tibble, stats, get_cont_summary()
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); stats (4.4.1)
#'
#' @param .df A data frame
#' @param .var_name Numeric column of interest.
#' @param .group_var Variable in .df that determines group membership. Omits 
#'      rows with missing (NA) .group_var values from processing.
#' @param .group_order A vector or list that gives the desired order of the
#'      groups, from right to left, in the output tibble. By default, it will
#'      use the results of unique(na.omit(.df[[.group_var]])). Column names in
#'      the final output can be renamed using list/vector naming. As such,
#'      a .group_var value of "blue" could be renamed "North America" with 
#'      list(blue = "North America") or c(blue = "North America").
#' @param n_decimal Number of decimal places to display in values. Default
#'   value is 2 
#'
#' @return A data frame
#' @export
#'
#' @examples
#' get_group_cont_summary(mtcars, "mpg", "cyl", n_decimal = 3)
#'   

get_group_cont_summary <- function(
    .df, .var_name, .group_var, .group_order = NA, n_decimal = 2
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
  # Check .var_name
  # -------------------------------------------------------------------------
  ## If there's more than one value for "var_name", raise an error
  if (length(.var_name) > 1) {
    message <- paste(
      "Multiple values in 'var_name' argument. Only one column allowed.", 
      "Please check input and retry."
    )
    stop(message)
  }
  ## If there's no value for "var_name", raise an error
  if (is.na(.var_name)) {
    message <- paste(
      "Requires a value for 'var_name', none provided.", 
      "Please check input and retry."
    )
    stop(message)
  }
  ## If "var_name" is not a string, raise an error
  if (!is.character(.var_name)){
    message <- paste(
      "Parameter 'var_name' requires a character column/vector name.", 
      "Please check input and retry."
    )
    stop(message)
  }
  # If "var_name" is not in the dataframe, raise an error
  if (!.var_name %in% colnames(.df)){
    message <- paste0(
      "Variable '", .var_name, "' is not in the .df. ", 
      "Please check input and retry."
    )
    stop(message)
  }
  # If .var_name is not numeric, raise an error
  if (!is.numeric(.df[[.var_name]])){
    message <- paste0(
      "Variable '", .var_name, "' must be numeric. ", 
      "Please check input and retry."
    )
    stop(message)
  }
  # Check .group_var
  # -------------------------------------------------------------------------
  ## If there's more than one value for .group_var, raise an error
  if (length(.group_var) > 1) {
    message <- paste(
      "Multiple values in .group_var argument. Only one column allowed.", 
      "Please check input and retry."
    )
    stop(message)
  }
  ## If there's no value for .group_var, raise an error
  if (is.na(.group_var)) {
    message <- paste(
      "Requires a value for .group_var, none provided.", 
      "Please check input and retry."
    )
    stop(message)
  }
  ## If .group_var is not a string, raise an error
  if (!is.character(.group_var)){
    message <- paste(
      "Parameter .group_var requires a character column/vector name.", 
      "Please check input and retry."
    )
    stop(message)
  }
  ## If .group_var is not in the dataframe, raise an error
  if (!.group_var %in% colnames(.df)){
    message <- paste0(
      "Variable '", .group_var, "' is not in the .df. ", 
      "Please check input and retry."
    )
    stop(message)
  }
  # Check and Lightly Process .group_order
  # -------------------------------------------------------------------------
  ## Get list of unique non-missing values in .group_var, and convert to
  ## character to avoid name issues
  group_values <- as.character(unique(na.omit(.df[[.group_var]])))
  
  ## If .group_order exists but is neither a list nor a vector, overwrite with
  ## all non-NA values in .group_var
  if(
    (sum(is.na(.group_order)) > 0) & 
    !(is.list(.group_order) | is.vector(.group_order))
  ){
    message <- paste0(
      ".group_order must be a list or vector. Ignoring passed value."
    )
    .group_order = group_values
    warning(message)
  }
  ## If .group_order does not have names, make names the values 
  ## (eg. "a" = "a")
  if(sum(!is.na(.group_order)) > 0 & is.null(names(.group_order))){
    message <- paste0(
      ".group_order entered without names. Using values for names."
    )
    names(.group_order) = .group_order
    warning(message)
  }
  ## If group_order values are not in values of .group_var, raise an error
  if(
    sum(!is.na(.group_order)) > 0 & 
    sum(!names(.group_order) %in% group_values) > 0
  ){
    message <- paste0(
      ".group_order contains values not present in .group_var.",
      "Please check input and retry."
    )
    stop(message)
  }
  ## If .group_order somehow does not exist at this point, raise an error
  if(
    sum(is.na(.group_order)) > 0|sum(is.null(.group_order)) > 0
  ){
    message <- paste0(
      "Unexpected error: .group_order does not exist"
    )
    stop(message)
  }
  
  # Pull Group Stats using get_cont_summary()
  # =========================================================================
  ## Set up first column (summary type)
  ### Calls the function in case any minor tweaks happen with funct output
  output <- get_cont_summary(mtcars$mpg) |>
    dplyr::select(summary_type)
  
  ## Add columns for each group, based on order, named based on list
  for (i in seq(1, length(names(.group_order)))){
    ### Filter to only the desired group
    group_vector <- .df |>
      dplyr::filter(!!rlang::sym(.group_var) == names(.group_order)[i]) |>
      dplyr::select(!!rlang::sym(.var_name)) |>
      dplyr::pull()
    
    ### Calculate the statistics and append column
    output <- output |>
      dplyr::left_join(
        get_cont_summary(group_vector, n_decimal = n_decimal) |>
          dplyr::rename_at('summary_text', ~as.vector(.group_order[[i]])),
        by = 'summary_type'
      )
  }
  
  # Display output data frame.
  output
}