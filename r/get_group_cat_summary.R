#' Get a summary tibble for categorical values by group, human-focused
#'
#' @description This function generates a human-legible text tibble that
#'   displays the Frequency and Percent formatted text summary
#'   statistics for categorical values, by group, with a priority for human 
#'   legibility. The number of decimal places may be named for potential 
#'   non-integer values, default 2. Value/Category and Group orders may be 
#'   set, with value-names and columns in the table renamed as desired using 
#'   vector/list names.
#'   
#'   Dependencies: dplyr, tibble, stats, get_cat_summary()
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); stats (4.4.1)
#'
#' @param .df A data frame
#' @param .var_name Column of interest, recommend categoricals.
#' @param .value_order A vector or list that gives the desired order to 
#'      present values of .var_name, from top to bottom, in the output tibble. 
#'      By default, it will use the results of 
#'      unique(na.omit(.df[[.var_name]])). Column names in the final output 
#'      can be renamed using list/vector naming. As such, a .var_name value 
#'      of "t" could be renamed "Positive" with list(t = "Positive") or 
#'      c(t = "Positive").
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
#' get_group_cat_summary(mtcars, "cyl", "gear", n_decimal = 3)
#' 

get_group_cat_summary <- function(
    .df, .var_name, .group_var, .val_order = NA, .group_order = NA, 
    n_decimal = 2
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
  # Check and Lightly Process .val_order
  # -------------------------------------------------------------------------
  ## Get list of unique non-missing values in .var_name, and convert to
  ## character to avoid name issues
  var_values <- as.character(unique(na.omit(.df[[.var_name]])))
  
  ## If .val_order exists but is neither a list nor a vector, overwrite with
  ## all non-NA values in .var_name
  if(
    (sum(is.na(.val_order)) > 0) & 
    !(is.list(.val_order) | is.vector(.val_order))
  ){
    message <- paste0(
      ".val_order must be a list or vector. Ignoring passed value."
    )
    .val_order = var_values
    warning(message)
  }
  ## If .val_order does not have names, make names identical to the values. 
  ## (eg. "a" = "a")
  if(sum(!is.na(.val_order)) > 0 & is.null(names(.val_order))){
    message <- paste0(
      ".val_order entered without names. Using values for names."
    )
    names(.val_order) = .val_order
    warning(message)
  }
  ## If .val_order somehow does not exist at this point, raise an error
  if(
    sum(is.na(.val_order)) > 0|sum(is.null(.val_order)) > 0
  ){
    message <- paste0(
      "Unexpected error: .val_order does not exist"
    )
    stop(message)
  }
  
  # Generate Categorical Summary Statistics with get_cat_summary()
  # =========================================================================
  ## Initiate dataframe, to enforce order of values in table presentation
  output <- data.frame(value_name = c(as.character(.val_order), 'TOTAL'))
  
  ## Add columns for each group, based on order, named based on list
  for (i in seq(1, length(names(.group_order)))){
    ### Filter to only the desired group
    group_vector <- .df |>
      dplyr::filter(!!rlang::sym(.group_var) == names(.group_order)[i]) |>
      dplyr::select(!!rlang::sym(.var_name)) |>
      dplyr::pull()
    
    ### Get actual counts data and add it to the group set of the dataframe
    output <- output |>
      dplyr::left_join(
        #### Call get_cat_summary()
        get_cat_summary(
          .df |>
            dplyr::filter(
              !!rlang::sym(.group_var) == names(.group_order)[i]
            ) |>
            dplyr::select(!!rlang::sym(.var_name)) |>
            dplyr::pull(), 
          .val_order = .val_order,
          n_decimal = n_decimal
        ) |>
          #### Rename columns to reflect group name values
          dplyr::rename_at(
            c('n', 'perc', 'text'),
            ~c(
              paste0(names(.group_order)[i], "_n"),
              paste0(names(.group_order)[i], "_perc"),
              .group_order[[i]]
            )
          ),
        by = 'value_name'
      ) 
  }
  
  ## Return output table
  output
}