#' Get counts of binary/logical vectors, by group
#'
#' @description This function generates a reduced table that gives the 
#'   count of "TRUE" values for a list of binary/logical vectors and the
#'   total number of records within a grouping variable. Useful for
#'   frequency counts by a time group. 
#'    
#'   Dependencies: dplyr, stats
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); stats (4.4.1)
#'
#' @param .df A data frame
#' @param .group_var Variable in .df that determines group membership, such
#'	as those that mark time intervals
#' @param .binary_vars A vector or list that gives the names of columns
#' 	with binary/logical data. Must be logical, must exist in .df. Must
#'	be passed as strings.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' binary_count_group(mtcars |> dplyr::mutate(am = am=="0"), "cyl", "am")
#'   

binary_count_group <- function(.df, .group_var, .binary_vars){
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
  # Validate if .group_var is reasonable
  # -------------------------------------------------------------------------
  # If .group_var is given...
  if(sum(!is.na(.group_var)) > 0){
    ## make sure it was given as a string
    if (!is.character(.group_var)){
      message <- paste(
        "Parameter .group_var requires a character column/vector name.", 
        "Check input and try again."
      )
      stop(message)
    }
    # If there's more than one value for .group_var, raise an stop
    if (length(.group_var) > 1) {
      message <- paste(
        "Multiple values in .group_var argument. Only one allowed.", 
        "Check input and try again."
      )
      stop(message)
    }
    # If .group_var was given, make sure it exists in .df
    if(!(.group_var %in% colnames(.df))){
      message <- paste0(
        .group_var, " was given for .group_var, but it is not ",
        "in .df! Check input and try again."
      )
      stop(message)
    }
  }
  # If .group_var does not exist, raise an stop
  if (is.na(.group_var)) {
    message <- paste(
      ".group_var is required.", 
      "Check input and try again."
    )
    stop(message)
  }
  # Validate if .binary_vars is reasonable
  # -------------------------------------------------------------------------
  # If .binary_vars is given...
  if(sum(!is.na(.binary_vars)) > 0){
    ## make sure it was given as a string
    if (!is.character(.binary_vars)){
      message <- paste(
        "Parameter .binary_vars requires a character column/vector name.", 
        "Check input and try again."
      )
      stop(message)
    }
    # If .binary_vars was given, make sure it exists in .df
    if(sum(!(.binary_vars %in% colnames(.df)))>0){
      message <- paste0(
        ".binary_vars contains values not in .df! ",
        "Check input and try again."
      )
      stop(message)
    }
    # If .binary_vars was given, be sure they're binary/logical.
    ## NOTE: I'm sure there's a better way to do this...
    .num_vars <- length(.binary_vars)
    .num_logical <- 0 
    for (i in seq(1, .num_vars)){
      .num_logical <- .num_logical + sum(is.logical(.df[[.binary_vars[i]]]))
    }
    if (.num_logical != .num_vars) {
      message <- paste(
        ".binary_vars must be logical, but one or more columns are not.", 
        "Check input and try again."
      )
      stop(message)
    }
  }
  # If .binary_vars does not exist, raise an stop
  if (sum(is.na(.binary_vars)) > 0) {
    message <- paste(
      ".binary_vars is required.", 
      "Check input and try again."
    )
    stop(message)
  }
  # Create counts per "TRUE" for .binary_vars, by .group_var
  # ========================================================================
  output <- .df |>
    ## Reduce to only necessary columns
    dplyr::select(dplyr::all_of(c(.group_var, .binary_vars))) |>
    ## Group by the .group_var for calculations
    dplyr::group_by(!!rlang::sym(.group_var)) |>
    ## Get count of "TRUE" values for all binary variables
    dplyr::mutate(
      dplyr::across(dplyr::all_of(.binary_vars), ~sum(.x, na.rm = T))
    ) |>
    ## Get the number of records in each time period
    dplyr::mutate(
      num_rows = max(dplyr::row_number())
    ) |>
    ## Reduce to one row per time period
    dplyr::ungroup() |>
    dplyr::distinct() |>
    ## Arrange by time period, for convenience
    dplyr::arrange(!!rlang::sym(.group_var))
  
  ## Return output
  output
}