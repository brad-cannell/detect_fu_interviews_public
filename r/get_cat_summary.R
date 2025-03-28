#' Get a summary tibble for categorical values, human-focused
#'
#' @description This function generates a human-legible text tibble that
#'   displays the Frequency and Percent formatted text summary
#'   statistics for categorical values, with a priority for human 
#'   legibility. The number of decimal places may be named for potential 
#'   non-integer values, default 2. Value/Category order may be set, with 
#'   value-names in the table renamed as desired using vector/list names.
#'   
#'   Dependencies: dplyr, tibble, stats
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); stats (4.4.1)
#'
#' @param .df A data frame or vector
#' @param .var_name Column of interest, recommend categoricals.
#' @param .value_order A vector or list that gives the desired order to 
#'      present values of .var_name, from top to bottom, in the output tibble. 
#'      By default, it will use the results of 
#'      unique(na.omit(.df[[.var_name]])). Column names in the final output 
#'      can be renamed using list/vector naming. As such, a .var_name value 
#'      of "t" could be renamed "Positive" with list(t = "Positive") or 
#'      c(t = "Positive").
#' @param n_decimal Number of decimal places to display in values. Default
#'   value is 2 
#'
#' @return A data frame
#' @export
#'
#' @examples
#' get_cat_summary(mtcars$gear, n_decimal = 3)
#' 

get_cat_summary <- function(
    .df, .var_name = NA_character_, .val_order = NA, n_decimal = 2
    ){
  # Parse Inputs
  # =========================================================================
  # Ensure .df is a data frame or vector
  
  if (!is.data.frame(.df) & !is.vector(.df)) {
    message <- paste(
      "The value entered into .df is not a data frame or vector.", 
      "Please check input and retry."
    )
    stop(message)
  }
  
  # If .df is a vector ....
  # ------------------------------------------------------------------------- 
  # Extract column name. This will ignore anything passed into .var_name
  
  if (is.vector(.df)) {
    if (length(grep('\\$', deparse(substitute(.df)))) > 0)
      var_name <- substring(
        deparse(substitute(.df)),
        which(strsplit(deparse(substitute(.df)),'')[[1]]=='$')[1]+1)
    if (length(grep('\\[', deparse(substitute(.df)))) > 0 )
      var_name <- substring(
        deparse(substitute(.df)), 
        # Start position of cut
        which(strsplit(deparse(substitute(.df)),'')[[1]]=='[')[2]+2,
        # End position of cut
        which(strsplit(deparse(substitute(.df)),'')[[1]]==']')[1]-2
      )
    
    # If "var_name" also exists, return a warning that it is being ignored
    if (!is.na(.var_name)){
      message <- ".df is a vector. Ignoring .var_name argument."
      warning(message)
    }
    
    # Use passed vector for processing
    t_vector <- .df
    
  }
  
  # If .df is not a vector ....
  # -------------------------------------------------------------------------
  if (!is.vector(.df)){
    
    # If there's more than one value for .var_name, raise an error
    if (length(.var_name) > 1) {
      message <- paste(
        "Multiple values in .var_name argument. Only one column allowed.", 
        "Please check input and retry."
      )
      stop(message)
    }
    # If there's no value for .var_name, raise an error
    if (is.na(.var_name)) {
      message <- paste(
        "Requires a value for .var_name, none provided.", 
        "Please check input and retry."
      )
      stop(message)
    }
    # If .var_name is not a string, raise an error
    if (!is.character(.var_name)){
      message <- paste(
        "Parameter .var_name requires a character column/vector name.", 
        "Please check input and retry."
      )
      stop(message)
    }
    # If .var_name is not in the dataframe, raise an error
    if (!.var_name %in% colnames(.df)){
      message <- paste0(
        "Variable '", .var_name, "' is not in the .df. ", 
        "Please check input and retry."
      )
      stop(message)
    }
    
    # Select column/vector for processing
    t_vector <- .df[[.var_name]] 
  }
  
  # Check and Lightly Process .val_order
  # -------------------------------------------------------------------------
  ## Get list of unique non-missing values in .var_name, and convert to
  ## character to avoid name issues
  var_values <- as.character(unique(na.omit(t_vector)))
  
  ## If .val_order exists and is neither a list nor a vector, overwrite with
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
  
  # Generate Categorical Summary Statistics Output, Text-Based
  # =========================================================================
  ## Initiate dataframe, to enforce order of values in table presentation
  output <- data.frame(value_name = c(names(.val_order), 'total'))
  
  ## Get actual counts data and add it to the ordered dataframe
  output <- output |>
    dplyr::left_join(
      ### Get frequency count for each value in the target .var_name
      data.frame(t_vector) |>
        purrr::map_df(table) |>
        ### Get total by summing all values
        dplyr::mutate(
          total = sum(dplyr::across(dplyr::everything()), na.rm = T)
        ) |>
        ### Transpose to get a column of counts (each row is a value)
        t() |>
        as.data.frame() |>
        ### Pull rownames to avoid data moving about
        tibble::rownames_to_column(var = 'value_name') |>
        ### Name columns "n", "perc", "text" for standardization
        dplyr::rename_at('V1', ~'n') |>
        dplyr::mutate(
          perc = n / max(n),
          #### Text output as "N (%)", with desired number of decimal places.
          text = paste0(
            format(n, big.mark =','), " (",
            stringr::str_trim(
              format(round(perc*100,n_decimal), nsmall = n_decimal), 
              side = 'both'
            ), 
            "%)"
          )
        ),
      by = 'value_name'
    ) |>
    ### Rename value options using value order list
    dplyr::mutate(value_name = c(as.character(.val_order), 'TOTAL'))|>
    ### Replace any missing values with 0
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric), 
      ~tidyr::replace_na(.x,0)
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character),
      ~tidyr::replace_na(.x, "0 (0%)")
    ))
  
  ## Return output table
  output
}