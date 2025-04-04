#' Get a summary tibble for continuous numeric values, human-focused
#'
#' @description This function generates a human-legible text tibble that
#'   displays both Mean (SD) and Median (IQR, Range) formatted text summary
#'   statistics, with a priority for human legibility. The number of decimal
#'   places may be named for potential non-integer values, default 2.
#'   
#'   Dependencies: dplyr, tibble, stats
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); stats (4.4.1)
#'
#' @param .df A data frame or vector
#' @param .var_name Numeric column of interest. Omitted if .df is a vector.
#' @param n_decimal Number of decimal places to display in values. Default
#'   value is 2 
#'
#' @return A data frame
#' @export
#'
#' @examples
#' get_cont_summary(mtcars$mpg, n_decimal = 3)
#'   

get_cont_summary <- function(.df, .var_name = NA_character_, n_decimal = 2){
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
  # Extract column name. This will ignore anything passed into var_name
  
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
      message <- ".df is a vector. Ignoring 'var_name' argument."
      warning(message)
    }
    
    # Use passed vector for processing
    t_vector <- .df
    
  }
  
  # If .df is not a vector ....
  # -------------------------------------------------------------------------
  
  if (!is.vector(.df)){
    
    # If there's more than one value for "var_name", raise an error
    if (length(.var_name) > 1) {
      message <- paste(
        "Multiple values in 'var_name' argument. Only one column allowed.", 
        "Please check input and retry."
      )
      stop(message)
    }
    # If there's no value for "var_name", raise an error
    if (is.na(.var_name)) {
      message <- paste(
        "Requires a value for 'var_name', none provided.", 
        "Please check input and retry."
      )
      stop(message)
    }
    # If "var_name" is not a string, raise an error
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
    
    # Select column/vector for processing
    t_vector <- .df[[.var_name]] 
  }
  # Generate Continuous Numeric Summary Statistics Output, Text-Based
  # =========================================================================
  ## Extract summary statistics
  stat_vals <- data.frame(
    ## Calculate and place into a basic df
    stat_name = names(summary(t_vector)),
    stat_val = as.vector(summary(t_vector))
  ) |>
    ## Add standard deviation
    dplyr::bind_rows(
      data.frame(
        stat_name = 'sd',
        stat_val = stats::sd(stats::na.omit(t_vector))[1]
      )
    ) |>
    ## Rename statistic name values for simplicity
    dplyr::mutate(
      stat_name = c('min', 'q1', 'med', 'mean', 'q3', 'max', 'sd')
    ) |>
    ## Get a clean text version of values. Use n_decimal decimal places 
    ## unless the value is an integer.
    dplyr::rowwise() |>
    dplyr::mutate(
      val_txt = ifelse(
        stat_val %% 1 == 0,
        format(stat_val, big.mark = ','),
        format(round(stat_val, n_decimal), big.mark = ',', nsmall = n_decimal)
      )
    ) |>
    dplyr::ungroup()
  
  # Convert to a human-legible simple text summary
  output <- data.frame(
    summary_type = c('Mean (SD)', 'Median (IQR, Range)'),
    summary_text = c(
      ## Parametric/Mean Stats Text
      paste0(
        stat_vals[stat_vals$stat_name == 'mean',]$val_txt, 
        " (", 
        stat_vals[stat_vals$stat_name == 'sd',]$val_txt,
        ")"
      ),
      ## Nonparametric/Median IQR Stats Text
      paste0(
        stat_vals[stat_vals$stat_name == 'med',]$val_txt, 
        " (IQR ", 
        stat_vals[stat_vals$stat_name == 'q1',]$val_txt,
        " - ",
        stat_vals[stat_vals$stat_name == 'q3',]$val_txt,
        ", Range ",
        stat_vals[stat_vals$stat_name == 'min',]$val_txt,
        " - ",
        stat_vals[stat_vals$stat_name == 'max',]$val_txt,
        ")"
      )
    )
  )
  
  # Return output data frame
  output
}