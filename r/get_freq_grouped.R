#' Package frequency count and percent of values in vectors, by group
#'
#' @description This function generates a table giving the frequency
#'   count and percent for values in one or more columns of a data frame
#'   when grouped. Useful for time-period groups. Calculates the number
#'   of non-missing entries, and gives percentage based on the total
#'   number of records and total non-missing records.
#'    
#'   Dependencies: dplyr, stats
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); stats (4.4.1)
#'
#' @param .df A data frame
#' @param .group_var Variable in .df that determines group membership, such
#'	as those that mark time intervals
#' @param .item_list A vector or list that gives the names of columns
#' 	of interest. Must exist in .df. If a named list, uses names as
#'	the column names in .df
#' @param .val_order A vector or list that contains the minimum expected
#'	set of values across all columns in .df[,names(.item_list)].
#'	Must exist in .df. If a named list, uses names as the values. 
#'	Default is NA, which simply selects all unique values in 
#'	.df[,names(.item_list)] 
#'
#' @return A data frame
#' @export
#'
get_freq_grouped <- function(
    .df, .group_var, .item_list, .val_order = NA
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
  # Validate if .item_list is reasonable
  # -------------------------------------------------------------------------
  # If .item_list is given...
  if(sum(!is.na(.item_list)) > 0){
    ## Ensure .item_list was given as a list or vector
    if (!is.list(.item_list)|!is.vector(.item_list)){
      message <- paste(
        "Parameter .item_list is expected to be a vector or list.", 
        "Check input and try again."
      )
      stop(message)
    }
    ## If .item_list was not given with names, make the values names
    if(is.null(names(.item_list))){
      message <- paste(
        "Parameter .item_list given without names. Making values names.", 
      )
      names(.item_list) <- item_list
      warning(message)
    }
    ## Ensure values of names(.item_list) are columns in .df
    if(sum(!(names(.item_list) %in% colnames(.df))) > 0){
      message <- paste0(
        ".item_list references columns that are not in .df! ",
        "Check input and try again."
      )
      stop(message)
    }
  }
  # If .item_list does not exist, raise an stop
  if (sum(is.na(.item_list)) > 0) {
    message <- paste(
      ".item_list is required.", 
      "Check input and try again."
    )
    stop(message)
  }
  # Check and Lightly Process .val_order
  # -------------------------------------------------------------------------
  ## Get list of unique non-missing values in names(.item_list) columns
  
  var_values <- unique(na.omit(unlist(
    .df[,names(.item_list)]
  )))
  
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
    names(.val_order) = as.character(.val_order)
    warning(message)
  }
  ## If names(.val_order) contains values not in names(.item_list) columns 
  ## of .df, raise a warning
  if(
    sum(!is.na(.val_order)) > 0 & 
    sum(!names(.val_order) %in% as.character(var_values)) > 0
  ) {
    message <- paste0(
      ".val_order contains values that are not in any .item_list cols of .df.",
      " Did you make a typo?."
    )
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
  # Process into packaged data!
  # =========================================================================
  ## Initiate output table
  ## ------------------------------------------------------------------------
  output <- data.frame(
    matrix(
        vector(), 
        # 0 rows
        0, 
        # 8 columns
        8,
        # Set column names
        dimnames=list(
          c(), 
          c(
            .group_var, 'var_name', 'response', 'n', 'per_comp', 'per_total',
            'num_records', 'total_comp'
            )
      )),
      stringsAsFactors=F
      ) |>
      ## Make .group_var, "var_name", and "response" characters, 
      ## for compatibility
      dplyr::mutate(dplyr::across(
        dplyr::all_of(c(.group_var, 'var_name', 'response')), 
        ~as.character(.x)
      )) |>
      ## Make remaining columns numeric, for compatibility
      dplyr::mutate(dplyr::across(
        !dplyr::all_of(c(.group_var, 'var_name', 'response')), 
        ~as.numeric(.x)
      ))
  
  ## For each item in .item_list...
  for (i in seq(1, length(names(.item_list)))){
    
    ## Get the item's count table (frequency count of values per .group_var)
    ## ----------------------------------------------------------------------
    .item_counts <- dplyr::bind_rows(
      ### Initialize frame with empty dataframe with no rows:
      #### Column for .group_var and each value in .val_order, to ensure 0
      #### counts for any item in .val_order that is missing in the item!
      data.frame(matrix(
        vector(), 
        # 0 rows
        0, 
        # ncolumns = number of items in .val_order + 1 for .group_var
        length(names(.val_order))+1,
        # No row names. Take column names from .group_var and .val_order items
        dimnames=list(c(), c(.group_var, names(.val_order)))
      ),
      stringsAsFactors=F
      ) |>
        # Ensure .group_var is a factor, for later processing
        dplyr::mutate(dplyr::across(
          dplyr::all_of(.group_var), ~factor(.x))
        ) |>
        # Convert columns that will hold counts to numeric, for later NA fixes
        dplyr::mutate(dplyr::across(
          dplyr::all_of(names(.val_order)), ~as.numeric(.x))
        ),
      ### Extract count of values in .item_list[i], by .group_var
      .df |>
        # Ensure .group_var is a factor, for later processing
        dplyr::mutate(dplyr::across(dplyr::all_of(.group_var), ~factor(.x))) |>
        # Get counts
        dplyr::count(
          !!rlang::sym(.group_var), 
          !!rlang::sym(names(.item_list)[i])
        )  |>
        # Pivot into .group_var - value1 - value2 - etc format
        tidyr::pivot_wider(
          names_from = names(.item_list[i]),
          values_from = n
        )
    ) |>
      ### Additional processing
      ## --------------------------------------------------------------------
    #### If NA values in .item_list[i] weren't already processed into 
    #### "missing", rename the resulting "NA" count column to "missing"
    dplyr::rename_with(
      ~ case_when(
        . == "NA" ~ "missing",
        TRUE ~ .
      )
    ) |>
      # Replace any missing counts (such as values that are in .val_order but
      # aren't in .item_list[i]) with 0
      dplyr::mutate(dplyr::across(
        dplyr::where(is.numeric),
        ~tidyr::replace_na(.x,0)
      )) %>%
      # Requires maggritr pipe (%>%) for rowSums to process correctly
      dplyr::mutate(
        ### Get total number of records but summing all counts for .group_var
        num_records = rowSums(.[setdiff(names(.),.group_var)]),
        ### Calculate the total that aren't missing from the total - missing
        total_comp = num_records - missing,
        completed = total_comp
      ) |>
      ### Pivot to .group_var - "response" - n - num_records - total_comp
      tidyr::pivot_longer(
        cols = dplyr::all_of(c(names(.val_order), 'missing', 'completed')),
        names_to = 'response',
        values_to = 'n'
      ) |>
      ### Add percentages
      dplyr::mutate(
        per_comp = n / total_comp,
        per_total = n / num_records
      ) |>
      ### Add names(.item_list)[i] to "var_name"
      dplyr::mutate(
        var_name = names(.item_list)[i]
      )
    
    ## Add to output list
    ## ----------------------------------------------------------------------
    output <- dplyr::bind_rows(output, .item_counts)
  }
  
  ## Return output
  ## -----------------------------------------------------------------------
  output
}