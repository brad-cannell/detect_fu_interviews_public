#' Get a summary table for DETECT Tool Response Patterns
#'
#' @description This function generates a human-legible text table that
#'   displays the Frequency and Percent formatted text summary
#'   statistics for DETECT Tool Response Patterns. This includes values for:
#'   Times Completed; Times answered "Yes", "No", "UTA"; Times the "Yes" for
#'   an item was the only "Yes" in the record. Times Completed uses the total
#'   number of records as a denominator for percentages. Times answered "Yes", 
#'   "No", "UTA" use the number of completed responses as the denominator for
#'   percentages. Times the "Yes" for an item was the only "Yes" in the record
#'   uses the number of "yes" responses as the denominator for percentages.
#'   Both text and values are returned. The number of decimal places may be 
#'   named for potential non-integer values, default 2. Item order may be set, 
#'   with text-based values for item name in the table renamed as desired 
#'   using vector/list names.
#'   
#'   Dependencies: dplyr, tibble, stats
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); stats (4.4.1)
#'
#' @param .df A data frame or vector with DETECT tool items as columns. Only
#'      character-based values of "yes", "no", and "uta" are counted, though
#'      any present value may skew the "completed" count.
#' @param .dt_item_list A vector or list that gives the desired DETECT tool
#'      item column names, in the desired order for presentation, from top to 
#'      bottom, in the output table. By default, it will use the values of 
#'      .dt_item_list. Text values in the final output can be renamed using 
#'      list/vector naming. As such, a .var_name value of "t" could be 
#'      renamed "Positive" with list(t = "Positive") or c(t = "Positive").
#' @param n_decimal Number of decimal places to display in values. Default
#'   value is 2 
#'
#' @return A data frame
#' @export
#'

get_dt_resp_pattern <- function(.df, .dt_item_list = NULL, n_decimal = 2){
  # Parse Inputs
  # =========================================================================
  # Ensure .df is a data frame
  if (!is.data.frame(.df)) {
    message <- paste(
      "The value entered into .df is not a data frame. ", 
      "Please check input and retry."
    )
    stop(message)
  }
  
  # Check and Lightly Process .dt_item_list
  # -------------------------------------------------------------------------
  ## If .dt_item_list exists and is neither a list nor a vector, raise error
  if(
    (sum(is.na(.dt_item_list)) > 0) & 
    !(is.list(.dt_item_list) | is.vector(.dt_item_list))
  ){
    message <- paste0(
      ".dt_item_list must be a list or vector. ",
      "Please check input and retry."
    )
    stop(message)
  }
  ## If .dt_item_list does not have names, make names identical to the values. 
  ## (eg. "a" = "a")
  if(sum(!is.na(.dt_item_list)) > 0 & is.null(names(.dt_item_list))){
    message <- paste0(
      ".dt_item_list entered without names. Using values for names."
    )
    names(.dt_item_list) = .dt_item_list
    warning(message)
  }
  ## If .dt_item_list values are not in .df, raise an error
  if(
    sum(!is.na(.dt_item_list)) > 0 & 
    sum(!names(.dt_item_list) %in% colnames(.df)) > 0
  ){
    message <- paste0(
      ".dt_item_list contains values not present in .df.",
      "Please check input and retry."
    )
    stop(message)
  }
  ## If .dt_item_list somehow does not exist at this point, raise an error
  if(
    sum(is.na(.dt_item_list)) > 0|sum(is.null(.dt_item_list)) > 0
  ){
    message <- paste0(
      ".dt_item_list is required. ",
      "Please check input and retry."
    )
    stop(message)
  }
  
  # Create Response-Pattern Table
  # =========================================================================
  # Subset the .df to just have the DETECT Item Columns, if not already done
  # -------------------------------------------------------------------------
  dt_item_df <- .df |>
    dplyr::select(dplyr::all_of(names(.dt_item_list)))
  
  # Flag if the row has only one "yes" response
  # -------------------------------------------------------------------------
  dt_item_df$one_yes <- rowSums(
    dt_item_df |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(), 
        ~.x == 'yes'
      )), na.rm = T) == 1
  
  # Initialize the output data frame
  # -------------------------------------------------------------------------
  output <- data.frame(
    text_var_val = character(), text_completed = character(), 
    text_yes = character(), text_no = character(), text_uta = character(), 
    text_yes_only = character(),
    n_no = integer(), n_uta = integer(), n_yes = integer(), 
    n_completed = integer(), n_yes_only = integer(), 
    per_no = double(), per_uta = double(), per_yes = double(), 
    per_completed = double(), per_yes_only = double(), 
    var_val = character()
  )
  
  # For each DETECT Item in .dt_item_list....
  # -------------------------------------------------------------------------
  for (i in seq(1, length(.dt_item_list))){
    
    
    ## Calculate the number of rows where the item was completed
    n_completed <- sum(!is.na(dt_item_df[[names(.dt_item_list)[i]]]))
    
    output <- output |>
      dplyr::bind_rows(
        ## Extract the counts of each answer type ('yes', 'no', 'uta')
        as.data.frame(table(dt_item_df[[names(.dt_item_list)[i]]])) |>
          dplyr::mutate(
        ## Calculate the percentage of completions that gave each answer type
            per = Freq / n_completed
          ) |>
          ## Add the count and percentages for completion and "only yes"
          dplyr::bind_rows(data.frame(
            Var1 = c('completed', 'yes_only'),
            Freq = c(
              n_completed, 
              ### Extracts the count of responses where the "yes" to the given 
              ### item was the "only yes" in the record
              ### NOTE: This only checks the items in .dt_item_list!
              nrow(dt_item_df |>
                     dplyr::filter(
                       one_yes & 
                         !!rlang::sym(names(.dt_item_list)[i]) == "yes")
              )
            ),
            per = c(
            ### Completion % uses the number of records as the denominator
              n_completed / nrow(dt_item_df),
            ### "Only yes" uses the number of "yes" answers as the denominator
              nrow(dt_item_df |>
                     dplyr::filter(
                       one_yes & 
                         !!rlang::sym(names(.dt_item_list)[i]) == "yes")
              ) / nrow(dt_item_df |>
                         dplyr::filter(
                           !!rlang::sym(names(.dt_item_list)[i]) == "yes")
                       )
            ))
          ) |>
        ## Consolidate count and percent into a text-based summary cell
          dplyr::mutate(
            text = paste0(
              format(Freq, big.mark = ','), 
              " (",
              stringr::str_trim(
                format(round(per*100,n_decimal), nsmall = n_decimal),
                side = 'both'), 
              "%)"
            )
          ) |>
        ## Rename "Freq" so that after pivot the names are "n_yes", etc.
          dplyr::rename_at('Freq', ~'n') |>
        ## Pivot the data frame so that each DETECT item is a single row
          tidyr::pivot_wider(
            names_from = c('Var1'), 
            values_from = c('n', 'per', 'text'), 
            names_glue = "{.value}_{Var1}"
          ) |>
        ## Add the DETECT Item variable name and text-based name to the output
          dplyr::mutate(
            var_val = names(.dt_item_list)[i],
            text_var_val = as.character(.dt_item_list[i])
          ) 
      )
  }
  
  # Replace any missing values in the output with 0
  # ------------------------------------------------------------------------
  output <- output |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric), 
      ~tidyr::replace_na(.x,0)
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character),
      ~tidyr::replace_na(.x, "0 (0%)")
    ))
  
  # Return output
  # -------------------------------------------------------------------------
  output
}