# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper functions for recoding character columns to numeric columns, then 
# converting the numeric columns into factors, then relocating the factor 
# version of the column directly after the numeric version.
# Brad Cannell
# 2024-04-05
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data for testing ----
# Eventually move this to an R script.
mtcars_test <- mtcars |>
  # Create character columns, because that's what we are working with in the
  # DETECT data.
  dplyr::mutate(
    # Two dichotomous columns so we can test the function inside of across()
    vs = dplyr::if_else(vs == 0, "No", "Yes"),
    am = dplyr::if_else(am == 0, "No", "Yes"),
    # One variable with more than 2 levels
    cyl = dplyr::case_when(
      cyl == 4 ~ "Four",
      cyl == 6 ~ "Six",
      cyl == 8 ~ "Eight"
    )
  )


#' Recode A Character Column To Numeric Column
#' 
#' @description
#' Takes a categorical character column and a named vector of numeric category 
#' codes for each category as inputs and returns a numeric version of the 
#' column. Later, the numeric column will be converted to a factor.
#'
#' @param .col The name of the column containing the categorical character values.
#' @param .recode A named vector of numeric category codes.
#'
#' @return A categorical numeric vector.
#' 
char_to_num <- function(.col, .recode) {
  # Check to make sure that recode contains the same values as the column
  col_vals <- unique(.col)
  char_vals <- names(.recode)
  in_col_not_recode <- dplyr::setdiff(col_vals, char_vals)
  in_col_not_recode <- in_col_not_recode[!is.na(in_col_not_recode)] # Don't include NA
  if (length(in_col_not_recode) > 0) {
    stop("Values in .col, but not .recode: ", in_col_not_recode)
  }
  
  # Recode character values to numeric values
  for (i in seq_along(char_vals)) {
    .col[.col == char_vals[i]] <- .recode[i]
  }
  .col <- as.numeric(.col)
  # Return vector
  .col
}

## Tests ----

### Results checks
testthat::test_that("Test that char_to_num results as expected", {
  labs_n_y <- c("No" = 0, "Yes" = 1)
  result <- mtcars_test |> 
    dplyr::mutate(vs = char_to_num(vs, labs_n_y))
  
  testthat::expect_equal(
    result$vs, 
    c(
      0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 
      0, 1, 0, 1, 0, 0, 0, 1
    )
  )
})

### Error checks
testthat::test_that("Test to make sure that .recode contains the same values as the .col", {
  # Value in .col, but not .recode
  labs_n_y <- c("No" = 0, "Check" = 1)
  testthat::expect_error(
    mtcars_test |> dplyr::mutate(vs = char_to_num(vs, labs_n_y)),
    "Values in .col, but not .recode: Yes"
  )
})





#' Recode Multiple Character Columns To Numeric Columns
#' 
#' @description
#' Takes a character vector of categorical character column names and a named 
#' vector of numeric category codes for each category as inputs and returns a 
#' numeric version of each column. Later, the numeric columns will be converted 
#' to factors.
#'
#' @param .data A data frame.
#' @param .cols A character vector of categorical character column names.
#' @param .recode A named vector of numeric category codes for each category.
#'
#' @return
#' 
chars_to_nums <- function(.data, .cols, .recode) {
  .data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(.cols),
        ~ char_to_num(.x, .recode)
      )
    )
}

## Tests ----

### Results checks
testthat::test_that("Test that chars_to_nums results as expected", {
  labs_n_y <- c("No" = 0, "Yes" = 1)
  cols <- c("vs", "am")
  result <- mtcars_test |> 
    chars_to_nums(cols, labs_n_y)
  
  # Test vs values
  testthat::expect_equal(
    result$vs, 
    c(
      0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 
      0, 1, 0, 1, 0, 0, 0, 1
    )
  )
  
  # Test am values
  testthat::expect_equal(
    result$am, 
    c(
      1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 
      0, 1, 1, 1, 1, 1, 1, 1
    )
  )
})





#' Coerce Multiple Numeric Columns To Factors
#' 
#' @description
#' Takes a character vector of categorical numeric column names and a named 
#' vector of numeric category codes for each category as inputs and returns a 
#' factor version of each column. A "_f" is appended to the end of the factor
#' column name.
#'
#' @param .data A data frame.
#' @param .cols A character vector of categorical numeric column names.
#' @param .recode A named vector of numeric category codes for each category.
#'
#' @return A data frame
#' 
factors <- function(.data, .cols, .recode) {
  .data |>
    dplyr::mutate(
      dplyr::across(
        .cols  = dplyr::all_of(.cols),
        .fns   = ~ factor(.x, .recode, names(.recode)),
        .names = "{col}_f"
      )
    )
}

## Tests ----

### Results checks
testthat::test_that("Test that factors results as expected", {
  labs_n_y <- c("No" = 0, "Yes" = 1)
  cols <- c("vs", "am")
  result <- mtcars_test |> 
    chars_to_nums(cols, labs_n_y) |> 
    factors(cols, labs_n_y)
  
  # Test vs values
  testthat::expect_equal(
    result$vs, 
    c(
      0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 
      0, 1, 0, 1, 0, 0, 0, 1
    )
  )
  
  # Test vs_f values
  testthat::expect_equal(
    result$vs_f, 
    factor(c(
      "No", "No", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", 
      "No", "No", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes", 'No', 
      "No", "No", "No", "Yes", "No", "Yes", "No", "No", "No", "Yes"
    ))
  )
})





#' Relocate Factor Version Of Columns Immediately After Numeric Version
#'
#' @param .data A data frame.
#' @param .cols A character vector of categorical numeric column names.
#'
#' @return A data frame.
#' 
relocate_factors <- function(.data, .cols) {
  # Loop over each column in .cols. Relocate the _f version immediately after
  # the numeric version.
  for (col in .cols) {
    col_f <- paste0(col, "_f")
    .data <- .data |>
      dplyr::relocate(all_of(col_f), .after = all_of(col))
  }
  # Return data frame
  .data
}

## Tests ----

### Results checks
testthat::test_that("Test that the relocate_factors() results area as expected", {
  y_n_labs <- c("No" = 0, "Yes" = 1)
  cols <- c("vs", "am")
  results <- mtcars_test |>
    chars_to_nums(cols, y_n_labs) |>
    factors(cols, y_n_labs) |>
    relocate_factors(cols)
  
  # Test vs location
  testthat::expect_equal(which(colnames(results) == "vs"), 8L)
  # Test vs_f values
  testthat::expect_equal(which(colnames(results) == "vs_f"), 9L)
})

# Clean up ----
rm(mtcars_test)
