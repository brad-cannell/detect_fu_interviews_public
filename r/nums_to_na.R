# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function To Help Convert Selected Numeric Values To NAs
# Brad Cannell
# 2024-04-09
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data for testing ----
mtcars_test <- mtcars
# Add 7's and 9's to the mtcars data
mtcars_test$vs[1] <- 7
mtcars_test$vs[3] <- 9
mtcars_test$am[2] <- 7
mtcars_test$am[4] <- 9
mtcars_test$cyl[1] <- 99
# Add some attributes. We want to make sure the aren't' dropped.
attr(mtcars_test$vs, "description") <- "The vs column from mtcars"
attr(mtcars_test$vs, "value_labels") <- c("No" = 0, "Yes" = 1, "Don't Know" = 7, "Refused" = 9)
attr(mtcars_test$am, "description") <- "The am column from mtcars"
attr(mtcars_test$am, "value_labels") <- c("No" = 0, "Yes" = 1, "Don't Know" = 7, "Refused" = 9)
attr(mtcars_test$cyl, "description") <- "The cyl column from mtcars"
attr(mtcars_test$cyl, "value_labels") <- c("Four cyl" = 4, "Six cyl" = 6, "Eight cyl" = 8, "Unknown" = 99)


#' Function To Help Convert Selected Numeric Values To NA
#' 
#' @description
#' Numeric categorical variables often code responses like "Don't know" and 
#' "Refused" as 7 and 9 or 77 and 99. This function will help us convert those
#' response to NA while preserving column attributes (e.g., labels). 
#'
#' @param .col The name of the column containing the numerical values to be 
#'   converted to NA.
#' @param .na_values A vector of numeric values to convert to NA. 
#'
#' @return A numeric vector.
num_to_na <- function(.col, .na_values) {
  # Store the attributes
  x_attr <- attributes(.col)
  # Convert numeric values to NA's
  .col <- dplyr::if_else(.col %in% .na_values, NA_real_, .col)
  # Add attributes back to the vector
  attributes(.col) <- x_attr
  # Return .col
  .col
}

## Tests ----

results <- mtcars_test |>
  # One column
  dplyr::mutate(cyl = num_to_na(cyl, 99)) |> 
  # Multiple columns with across
  dplyr::mutate(
    dplyr::across(
      c(vs, am),
      ~ num_to_na(.x, c(7, 9))
    )
  )

### Attributes checks
testthat::test_that("Test to make sure that data frame attributes are retained.", {
  # Test the description attribute
  testthat::expect_equal(
    attr(results$cyl, "description"),
    "The cyl column from mtcars"
  )
  
  # Test the value_labels attribute
  testthat::expect_equal(
    attr(results$cyl, "value_labels"),
    c("Four cyl" = 4, "Six cyl" = 6, "Eight cyl" = 8, "Unknown" = 99)
  )
})

### Results checks
testthat::test_that("Test that num_to_na() results are as expected", {
  # Test vs values
  testthat::expect_equal(
    results$vs[1:4], 
    c(NA, 0, NA, 1)
  )
  
  # Test am values
  testthat::expect_equal(
    results$am[1:4], 
    c(1, NA, 1, NA)
  )
})

### Clean up ----
rm(results)


#' Function To Help Convert Selected Numeric Values To NA Across Multiple Columns
#' 
#' @description
#' This function is similar to num_to_na(). However, instead of passing it a 
#' single numeric column, you pass it a data frame and a vector of column names 
#' with numeric values to convert to NA.
#'
#' @param .data A data frame.
#' @param .cols The name of the columns containing the numerical values to be 
#'   converted to NA.
#' @param .na_values A vector of numeric values to convert to NA. 
#' @param .suffix A suffix attached to the original column name. This function
#'   assumes that the new column name (i.e., the column with "Don't know" and
#'   "Refused" converted to NA) will have the form 
#'   "{original column name}_{.suffix}".
#'
#' @return A data frame.
nums_to_nas <- function(.data, .cols, .na_values, .suffix) {
  .data |> 
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(.cols),
        ~ num_to_na(.x, .na_values),
        .names = "{col}_{.suffix}"
      )
    )
}

## Tests ----

testthat::test_that("Test that nums_to_nas() results are as expected", {
  # Columns to manipulate
  cols <- c("vs", "am")
  # Suffix for the version of each column with "Don't Know" and "Refused" changed
  # to NA.
  suffix <- "2cat"
  # NA values
  na_values <- c(7, 9)
  # Manipulate the data
  results <- mtcars_test |>
    nums_to_nas(cols, na_values, suffix)
  
  # Test vs values
  testthat::expect_equal(
    results$vs_2cat[1:4], 
    c(NA, 0, NA, 1)
  )
  
  # Test am values
  testthat::expect_equal(
    results$am_2cat[1:4], 
    c(1, NA, 1, NA)
  )
})


#' Relocate New Column With NA Values Immediately After The Original Column
#'
#' @param .data A data frame.
#' @param .cols A character vector of categorical numeric column names.
#' @param .suffix A suffix attached to the original column name. This function
#'   assumes that the new column name (i.e., the column with "Don't know" and
#'   "Refused" converted to NA) will have the form 
#'   "{original column name}_{.suffix}".
#'
#' @return A data frame.
#' 
relocate_na_cols <- function(.data, .cols, .suffix) {
  # Loop over each column in .cols. Relocate the new version immediately after
  # the original version.
  for (col in .cols) {
    col_suffix <- paste(col, .suffix, sep = "_")
    .data <- .data |>
      dplyr::relocate(all_of(col_suffix), .after = all_of(col))
  }
  # Return data frame
  .data
}

## Tests ----

testthat::test_that("Test that relocate_na_cols() results are as expected", {
  # Columns to manipulate
  cols <- c("vs", "am")
  # Suffix for the version of each column with "Don't Know" and "Refused" changed
  # to NA.
  suffix <- "2cat"
  # NA values
  na_values <- c(7, 9)
  # Manipulate the data
  results <- mtcars_test |>
    nums_to_nas(cols, na_values, suffix) |> 
    relocate_na_cols(cols, suffix)

  # Test vs location
  testthat::expect_equal(which(colnames(results) == "vs"), 8L)
  # Test vs_f values
  testthat::expect_equal(which(colnames(results) == "vs_2cat"), 9L)
})


# Clean up ----
rm(mtcars_test)
