# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unit tests for nums_to_na.R
# Functions To Help Convert Selected Numeric Values To NAs
# Brad Cannell
# 2024-04-09
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Style Notes: 
# - Leave 2 blank lines above level 1 headings.
# - Leave 1 blank line above all other headings.


# Load functions ----
source(here::here("R", "nums_to_na.R"))


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


# num_to_na ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Numeric categorical variables often code responses like "Don't know" and 
# "Refused" as 7 and 9 or 77 and 99. This function will help us convert those
# response to NA while preserving column attributes (e.g., labels). 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Create a data frame of results to test below.
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

## Attributes checks
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

## Results checks
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


# nums_to_nas ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This function is similar to num_to_na(). However, instead of passing it a 
# single numeric column, you pass it a data frame and a vector of column names 
# with numeric values to convert to NA. 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


# relocate_na_cols ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Relocate New Column With NA Values Immediately After The Original Column. 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
rm(results)