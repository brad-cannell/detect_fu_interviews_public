# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unit tests for recoding_factoring_relocating.R
# Helper functions for recoding character columns to numeric columns, then 
# converting the numeric columns into factors, then relocating the factor 
# version of the column directly after the numeric version.
# Brad Cannell
# 2024-04-05
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Style Notes: 
# - Leave 2 blank lines above level 1 headings.
# - Leave 1 blank line above all other headings.


# Load functions ----
source(here::here("R", "recoding_factoring_relocating.R"))


# Data for testing ----
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


# char_to_num ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Takes a categorical character column and a named vector of numeric category 
# codes for each category as inputs and returns a numeric version of the 
# column. Later, the numeric column will be converted to a factor.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Results checks
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

## Error checks
testthat::test_that("Test to make sure that .recode contains the same values as the .col", {
  # Value in .col, but not .recode
  labs_n_y <- c("No" = 0, "Check" = 1)
  testthat::expect_error(
    mtcars_test |> dplyr::mutate(vs = char_to_num(vs, labs_n_y)),
    "Values in .col, but not .recode: Yes"
  )
})


# chars_to_nums ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Takes a character vector of categorical character column names and a named 
# vector of numeric category codes for each category as inputs and returns a 
# numeric version of each column. Later, the numeric columns will be converted 
# to factors.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Results checks
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


# factors ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Takes a character vector of categorical numeric column names and a named 
# vector of numeric category codes for each category as inputs and returns a 
# factor version of each column. A "_f" is appended to the end of the factor
# column name.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Results checks
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


# relocate_factors ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Relocate Factor Version Of Columns Immediately After Numeric Version
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Results checks
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