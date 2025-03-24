# Load the testthat package
library(testthat)

# Assuming commaSeparatedStringToIntArray is already defined in your environment.
# If it's in a separate file, you can source it like:
# source("path/to/your/function_file.R")

context("Tests for commaSeparatedStringToIntArray")

test_that("Basic conversion of integer strings", {
  input <- "1,2,3,4"
  expected <- c(1, 2, 3, 4)
  result <- commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("Handles trailing comma", {
  input <- "5,6,7,"
  expected <- c(5, 6, 7)
  result <- commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("Handles multiple consecutive commas", {
  input <- "8,,9,,,10"
  expected <- c(8, 9, 10)
  result <- commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("Handles extra spaces around numbers", {
  # Even though extra spaces aren't explicitly trimmed by the function,
  # as.double() can handle leading/trailing spaces.
  input <- " 11, 12 , 13  "
  expected <- c(11, 12, 13)
  result <- commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("Handles non-numeric values gracefully", {
  # Conversion of non-numeric strings produces NA with a warning.
  input <- "14,abc,16"
  # Suppress the warning for test clarity
  result <- suppressWarnings(commaSeparatedStringToIntArray(input))
  expected <- c(14, NA, 16)
  expect_equal(result, expected)
})

test_that("Empty string returns an empty numeric vector", {
  input <- ""
  expected <- numeric(0)
  result <- commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("String with only commas returns empty vector", {
  input <- ",,,"
  expected <- numeric(0)
  result <- commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("Long numeric string converts correctly", {
  # Create a long string of comma-separated numbers 1 to 100
  numbers <- paste(1:100, collapse = ",")
  expected <- 1:100
  result <- commaSeparatedStringToIntArray(numbers)
  expect_equal(result, expected)
})
