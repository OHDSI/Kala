library(testthat)
library(Kala)

context("Tests for commaSeparatedStringToIntArray")

test_that("Basic conversion of integer strings", {
  input <- "1,2,3,4"
  expected <- c(1, 2, 3, 4)
  result <- Kala::commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("Handles trailing comma", {
  input <- "5,6,7,"
  expected <- c(5, 6, 7)
  result <- Kala::commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("Handles multiple consecutive commas", {
  input <- "8,,9,,,10"
  expected <- c(8, 9, 10)
  result <- Kala::commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("Handles extra spaces around numbers", {
  input <- " 11, 12 , 13  "
  expected <- c(11, 12, 13)
  result <- Kala::commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("Handles non-numeric values gracefully", {
  input <- "14,abc,16"
  result <- suppressWarnings(Kala::commaSeparatedStringToIntArray(input))
  expected <- c(14, NA, 16)
  expect_equal(result, expected)
})

test_that("Empty string returns an empty numeric vector", {
  input <- ""
  expected <- numeric(0)
  result <- Kala::commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("String with only commas returns empty vector", {
  input <- ",,,"
  expected <- numeric(0)
  result <- Kala::commaSeparatedStringToIntArray(input)
  expect_equal(result, expected)
})

test_that("Long numeric string converts correctly", {
  numbers <- paste(1:100, collapse = ",")
  expected <- 1:100
  result <- Kala::commaSeparatedStringToIntArray(numbers)
  expect_equal(result, expected)
})
