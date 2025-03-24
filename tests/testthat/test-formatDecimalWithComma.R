# Load testthat package
library(testthat)

context("Tests for formatDecimalWithComma")

test_that("Default behavior with rounding enabled", {
  # Default settings: decimalPlaces = 1, round = TRUE
  # For number 1234567.8912, floor(1234567.8912)=1234567 and 0.8912 rounds to 0.9
  expect_equal(formatDecimalWithComma(1234567.8912), "1,234,567.9")
})

test_that("Rounding disabled with specified decimal places", {
  # When rounding is disabled, the decimal portion is truncated
  # For number 1234567.8912 with 2 decimal places:
  # floor(1234567.8912)=1234567; decimal=0.8912 -> truncated to 0.89
  expect_equal(
    formatDecimalWithComma(1234567.8912, decimalPlaces = 2, round = FALSE),
    "1,234,567.89"
  )
})

test_that("Handles negative numbers correctly", {
  # For a negative number, floor(-1234567.8912) becomes -1234568,
  # and the decimal part is computed as -1234567.8912 - (-1234568) = 0.1088,
  # which rounds to 0.1 with one decimal place.
  expect_equal(formatDecimalWithComma(-1234567.8912), "-1,234,568.1")
})

test_that("Handles numbers with zero decimal part", {
  # When the number is an integer, the decimal part is 0
  # For 1000, we expect the formatted output to include a decimal part "0"
  expect_equal(formatDecimalWithComma(1000), "1,000.0")
})

test_that("Rounding effect: when decimal rounds up to 1.0", {
  # Consider number 999.999:
  # floor(999.999) = 999, and 0.999 rounds to 1.0 with 1 decimal place.
  # This results in a formatted output of "999.0", because the integer part is not adjusted.
  expect_equal(
    formatDecimalWithComma(999.999, decimalPlaces = 1, round = TRUE),
    "999.0"
  )
})

test_that("Truncation does not round up", {
  # Using the same number with truncation should not round up the decimal part.
  # For 999.999 with 1 decimal place and round = FALSE, the decimal part becomes 0.9.
  expect_equal(
    formatDecimalWithComma(999.999, decimalPlaces = 1, round = FALSE),
    "999.9"
  )
})

test_that("Handles truncation with multiple decimal places", {
  # For number 1234.5678 with 2 decimal places and round = FALSE:
  # floor(1234.5678)=1234 and trunc(0.5678*100)/100 = 0.56
  expect_equal(
    formatDecimalWithComma(1234.5678, decimalPlaces = 2, round = FALSE),
    "1,234.56"
  )
})

test_that("Handles zero decimalPlaces", {
  # When decimalPlaces is 0, the decimal part is rounded/truncated to an integer.
  # For 1234.5678 with decimalPlaces = 0, floor(1234.5678)=1234,
  # and round(0.5678, 0) yields 1, but no digits remain after the decimal point.
  # The expected output is "1,234." (an empty decimal part after the period).
  expect_equal(
    formatDecimalWithComma(1234.5678, decimalPlaces = 0, round = TRUE),
    "1,234."
  )
})

test_that("Handles numbers less than 1", {
  # For a number less than 1, e.g. 0.12345 with 2 decimal places:
  # floor(0.12345)=0 and round(0.12345, 2)=0.12, so the formatted output is "0.12"
  expect_equal(
    formatDecimalWithComma(0.12345, decimalPlaces = 2, round = TRUE),
    "0.12"
  )
})

test_that("Handles large integers", {
  # For large numbers that are exact integers, the function should still add the decimal part.
  # For 1000000, we expect "1,000,000.0"
  expect_equal(formatDecimalWithComma(1000000), "1,000,000.0")
})
