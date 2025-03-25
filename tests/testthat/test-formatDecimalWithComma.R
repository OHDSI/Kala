library(testthat)
library(Kala) # Load your package

context("Tests for formatDecimalWithComma")

test_that("Default behavior with rounding enabled", {
  expect_equal(
    Kala::formatDecimalWithComma(1234567.8912),
    "1,234,567.9"
  )
})

test_that("Rounding disabled with specified decimal places", {
  expect_equal(
    Kala::formatDecimalWithComma(1234567.8912, decimalPlaces = 2, round = FALSE),
    "1,234,567.89"
  )
})

test_that("Handles negative numbers correctly", {
  expect_equal(
    Kala::formatDecimalWithComma(-1234567.8912),
    "-1,234,568.1"
  )
})

test_that("Handles numbers with zero decimal part", {
  expect_equal(Kala::formatDecimalWithComma(1000), "1,000.0")
})

test_that("Rounding effect: when decimal rounds up to 1.0", {
  expect_equal(
    Kala::formatDecimalWithComma(999.999, decimalPlaces = 1, round = TRUE),
    "999.0"
  )
})

test_that("Truncation does not round up", {
  expect_equal(
    Kala::formatDecimalWithComma(999.999, decimalPlaces = 1, round = FALSE),
    "999.9"
  )
})

test_that("Handles truncation with multiple decimal places", {
  expect_equal(
    Kala::formatDecimalWithComma(1234.5678, decimalPlaces = 2, round = FALSE),
    "1,234.56"
  )
})

test_that("Handles zero decimalPlaces", {
  expect_equal(
    Kala::formatDecimalWithComma(1234.5678, decimalPlaces = 0, round = TRUE),
    "1,234."
  )
})

test_that("Handles numbers less than 1", {
  expect_equal(
    Kala::formatDecimalWithComma(0.12345, decimalPlaces = 2, round = TRUE),
    "0.12"
  )
})

test_that("Handles large integers", {
  expect_equal(Kala::formatDecimalWithComma(1000000), "1,000,000.0")
})
