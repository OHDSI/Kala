library(testthat)
library(Kala)

context("Tests for formatCountPercent, formatIntegerWithComma, and formatPercent")

### Tests for formatIntegerWithComma ###
test_that("formatIntegerWithComma formats whole numbers correctly", {
  expect_equal(Kala:::formatIntegerWithComma(1234567), "1,234,567")
  expect_equal(Kala:::formatIntegerWithComma(100), "100")
  expect_equal(Kala:::formatIntegerWithComma(0), "0")
})

test_that("formatIntegerWithComma handles negative numbers and decimals", {
  expect_equal(Kala:::formatIntegerWithComma(-987654), "-987,654")
  # When given a non-integer numeric, the function formats the integer part only.
  expect_equal(Kala:::formatIntegerWithComma(12345.67), "12345")
})

### Tests for formatPercent ###
test_that("formatPercent formats percentages with default digits", {
  # Default digits = 2 should yield two decimal places
  expect_equal(Kala:::formatPercent(0.789), "78.90%")
  expect_equal(Kala:::formatPercent(0), "0.00%")
})

test_that("formatPercent respects the digits parameter", {
  expect_equal(Kala:::formatPercent(0.789, digits = 1), "78.9%")
  expect_equal(Kala:::formatPercent(0.5, digits = 0), "50%")
})

test_that("formatPercent handles negative percentages", {
  expect_equal(Kala:::formatPercent(-0.123), "-12.30%")
})

### Tests for formatCountPercent ###
test_that("formatCountPercent returns correctly formatted count and percentage", {
  # Using default percentDigits (which is 1)
  expect_equal(Kala:::formatCountPercent(123456, 0.789), "123,456 (78.9%)")

  # Explicitly specifying percentDigits = 2
  expect_equal(
    Kala:::formatCountPercent(987654, 0.321, percentDigits = 2),
    "987,654 (32.10%)"
  )

  # Test with zero values for both count and percent
  expect_equal(Kala:::formatCountPercent(0, 0), "0 (0.0%)")

  # Test with zero values for both count and percent with 2 digits
  expect_equal(
    Kala:::formatCountPercent(0, 0, percentDigits = 2),
    "0 (0.00%)"
  )

  # Test with negative values
  expect_equal(
    Kala:::formatCountPercent(-123456, -0.789, percentDigits = 1),
    "-123,456 (-78.9%)"
  )
})
