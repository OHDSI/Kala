# Load required packages
library(testthat)
library(tibble)
library(Kala)

context("Tests for compareTibbles")

test_that("Identical tibbles with different column and row orders", {
  # Create two tibbles with same data but different column order and row order
  tib1 <- tibble(x = c(1, 2, 3), y = c("a", "b", "c"))
  tib2 <- tibble(y = c("c", "a", "b"), x = c(3, 1, 2))

  result <- Kala::compareTibbles(tib1, tib2)

  expect_true(result$identical)
  expect_equal(result$additionalColumnsInFirst, character(0))
  expect_equal(result$additionalColumnsInSecond, character(0))
})

test_that("Tibbles with different columns", {
  # tib1 has columns x and y, tib2 has columns x and z
  tib1 <- tibble(x = c(1, 2, 3), y = c("a", "b", "c"))
  tib2 <- tibble(x = c(1, 2, 3), z = c("a", "b", "c"))

  result <- Kala::compareTibbles(tib1, tib2)

  expect_false(result$identical)
  expect_equal(result$additionalColumnsInFirst, "y")
  expect_equal(result$additionalColumnsInSecond, "z")
})

test_that("Tibbles with same columns but different number of rows", {
  # tib1 has an extra row compared to tib2
  tib1 <- tibble(x = c(1, 2, 3), y = c("a", "b", "c"))
  tib2 <- tibble(x = c(1, 2), y = c("a", "b"))

  result <- Kala::compareTibbles(tib1, tib2)

  expect_false(result$identical)
  expect_equal(result$additionalColumnsInFirst, character(0))
  expect_equal(result$additionalColumnsInSecond, character(0))
  expect_equal(result$additionalRowsInFirst, 1)
  expect_equal(result$additionalRowsInSecond, -1)

  # Verify that the extra row in tib1 is correctly identified
  expected_diff <- tib1[3, , drop = FALSE]
  expect_equal(result$presentInFirstNotSecond, expected_diff)
  expect_equal(nrow(result$presentInSecondNotFirst), 0)
})

test_that("Tibbles with same columns but different number of rows (duplicate test)", {
  tib1 <- tibble(x = c(1, 2, 3), y = c("a", "b", "c"))
  tib2 <- tibble(x = c(1, 2), y = c("a", "b"))

  result <- Kala::compareTibbles(tib1, tib2)

  expect_false(result$identical)
  expect_equal(result$additionalColumnsInFirst, character(0))
  expect_equal(result$additionalColumnsInSecond, character(0))
  expect_equal(result$additionalRowsInFirst, 1)
  expect_equal(result$additionalRowsInSecond, -1)

  expected_diff <- tib1[3, , drop = FALSE]
  expect_equal(result$presentInFirstNotSecond, expected_diff)
  expect_equal(nrow(result$presentInSecondNotFirst), 0)
})

test_that("Empty tibbles with identical columns", {
  # Both tibbles are empty but have the same column names and types
  tib1 <- tibble(x = numeric(), y = character())
  tib2 <- tibble(x = numeric(), y = character())

  result <- Kala::compareTibbles(tib1, tib2)

  expect_true(result$identical)
  expect_equal(result$additionalColumnsInFirst, character(0))
  expect_equal(result$additionalColumnsInSecond, character(0))
})

test_that("Empty tibbles with different columns", {
  # Both tibbles are empty but have different column names
  tib1 <- tibble(x = numeric(), y = character())
  tib2 <- tibble(x = numeric(), z = character())

  result <- Kala::compareTibbles(tib1, tib2)

  expect_false(result$identical)
  expect_equal(result$additionalColumnsInFirst, "y")
  expect_equal(result$additionalColumnsInSecond, "z")
})
