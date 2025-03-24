# Load the testthat package
library(testthat)

context("Tests for convertCohortIdToCovariateId")

test_that("Correct conversion for a single cohortId using default analysis ID", {
  # For a single value, e.g., 1, the expected value is (1 * 1000) + 150 = 1150
  cohortId <- 1
  expected <- (cohortId * 1000) + 150
  result <- convertCohortIdToCovariateId(cohortId)
  expect_equal(result, expected)
})

test_that("Correct conversion for a vector of cohortIds using default analysis ID", {
  # Test vector input: cohortIds = 1, 2, 3
  cohortIds <- c(1, 2, 3)
  expected <- (cohortIds * 1000) + 150
  result <- convertCohortIdToCovariateId(cohortIds)
  expect_equal(result, expected)
})

test_that("Conversion works with a custom cohortCovariateAnalysisId", {
  # Test with a custom analysis ID, e.g., 200
  cohortIds <- c(10, 20)
  customId <- 200
  expected <- (cohortIds * 1000) + customId
  result <- convertCohortIdToCovariateId(cohortIds, cohortCovariateAnalysisId = customId)
  expect_equal(result, expected)
})

test_that("Handles zero cohortId correctly", {
  # When cohortId is 0, expected value is (0 * 1000) + 150 = 150
  cohortId <- 0
  expected <- (0 * 1000) + 150
  result <- convertCohortIdToCovariateId(cohortId)
  expect_equal(result, expected)
})

test_that("Handles negative cohortIds correctly", {
  # Test with negative values to ensure arithmetic is correctly applied
  cohortIds <- c(-1, -2)
  expected <- (cohortIds * 1000) + 150
  result <- convertCohortIdToCovariateId(cohortIds)
  expect_equal(result, expected)
})

test_that("Output is of numeric type", {
  # Confirm that the function returns a numeric vector (double type)
  cohortIds <- 5
  result <- convertCohortIdToCovariateId(cohortIds)
  expect_type(result, "double")
})
