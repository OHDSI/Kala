# Load required packages
library(testthat)
library(tibble)
library(Kala)

test_that("StdDiff All default", {
  stdDiff <- getFeatureExtractionStandardizedDifference(
    covariateData1Path = system.file("testdata", "covariateData_cohortId_1.zip", package = "Kala"),
    covariateData2Path = system.file("testdata", "covariateData_cohortId_4.zip", package = "Kala"),
    cohortId1 = 1, cohortId2 = 4
  )


  stopifnot(nrow(stdDiff) == 34779)
  stopifnot(ncol(stdDiff) == 10)

  stopifnot(stdDiff$stdDiff[1] <= -1.91784 & stdDiff$stdDiff[1] >= -1.91785)

  stopifnot(mean(stdDiff$stdDiff, na.rm = T) >= -0.0108157 & mean(stdDiff$stdDiff, na.rm = T) <= -0.0108156)

  expect_equal(length(stdDiff[stdDiff$stdDiff == "NaN", ]$stdDiff), 92)
})

test_that("StdDiff With TimeRef)", {
  timeRef <- tibble(
    timeId = c(1, 2, 3, 4),
    startDay = c(-9999, -9999, -9999, 0),
    endDay = c(0, -1, 9999, 9999)
  )

  stdDiff <- getFeatureExtractionStandardizedDifference(
    covariateData1Path = system.file("testdata", "covariateData_cohortId_1.zip", package = "Kala"),
    covariateData2Path = system.file("testdata", "covariateData_cohortId_4.zip", package = "Kala"),
    cohortId1 = 1, cohortId2 = 4, timeRef = timeRef
  )


  stopifnot(nrow(stdDiff) == 3070)
  stopifnot(ncol(stdDiff) == 10)

  stopifnot(stdDiff$stdDiff[1] <= -1.91784 & stdDiff$stdDiff[1] >= -1.91785)

  stopifnot(mean(stdDiff$stdDiff, na.rm = T) >= -0.00755967 & mean(stdDiff$stdDiff, na.rm = T) <= -0.0075596)

  expect_equal(length(stdDiff[stdDiff$stdDiff == "NaN", ]$stdDiff), 15)
})

test_that("StdDiff Error 1)", {
  timeRef <- tibble(
    timeId = c(1, 2, 3, 4),
    startDay = c(-9999, -9999, -9999, 0),
    endDay = c(0, -1, 9999, 9999)
  )

  expect_message(getFeatureExtractionStandardizedDifference(
    covariateData1Path = system.file("testdata", "covariateData_cohortId_1.zip", package = "Kala"),
    covariateData2Path = system.file("testdata", "covariateData_cohortId_4.zip", package = "Kala"),
    cohortId1 = 1, cohortId2 = 4, timeRef = NULL, includeNonTimeVarying = FALSE
  ), "includeNonTimeVarying is FALSE and timeRef is NULL. no results.")
})

test_that("StdDiff Error 2)", {
  timeRef <- tibble(
    timeId = c(1, 2, 3, 4),
    startDay = c(-9999, -9999, -9999, 0),
    endDay = c(0, -1, 9999, 9999)
  )

  expect_error(getFeatureExtractionStandardizedDifference(
    covariateData1Path = NULL,
    covariateData2Path = NULL,
    cohortId1 = 1, cohortId2 = 4, timeRef = timeRef
  ), "covariateData1/2 and path are all NULL")
})

test_that("StdDiff Error 3)", {
  timeRef <- tibble(
    timeId = c(),
    startDay = c(),
    endDay = c()
  )

  expect_error(getFeatureExtractionStandardizedDifference(
    covariateData1Path = system.file("testdata", "covariateData_cohortId_1.zip", package = "Kala"),
    covariateData2Path = system.file("testdata", "covariateData_cohortId_4.zip", package = "Kala"),
    cohortId1 = 1, cohortId2 = 4, timeRef = timeRef
  ), "please check timeRef")
})
