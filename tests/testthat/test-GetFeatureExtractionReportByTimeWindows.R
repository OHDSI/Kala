# Load required packages
library(testthat)
library(tibble)
library(Kala)

test_that("Get report (ByWindows, With Table Data)", {
  covariateData <-
    FeatureExtraction::loadCovariateData(file = system.file("testdata", "covariateData_cohortId_1.zip", package = "Kala"))

  table1Specs <- getTable1SpecificationsFromCovariateData(covariateData)

  reportTimeWindows <- getFeatureExtractionReportByTimeWindows(covariateData = covariateData, table1Specifications = table1Specs, cohortId = 1)

  stopifnot(length(reportTimeWindows) == 2)

  stopifnot(ncol(reportTimeWindows$raw) == 11)

  stopifnot(nrow(reportTimeWindows$raw) == 5148)
  stopifnot(nrow(reportTimeWindows$formatted) == 566)

  stopifnot(max(reportTimeWindows$formatted$conceptId, na.rm = T) == 46235214)

  expect_equal(reportTimeWindows$formatted$`d-9999d0`[2], "171 (9.3%)")
})

test_that("Get report (ByWindows, no format)", {
  covariateData <-
    FeatureExtraction::loadCovariateData(file = system.file("testdata", "covariateData_cohortId_1.zip", package = "Kala"))

  reportTimeWindowsNoFormat <- getFeatureExtractionReportByTimeWindows(
    covariateData = covariateData,
    cohortId = 1,
    format = FALSE
  )

  expect_false(is.null(reportTimeWindowsNoFormat))
  expect_equal(length(reportTimeWindowsNoFormat), 2)
  expect_true(nrow(reportTimeWindowsNoFormat$raw) > 0)
  expect_equal(nrow(reportTimeWindowsNoFormat$raw), nrow(reportTimeWindowsNoFormat$formatted))
  expect_equal(ncol(reportTimeWindowsNoFormat$raw), ncol(reportTimeWindowsNoFormat$formatted))
  expect_false("report" %in% colnames(reportTimeWindowsNoFormat$raw))

  reportTimeWindowsNoFormatPivot <- getFeatureExtractionReportByTimeWindows(
    covariateData = covariateData,
    cohortId = 1,
    format = FALSE,
    pivot = TRUE
  )
  expect_equal(nrow(reportTimeWindowsNoFormat$raw), nrow(reportTimeWindowsNoFormatPivot$formatted))
  expect_equal(ncol(reportTimeWindowsNoFormat$raw), ncol(reportTimeWindowsNoFormatPivot$formatted))
})
