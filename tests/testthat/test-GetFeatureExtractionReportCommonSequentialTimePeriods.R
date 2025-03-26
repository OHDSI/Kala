# Load required packages
library(testthat)
library(tibble)
library(Kala)

test_that("Get report (ByWindows, With Table Data)", {
  reportCommonPeriods <- getFeatureExtractionReportCommonSequentialTimePeriods()

  expected <- tibble(
    timeId = c(15, 21, 23, 25, 27, 29, 31, 33, 36, 38, 41, 44, 47, 53, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70),
    startDay = c(-391, -361, -331, -301, -271, -241, -211, -181, -151, -121, -91, -61, -31, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    endDay = c(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 31, 61, 91, 121, 151, 181, 211, 241, 271, 301, 331, 361)
  )

  expect_equal(reportCommonPeriods, expected)
})
