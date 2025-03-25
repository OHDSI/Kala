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
  
  expect_equal(reportTimeWindows$formatted$`d-9999d0`[2],"171 (9.3%)")
    
})