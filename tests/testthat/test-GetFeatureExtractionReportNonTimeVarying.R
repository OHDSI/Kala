# Load required packages
library(testthat)
library(tibble)
library(Kala)

test_that("Get report (NonTimeVarying)", {
  
  
  reportNonTimeVarying <- getFeatureExtractionReportNonTimeVarying(covariateDataPath = system.file("testdata", "covariateData_cohortId_1.zip", package = "Kala"), 
                                                                   cohortId = 1)
  
  stopifnot(length(reportNonTimeVarying) == 3)
  
  stopifnot(ncol(reportNonTimeVarying$raw) == 13)
  
  stopifnot(nrow(reportNonTimeVarying$raw) == 5158)
  stopifnot(nrow(reportNonTimeVarying$formatted) == 564)
  
  stopifnot(max(reportNonTimeVarying$formatted$conceptId) == 46235214)
  
  expect_equal(reportNonTimeVarying$formatted$nonTimeVarying[1:10],c("3.0", "0.1", "1.0", "1.0", "1.0", "3.0", "0.1", "1.0", "1.0", "1.0"))
  
})