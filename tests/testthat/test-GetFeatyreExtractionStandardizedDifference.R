# Load required packages
library(testthat)
library(tibble)
library(Kala)

test_that("Get report (ByWindows, With Table Data)", {
  
  stdDiff <- getFeatureExtractionStandardizedDifference(covariateData1Path = system.file("testdata", "covariateData_cohortId_1.zip", package = "Kala"),
                                                        covariateData2Path = system.file("testdata", "covariateData_cohortId_4.zip", package = "Kala"), 
                                                        cohortId1 = 1, cohortId2 = 4)
  
  
  stopifnot(nrow(stdDiff) == 34779)
  stopifnot(ncol(stdDiff) == 10)
  
  stopifnot(stdDiff$stdDiff[1] <= -1.91784 & stdDiff$stdDiff[1] >= -1.91785)
  
  stopifnot(mean(stdDiff$stdDiff, na.rm = T) >= -0.0108157 & mean(stdDiff$stdDiff, na.rm = T) <= -0.0108156)

  expect_equal(length(stdDiff[stdDiff$stdDiff=="NaN",]$stdDiff),92)
    
})