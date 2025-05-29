getCovariateDataToPlot <- function(){
  cohortDefinitionSet <- dplyr::tibble(
    cohortId = c(1080, 10801),
    cohortName = c("cohort1", "cohort2")
  )
  covariateResultsAggregated <-
    FeatureExtraction::loadCovariateData(file = system.file("testdata", "covariateData_cohortId_1001.zip", package = "Kala"))
  
  timeWindowsFeatureExtractionReport <- Kala::getFeatureExtractionReportByTimeWindows(
    covariateData = covariateResultsAggregated,
    includedCovariateIds = paste0(as.character(cohortDefinitionSet$cohortId), "150"),
    cohortId = 1001
  )
}

test_that("rawTemporalCovariatesBarPlot works with default parameters", {
  timeWindowsFeatureExtractionReport <- getCovariateDataToPlot()
  plot <- rawTemporalCovariatesBarPlot(timeWindowsFeatureExtractionReport)
  expect_s3_class(plot, "ggplot")
})

test_that("rawTemporalCovariatesBarPlot works with default parameters when provided `raw` data frame", {
  timeWindowsFeatureExtractionReport <- getCovariateDataToPlot()
  plot <- rawTemporalCovariatesBarPlot(timeWindowsFeatureExtractionReport$raw)
  expect_s3_class(plot, "ggplot")
})

test_that("rawTemporalCovariatesBarPlot test custom parameters", {
  timeWindowsFeatureExtractionReport <- getCovariateDataToPlot()
  plot <- rawTemporalCovariatesBarPlot(
    timeWindowsFeatureExtractionReport,
    covariateValueToPlot = "sumValue",
    barLabel = "report",
    barPosition = "dodge"
  )
  expect_s3_class(plot, "ggplot")
})

test_that("rawTemporalCovariatesBarPlot error on arbitrary object", {
  expect_error(rawTemporalCovariatesBarPlot(c(1,2,3)),
               "Unrecognized `covariateData` object format.")
})

test_that("rawTemporalCovariatesBarPlot error on empty data", {
  timeWindowsFeatureExtractionReport <- data.frame(
    covariateId = numeric(0),
    periodName = character(0),
    timeId = numeric(0),
    covariateName = character(0),
    sumValue = numeric(0),
    report = character(0)
  )
  expect_error(rawTemporalCovariatesBarPlot(timeWindowsFeatureExtractionReport),
               "No data to plot. Check your `covariateIds` or `covariateData` input.")
})

test_that("rawTemporalCovariatesBarPlot error on missing columns", {
  timeWindowsFeatureExtractionReport <- data.frame(
    # covariateId = numeric(0),
    periodName = character(0),
    # timeId = numeric(0),
    covariateName = character(0),
    sumValue = numeric(0),
    report = character(0)
  )
  expect_error(rawTemporalCovariatesBarPlot(timeWindowsFeatureExtractionReport),
               "Missing required columns: ")
})
