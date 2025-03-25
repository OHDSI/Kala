library(testthat)
library(Kala)

context("Tests for Default Time Covariates")

test_that("Default Temporal Covariates via Eunomia", {
  
  covariateCohortDefinitionSet <- dplyr::tibble(
    cohortId = c(1, 4),
    cohortName = c("Celecoxib", "NSAIDS")
  )
  
  expected <-
    list(temporal = TRUE, temporalSequence = FALSE, analysisId = 150, covariateCohortDatabaseSchema = "main",
         covariateCohortTable = "main.cohort", covariateCohorts = tibble(
           cohortId = c(1,4),
           cohortName = c("Celecoxib","NSAIDS")
         ), valueType = "binary",
         temporalStartDays = c(-9999,-9999,-9999,-4015,-3650,-3285,-2920,-2555,-2190,-1825,-1460,-1095,-730,-391,-391,-365,-365,-365,-364,-361,-361,-331,-331,-301,-301,-271,-271,-241,-241,-211,-211,-181,-181,-180,-151,-151,-121,-121,-120,-91,-91,-90,-61,-61,-60,-31,-31,-30,-30,-29,-7,-1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,31,31,61,91,121,151,181,211,241,271,301,331,366,731,1096,1461,1826,2191,2556,2921,3286),
         temporalEndDays = c(0,-1,9999,-3651,-3286,-2921,-2556,-2191,-1826,-1461,-1096,-731,-366,-362,-1,0,-31,-1,0,-332,-1,-302,-1,-272,-1,-242,-1,-212,-1,-182,-1,-152,-1,0,-122,-1,-92,-1,-91,-62,-1,-61,-32,-1,-31,-2,-1,0,-1,0,7,1,0,9999,365,30,365,1,31,61,91,121,151,181,211,241,271,301,331,361,9999,365,60,90,120,150,180,210,240,270,300,330,360,730,1095,1460,1825,2190,2555,2920,3285,3650),
         includedCovariateIds = c(1,4), warnOnAnalysisIdOverlap=TRUE)
  
  attr(expected, "fun") <- "getDbCohortBasedCovariatesData"
  attr(expected, "class") <- "covariateSettings"
  
  results <- getFeatureExtractionDefaultTemporalCohortCovariateSettings(
    covariateCohortDatabaseSchema = "main",
    covariateCohortTable = "main.cohort",
    covariateCohortDefinitionSet = covariateCohortDefinitionSet
  )
  
  expect_equal(results, expected)
  
})