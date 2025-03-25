# Load required packages
library(testthat)
library(tibble)
library(Kala)

test_that("Default feature extraction run", {
  temp_dir <- tempdir()

  covariateCohortDefinitionSet <- dplyr::tibble(
    cohortId = c(1),
    cohortName = c("Celecoxib")
  )

  defaultCovariates <- Kala::getFeatureExtractionDefaultTemporalCovariateSettings()

  executeFeatureExtraction(
    connectionDetails = eunomiaConnectionDetails,
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortIds = c(1),
    cohortTable = "cohort",
    outputFolder = temp_dir,
    covariateSettings = defaultCovariates,
    covariateCohortDefinitionSet = covariateCohortDefinitionSet
  )

  output_zip <- file.path(temp_dir, "covariateData_cohortId_1.zip")

  is_zip_valid <- tryCatch(
    {
      zip::zip_list(output_zip)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  stopifnot(is_zip_valid)

  zip_contents <- zip::zip_list(output_zip)
  unzip(zipfile = output_zip, exdir = temp_dir)

  conn <- dbConnect(RSQLite::SQLite(), paste0(tempdir(), "\\", zip_contents[2, ]$filename))

  coh1_c <- dbReadTable(conn, "covariates")
  coh1_cc <- dbReadTable(conn, "covariatesContinuous")

  stopifnot(nrow(coh1_c) == 31524)
  stopifnot(ncol(coh1_c) == 5)
  stopifnot(mean(coh1_c$averageValue) >= 0.0193576 & mean(coh1_c$averageValue) <= 0.0193577)
  stopifnot(mean(coh1_c$sumValue) >= 35.695 & mean(coh1_c$sumValue) <= 35.696)

  stopifnot(nrow(coh1_cc) == 2)
  stopifnot(ncol(coh1_cc) == 13)

  expect_equal(colnames(coh1_c), c("cohortDefinitionId", "covariateId", "timeId", "sumValue", "averageValue"))
})


test_that("Row_id == TRUE feature extraction run", {
  temp_dir <- tempdir()

  covariateCohortDefinitionSet <- dplyr::tibble(
    cohortId = c(1, 4),
    cohortName = c("Celecoxib", "NSAIDS")
  )

  defaultCovariates <- Kala::getFeatureExtractionDefaultTemporalCovariateSettings()

  executeFeatureExtraction(
    connectionDetails = eunomiaConnectionDetails,
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortIds = c(1, 4),
    cohortTable = "cohort",
    outputFolder = temp_dir,
    covariateSettings = defaultCovariates,
    covariateCohortDefinitionSet = covariateCohortDefinitionSet,
    rowIdField = "row_id"
  )

  output_zip <- file.path(temp_dir, "covariateData_cohortId_1.zip")

  is_zip_valid <- tryCatch(
    {
      zip::zip_list(output_zip)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  stopifnot(is_zip_valid)

  zip_contents <- zip::zip_list(output_zip)
  unzip(zipfile = output_zip, exdir = temp_dir)

  conn <- dbConnect(RSQLite::SQLite(), paste0(tempdir(), "\\", zip_contents[2, ]$filename))

  coh1_c <- dbReadTable(conn, "covariates")
  coh1_cc <- dbReadTable(conn, "covariatesContinuous")

  stopifnot(nrow(coh1_c) == 31524)
  stopifnot(ncol(coh1_c) == 5)
  stopifnot(mean(coh1_c$averageValue) >= 0.0193576 & mean(coh1_c$averageValue) <= 0.0193577)
  stopifnot(mean(coh1_c$sumValue) >= 35.695 & mean(coh1_c$sumValue) <= 35.696)

  stopifnot(nrow(coh1_cc) == 2)
  stopifnot(ncol(coh1_cc) == 13)

  expect_equal(colnames(coh1_c), c("cohortDefinitionId", "covariateId", "timeId", "sumValue", "averageValue"))
})
