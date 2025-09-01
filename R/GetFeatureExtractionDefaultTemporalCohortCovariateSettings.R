# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of Kala
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Get Default Temporal Cohort Covariate Settings for Feature Extraction
#'
#' This function creates default temporal covariate settings for cohort-based feature extraction.
#' It constructs temporal windows from the provided time window definitions and sets up the
#' necessary parameters to extract covariate data for cohorts. If no specific covariate IDs are provided,
#' it uses the cohort IDs from the provided cohort definition set.
#'
#' @param timeWindows A tibble or list containing time window definitions with at least \code{startDay} and \code{endDay}
#'   elements. Defaults to the output of \code{getFeatureExtractionDefaultTimeWindows()}.
#' @param analysisId A numeric identifier for the analysis. Defaults to \code{150}.
#' @param covariateCohortDatabaseSchema A character string specifying the database schema where the covariate cohorts reside.
#' @param covariateCohortTable A character string specifying the table name containing the covariate cohorts.
#' @param covariateCohortDefinitionSet A data frame or tibble that includes the cohort definitions, with at least the columns
#'   \code{cohortId} and \code{cohortName}.
#' @param includedCovariateIds An optional vector of covariate IDs to include. If \code{NULL}, the function defaults
#'   to using all \code{cohortId} values from \code{covariateCohortDefinitionSet}.
#' @param valueType A character string indicating the type of value to be used in the covariate settings (e.g., "binary").
#'   Defaults to \code{"binary"}.
#'
#' @return An object representing the cohort-based temporal covariate settings, as created by
#'   \code{FeatureExtraction::createCohortBasedTemporalCovariateSettings}.
#'
#' @details
#' The function first constructs a tibble of distinct temporal windows by extracting \code{startDay} and \code{endDay}
#' from the provided \code{timeWindows}. If \code{includedCovariateIds} is not specified, it is set to the
#' \code{cohortId} values from the \code{covariateCohortDefinitionSet}. These parameters are then passed to the
#' \code{FeatureExtraction::createCohortBasedTemporalCovariateSettings} function, which sets up the covariate extraction
#' process based on the specified temporal windows and cohort information.
#'
#' @examples
#' \dontrun{
#' # Assume default time windows and a cohort definition set are available
#' covariateCohortDefinitionSet <- dplyr::tibble(
#'   cohortId = c(1, 2, 3),
#'   cohortName = c("Cohort A", "Cohort B", "Cohort C")
#' )
#'
#' settings <- getFeatureExtractionDefaultTemporalCohortCovariateSettings(
#'   covariateCohortDatabaseSchema = "my_schema",
#'   covariateCohortTable = "my_cohort_table",
#'   covariateCohortDefinitionSet = covariateCohortDefinitionSet
#' )
#' print(settings)
#' }
#'
#' @export
getFeatureExtractionDefaultTemporalCohortCovariateSettings <-
  function(timeWindows = getFeatureExtractionDefaultTimeWindows(),
           analysisId = 150,
           covariateCohortDatabaseSchema,
           covariateCohortTable,
           covariateCohortDefinitionSet,
           includedCovariateIds = NULL,
           valueType = "binary") {
    feTemporalDays <- dplyr::tibble(
      startDay = timeWindows$startDay,
      endDay = timeWindows$endDay
    ) |>
      dplyr::tibble() |>
      dplyr::distinct() |>
      dplyr::arrange(.data$startDay)

    if (is.null(includedCovariateIds)) {
      includedCovariateIds <- covariateCohortDefinitionSet$cohortId
    }

    featureExtractionSettings <-
      FeatureExtraction::createCohortBasedTemporalCovariateSettings(
        analysisId = analysisId,
        covariateCohortDatabaseSchema = covariateCohortDatabaseSchema,
        covariateCohortTable = covariateCohortTable,
        covariateCohorts = covariateCohortDefinitionSet |>
          dplyr::select(.data$cohortId, .data$cohortName),
        valueType = valueType,
        temporalStartDays = feTemporalDays$startDay,
        temporalEndDays = feTemporalDays$endDay,
        includedCovariateIds = includedCovariateIds
      )

    return(featureExtractionSettings)
  }
