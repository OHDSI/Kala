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


#' Get Feature Extraction Default Temporal Covariate Settings
#'
#' This function creates a set of default temporal covariate settings for feature extraction by configuring
#' various covariate flags and specifying temporal windows. It uses the provided time windows (or the default
#' from \code{getFeatureExtractionDefaultTimeWindows()}) to construct a tibble of distinct temporal intervals,
#' which is then passed along with the specified covariate settings to \code{FeatureExtraction::createTemporalCovariateSettings}.
#'
#' @param timeWindows A list or tibble containing the temporal window definitions, with elements \code{startDay} and \code{endDay}.
#'   Defaults to the output of \code{getFeatureExtractionDefaultTimeWindows()}.
#' @param useConditionOccurrence Logical indicating whether to include condition occurrence covariates. Defaults to \code{TRUE}.
#' @param useProcedureOccurrence Logical indicating whether to include procedure occurrence covariates. Defaults to \code{TRUE}.
#' @param useDrugEraStart Logical indicating whether to include drug era start covariates. Defaults to \code{TRUE}.
#' @param useMeasurement Logical indicating whether to include measurement covariates. Defaults to \code{TRUE}.
#' @param useConditionEraStart Logical indicating whether to include condition era start covariates. Defaults to \code{TRUE}.
#' @param useConditionEraOverlap Logical indicating whether to include condition era overlap covariates. Defaults to \code{TRUE}.
#' @param useVisitCount Logical indicating whether to include visit count covariates. Defaults to \code{TRUE}.
#' @param useVisitConceptCount Logical indicating whether to include visit concept count covariates. Defaults to \code{TRUE}.
#' @param useConditionEraGroupStart Logical indicating whether to include condition era group start covariates. Defaults to \code{TRUE}.
#' @param useConditionEraGroupOverlap Logical indicating whether to include condition era group overlap covariates. Defaults to \code{TRUE}.
#' @param useDrugExposure Logical indicating whether to include drug exposure covariates. Defaults to \code{FALSE} due to potential overabundance of concept IDs.
#' @param useDrugEraOverlap Logical indicating whether to include drug era overlap covariates. Defaults to \code{TRUE}.
#' @param useDrugEraGroupStart Logical indicating whether to include drug era group start covariates. Defaults to \code{TRUE}.
#' @param useDrugEraGroupOverlap Logical indicating whether to include drug era group overlap covariates. Defaults to \code{TRUE}.
#' @param useObservation Logical indicating whether to include observation covariates. Defaults to \code{TRUE}.
#' @param useDeviceExposure Logical indicating whether to include device exposure covariates. Defaults to \code{TRUE}.
#'
#' @return An object containing the temporal covariate settings, as created by \code{FeatureExtraction::createTemporalCovariateSettings}.
#'
#' @details
#' This function first constructs a tibble, \code{feTemporalDays}, by extracting the \code{startDay} and \code{endDay}
#' from the provided \code{timeWindows} and ensuring that the intervals are distinct and ordered. These temporal windows
#' are then used to set the \code{temporalStartDays} and \code{temporalEndDays} parameters when calling
#' \code{FeatureExtraction::createTemporalCovariateSettings} along with other covariate flags.
#'
#' @examples
#' \dontrun{
#' # Retrieve default temporal covariate settings for feature extraction
#' temporalSettings <- getFeatureExtractionDefaultTemporalCovariateSettings()
#' print(temporalSettings)
#' }
#'
getFeatureExtractionDefaultTemporalCovariateSettings <-
  function(timeWindows = getFeatureExtractionDefaultTimeWindows(),
           useConditionOccurrence = TRUE,
           useProcedureOccurrence = TRUE,
           useDrugEraStart = TRUE,
           useMeasurement = TRUE,
           useConditionEraStart = TRUE,
           useConditionEraOverlap = TRUE,
           useVisitCount = TRUE,
           useVisitConceptCount = TRUE,
           useConditionEraGroupStart = TRUE,
           useConditionEraGroupOverlap = TRUE,
           useDrugExposure = FALSE,
           # leads to too many concept id
           useDrugEraOverlap = TRUE,
           useDrugEraGroupStart = TRUE,
           useDrugEraGroupOverlap = TRUE,
           useObservation = TRUE,
           useDeviceExposure = TRUE) {
    feTemporalDays <- dplyr::tibble(
      startDay = timeWindows$startDay,
      endDay = timeWindows$endDay
    ) |>
      dplyr::tibble() |>
      dplyr::distinct() |>
      dplyr::arrange(.data$startDay)

    featureExtractionSettings <-
      FeatureExtraction::createTemporalCovariateSettings(
        useConditionOccurrence = useConditionOccurrence,
        useProcedureOccurrence = useProcedureOccurrence,
        useDrugEraStart = useDrugEraStart,
        useMeasurement = useMeasurement,
        useConditionEraStart = useConditionEraStart,
        useConditionEraOverlap = useConditionEraOverlap,
        useVisitCount = useVisitCount,
        useVisitConceptCount = useVisitConceptCount,
        useConditionEraGroupStart = useConditionEraGroupStart,
        useConditionEraGroupOverlap = useConditionEraGroupOverlap,
        useDrugExposure = useDrugExposure,
        # leads to too many concept id
        useDrugEraOverlap = useDrugEraOverlap,
        useDrugEraGroupStart = useDrugEraGroupStart,
        useDrugEraGroupOverlap = useDrugEraGroupOverlap,
        useObservation = useObservation,
        useDeviceExposure = useDeviceExposure,
        temporalStartDays = feTemporalDays$startDay,
        temporalEndDays = feTemporalDays$endDay
      )

    return(featureExtractionSettings)
  }
