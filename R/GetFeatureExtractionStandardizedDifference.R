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




#' Compute Standardized Difference Between Two Covariate Data Sets
#'
#' This function computes the standardized difference between covariate data for two cohorts by loading
#' covariate data from provided file paths and comparing them over defined time windows. The function
#' iterates over each time window to calculate the standardized difference using
#' \code{FeatureExtraction::computeStandardizedDifference}. Optionally, it also computes the standardized
#' difference for non-time-varying covariates when \code{includeNonTimeVarying} is set to \code{TRUE}.
#'
#' @param covariateData1Path A character string specifying the file path to the first covariate data set.
#' @param covariateData2Path A character string specifying the file path to the second covariate data set.
#' @param cohortId1 A numeric or character identifier for the first cohort (used to filter covariate data).
#' @param cohortId2 A numeric or character identifier for the second cohort (used to filter covariate data).
#' @param includeNonTimeVarying A logical value indicating whether to include non-time-varying covariates in the calculation.
#'   Defaults to \code{TRUE}.
#' @param timeRef An optional data frame that defines the time windows, containing at least the columns
#'   \code{startDay} and \code{endDay}. If \code{NULL}, the time reference will be derived from the covariate data.
#'
#' @return A tibble containing the computed standardized differences. For each time window, the output includes
#'   information such as \code{startDay}, \code{endDay}, \code{covariateId}, \code{covariateName}, and the calculated
#'   standardized difference. If non-time-varying covariates are included, their standardized difference is appended
#'   to the result.
#'
#'
#' @examples
#' \dontrun{
#' # Compute standardized differences between two covariate data sets for cohorts 1 and 2
#' stdDiff <- getFeatureExtractionStandardizedDifference(
#'   covariateData1Path = "path/to/covariateData1.rds",
#'   covariateData2Path = "path/to/covariateData2.rds",
#'   cohortId1 = 1,
#'   cohortId2 = 2,
#'   includeNonTimeVarying = TRUE
#' )
#' print(stdDiff)
#' }
#'
getFeatureExtractionStandardizedDifference <-
  function(covariateData1Path = NULL,
           covariateData2Path = NULL,
           cohortId1,
           cohortId2,
           includeNonTimeVarying = TRUE,
           timeRef = NULL) {
    if (all(is.null(timeRef), !includeNonTimeVarying)) {
      message("includeNonTimeVarying is FALSE and timeRef is NULL. no results.")
      return(NULL)
    }

    if (all(is.null(covariateData1Path), is.null(covariateData2Path))) {
      stop("covariateData1/2 and path are all NULL")
    }

    covariateData1 <- FeatureExtraction::loadCovariateData(file = covariateData1Path)
    covariateData2 <- FeatureExtraction::loadCovariateData(file = covariateData2Path)

    timeRef1 <- covariateData1$timeRef |> dplyr::collect()
    timeRef2 <- covariateData2$timeRef |> dplyr::collect()

    compared <- compareTibbles(tibble1 = timeRef1, tibble2 = timeRef2)

    if (!compared$identical) {
      message("covariate data is not identical")
      timeRefFromCovariateData <- timeRef1 |>
        dplyr::inner_join(timeRef2, by = c("timeId", "startDay", "endDay"))
    } else {
      timeRefFromCovariateData <- timeRef1
    }

    standardizedDifference <- c()

    if (!is.null(timeRef)) {
      checkmate::assertDataFrame(timeRef)
      if (!"startDay" %in% colnames(timeRef)) {
        stop("please check timeRef")
      }
      if (!"endDay" %in% colnames(timeRef)) {
        stop("please check timeRef")
      }
      timeRef <- timeRefFromCovariateData |>
        dplyr::inner_join(
          timeRef |>
            dplyr::select(.data$startDay, .data$endDay) |>
            dplyr::distinct(),
          by = c("startDay", "endDay")
        )
    } else {
      timeRef <- timeRefFromCovariateData
    }

    if (nrow(timeRef) == 0) {
      message("no valid time windows")
    }

    for (i in (1:nrow(timeRef))) {
      rowData <- timeRef[i, ]

      message(paste0("working on ", rowData$startDay, " to ", rowData$endDay))

      covariateData1 <- FeatureExtraction::loadCovariateData(file = covariateData1Path)
      covariateData1$covariates <- covariateData1$covariates |>
        dplyr::filter(
          .data$timeId == rowData$timeId,
          .data$cohortDefinitionId == cohortId1
        )

      covariateData2 <- FeatureExtraction::loadCovariateData(file = covariateData2Path)
      covariateData2$covariates <- covariateData2$covariates |>
        dplyr::filter(
          .data$timeId == rowData$timeId,
          .data$cohortDefinitionId == cohortId2
        )

      standardizedDifference[[i]] <- FeatureExtraction::computeStandardizedDifference(
        covariateData1 = covariateData1,
        covariateData2 = covariateData2,
        cohortId1 = cohortId1,
        cohortId2 = cohortId2
      ) |>
        tidyr::crossing(rowData |>
          dplyr::select(.data$startDay, .data$endDay)) |>
        dplyr::relocate(
          .data$startDay,
          .data$endDay,
          .data$covariateId,
          .data$covariateName
        )
    }

    standardizedDifference <- dplyr::bind_rows(standardizedDifference)

    if (includeNonTimeVarying) {
      message("working on non time varying")
      covariateData1 <- FeatureExtraction::loadCovariateData(file = covariateData1Path)
      covariateData1$covariates <- covariateData1$covariates |>
        dplyr::filter(
          is.na(.data$timeId),
          .data$cohortDefinitionId == cohortId1
        )

      covariateData2 <- FeatureExtraction::loadCovariateData(file = covariateData2Path)
      covariateData2$covariates <- covariateData2$covariates |>
        dplyr::filter(
          is.na(.data$timeId),
          .data$cohortDefinitionId == cohortId2
        )

      standardizedDifferenceNonTimeVarying <- FeatureExtraction::computeStandardizedDifference(
        covariateData1 = covariateData1,
        covariateData2 = covariateData2,
        cohortId1 = cohortId1,
        cohortId2 = cohortId2
      )

      standardizedDifference <- dplyr::bind_rows(
        standardizedDifference,
        standardizedDifferenceNonTimeVarying
      )
    }

    return(standardizedDifference)
  }
