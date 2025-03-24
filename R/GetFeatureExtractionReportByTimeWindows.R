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

#' Generate Feature Extraction Reports by Time Windows
#'
#' @description
#' This function generates detailed reports from feature extraction data, analyzing covariates
#' across different time windows. It processes both binary and continuous covariates, and can
#' handle time-varying and non-time-varying features. The function supports filtering by
#' covariate IDs, formatting according to table specifications, and pivoting results for
#' easier interpretation.
#'
#' @param covariateData A covariateData object containing feature extraction results with components:
#'   \itemize{
#'     \item analysisRef (analysisId, analysisName, domainId, isBinary, missingMeansZero)
#'     \item covariateRef (covariateId, covariateName, analysisId, conceptId, valueAsConceptId, collisions)
#'     \item covariates (cohortDefinitionId, covariateId, timeId, sumValue, averageValue)
#'     \item covariatesContinuous (cohortDefinitionId, covariateId, countValue, minValue, maxValue,
#'           averageValue, standardDeviation, medianValue, p10Value, p25Value, p75Value, p90Value, timeId)
#'     \item timeRef (timeId, startDay, endDay)
#'   }
#' @param startDays Vector of start days for time windows to include in the report. If NULL, all available
#'   time windows from covariateData$timeRef will be used.
#' @param endDays Vector of end days for time windows to include in the report. If NULL, all available
#'   time windows from covariateData$timeRef will be used.
#' @param includeNonTimeVarying Boolean indicating whether to include non-time-varying covariates.
#'   Default is FALSE.
#' @param minAverageValue Minimum average value threshold for including covariates. Default is 0.01.
#' @param includedCovariateIds Vector of covariate IDs to include. If NULL, all covariates will be included
#'   (subject to other filters).
#' @param excludedCovariateIds Vector of covariate IDs to exclude. If NULL, no covariates will be explicitly
#'   excluded.
#' @param table1Specifications Optional data frame with specifications for formatting as a Table 1, containing
#'   columns for label and covariateIds.
#' @param cohortId The cohort definition ID to generate the report for.
#' @param databaseId Optional database ID to include in the report header.
#' @param cohortName Optional cohort name to include in the report header.
#' @param reportName Optional report name to include in the report header.
#' @param format Boolean indicating whether to format the values in the report (e.g., as percentages).
#'   Default is TRUE.
#' @param distributionStatistic Character vector of statistics to include for continuous variables.
#'   Default includes "averageValue", "standardDeviation", "medianValue", "p25Value", "p75Value".
#' @param pivot Boolean indicating whether to pivot the report to have time periods as columns.
#'   Default is TRUE.
#'
#' @return A list with two components:
#'   \itemize{
#'     \item raw: The raw data frame with all covariate information before formatting
#'     \item formatted: The formatted report, either in long format or pivoted (if pivot = TRUE)
#'   }
#'
#' @details
#' The function processes both binary and continuous covariates from the provided covariateData object.
#' For binary covariates, it reports counts and percentages. For continuous covariates, it reports
#' the specified distribution statistics. The function can filter covariates based on minimum average
#' value and specific inclusion/exclusion lists.
#'
#' Time windows are specified using startDays and endDays parameters. If these are NULL, all time
#' windows in the covariateData will be used. Non-time-varying covariates can be included by setting
#' includeNonTimeVarying to TRUE.
#'
#' The table1Specifications parameter allows for organizing covariates into logical groups in the
#' report, similar to a Table 1 in clinical papers.
#'
#' @examples
#' \dontrun{
#' # Load covariate data
#' covariateData <- FeatureExtraction::loadCovariateData("path/to/covariateData")
#'
#' # Generate report for specific time windows
#' report <- getFeatureExtractionReportByTimeWindows(
#'   covariateData = covariateData,
#'   startDays = c(-365, -30, 0),
#'   endDays = c(-1, -1, 0),
#'   includeNonTimeVarying = TRUE,
#'   cohortId = 1
#' )
#'
#' # View the formatted report
#' View(report$formatted)
#' }
#'
#' @export
getFeatureExtractionReportByTimeWindows <- function(covariateData,
                                                    startDays = NULL,
                                                    endDays = NULL,
                                                    includeNonTimeVarying = FALSE,
                                                    minAverageValue = 0.01,
                                                    includedCovariateIds = NULL,
                                                    excludedCovariateIds = NULL,
                                                    table1Specifications = NULL,
                                                    cohortId,
                                                    databaseId = NULL,
                                                    cohortName = NULL,
                                                    reportName = NULL,
                                                    format = TRUE,
                                                    distributionStatistic = c(
                                                      "averageValue",
                                                      "standardDeviation",
                                                      "medianValue",
                                                      "p25Value",
                                                      "p75Value"
                                                    ),
                                                    pivot = TRUE) {
  # Check if table1Specifications is provided and process covariateIds
  if (!is.null(table1Specifications)) {
    if (nrow(table1Specifications) == 0) {
      stop("please check table1Specifications")
    }
    includedCovariateIdsFromTable1Specifications <-
      table1Specifications$covariateIds |>
      paste(collapse = ",") |>
      commaSeparatedStringToIntArray()

    if (!is.null(includedCovariateIds)) {
      includedCovariateIds <- intersect(
        includedCovariateIds,
        includedCovariateIdsFromTable1Specifications
      )
    } else {
      includedCovariateIds <- includedCovariateIdsFromTable1Specifications
    }
  }

  # Join covariateRef and analysisRef to get full covariate information
  covariateAnalysisId <- covariateData$covariateRef |>
    dplyr::select(
      .data$covariateId,
      .data$analysisId,
      .data$covariateName,
      .data$conceptId
    ) |>
    dplyr::inner_join(
      covariateData$analysisRef |>
        dplyr::select(.data$analysisId, .data$analysisName, .data$domainId),
      by = "analysisId"
    )

  # Filter by included/excluded covariateIds if specified
  if (!is.null(includedCovariateIds)) {
    covariateAnalysisId <- covariateAnalysisId |>
      dplyr::filter(.data$covariateId %in% includedCovariateIds)
  }

  if (!is.null(excludedCovariateIds)) {
    covariateAnalysisId <- covariateAnalysisId |>
      dplyr::filter(!.data$covariateId %in% excludedCovariateIds)
  }

  reportTimeVarying <- dplyr::tibble()
  reportNonTimeVarying <- dplyr::tibble()

  # If no specific time windows provided, use all from covariateData$timeRef
  if (all(
    is.null(startDays),
    is.null(endDays),
    "timeRef" %in% (names(covariateData))
  )) {
    startDays <-
      covariateData$timeRef |>
      dplyr::collect() |>
      dplyr::pull(.data$startDay)
    endDays <-
      covariateData$timeRef |>
      dplyr::collect() |>
      dplyr::pull(.data$endDay)
  }

  # Process time-varying covariates
  if (any(!is.null(startDays), !is.null(endDays))) {
    # Binary covariates
    reportTimeVarying1 <- covariateData$covariates |>
      dplyr::filter(.data$cohortDefinitionId == cohortId) |>
      dplyr::inner_join(
        covariateData$timeRef |>
          dplyr::filter(.data$startDay %in% startDays, .data$endDay %in% endDays) |>
          dplyr::mutate(
            periodName = paste0(
              "d",
              .data$startDay |> as.integer(),
              "d",
              .data$endDay |> as.integer()
            )
          ),
        by = "timeId"
      ) |>
      dplyr::inner_join(covariateAnalysisId, by = "covariateId") |>
      dplyr::arrange(
        .data$startDay,
        .data$endDay,
        dplyr::desc(.data$averageValue)
      ) |>
      dplyr::select(
        .data$covariateId,
        .data$covariateName,
        .data$conceptId,
        .data$analysisId,
        .data$analysisName,
        .data$domainId,
        .data$timeId,
        .data$periodName,
        .data$sumValue,
        .data$averageValue
      ) |>
      dplyr::filter(.data$averageValue > minAverageValue) |>
      dplyr::collect() |>
      dplyr::mutate(continuous = 0)

    # Continuous covariates if available
    if (!is.null(covariateData$covariatesContinuous)) {
      # Get statistics for continuous covariates
      reportTimeVarying2a <-
        covariateData$covariatesContinuous |>
        dplyr::filter(.data$cohortDefinitionId == cohortId) |>
        dplyr::inner_join(
          covariateData$timeRef |>
            dplyr::filter(.data$startDay %in% startDays, .data$endDay %in% endDays) |>
            dplyr::mutate(
              periodName = paste0(
                "d",
                .data$startDay |> as.integer(),
                "d",
                .data$endDay |> as.integer()
              )
            ),
          by = "timeId"
        ) |>
        dplyr::inner_join(covariateAnalysisId, by = "covariateId") |>
        dplyr::arrange(
          .data$startDay,
          .data$endDay,
          dplyr::desc(.data$averageValue)
        ) |>
        dplyr::select(
          "covariateId",
          "covariateName",
          "conceptId",
          "analysisId",
          "analysisName",
          "domainId",
          "timeId",
          "periodName",
          dplyr::all_of(distributionStatistic)
        ) |>
        dplyr::filter(.data$averageValue > minAverageValue) |>
        dplyr::collect() |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(distributionStatistic),
          names_to = "statistic",
          values_to = "averageValue"
        ) |>
        dplyr::mutate(
          statistic = stringr::str_replace(
            string = .data$statistic,
            pattern = "Value",
            replacement = ""
          )
        ) |>
        dplyr::mutate(covariateName = paste0(.data$covariateName, " (", .data$statistic, ")")) |>
        dplyr::select(-.data$statistic)

      # Get count values for continuous covariates
      reportTimeVarying2b <-
        covariateData$covariatesContinuous |>
        dplyr::filter(.data$cohortDefinitionId == cohortId) |>
        dplyr::inner_join(
          covariateData$timeRef |>
            dplyr::filter(.data$startDay %in% startDays, .data$endDay %in% endDays) |>
            dplyr::mutate(
              periodName = paste0(
                "d",
                .data$startDay |> as.integer(),
                "d",
                .data$endDay |> as.integer()
              )
            ),
          by = "timeId"
        ) |>
        dplyr::inner_join(covariateAnalysisId, by = "covariateId") |>
        dplyr::select(
          .data$covariateId,
          .data$timeId,
          .data$periodName,
          .data$countValue
        ) |>
        dplyr::rename(sumValue = .data$countValue) |>
        dplyr::collect()

      # Join statistics with counts for continuous covariates
      reportTimeVarying2 <- reportTimeVarying2a |>
        dplyr::inner_join(reportTimeVarying2b, by = c("covariateId", "periodName")) |>
        dplyr::mutate(continuous = 1)
    } else {
      reportTimeVarying2 <- dplyr::tibble()
    }

    # Combine binary and continuous time-varying covariates
    reportTimeVarying <- dplyr::tibble()
    if (nrow(reportTimeVarying1) > 0) {
      reportTimeVarying <- dplyr::bind_rows(reportTimeVarying, reportTimeVarying1)
    }
    if (nrow(reportTimeVarying2) > 0) {
      reportTimeVarying <- dplyr::bind_rows(reportTimeVarying, reportTimeVarying2)
    }

    # Format the report if needed
    if (all(nrow(reportTimeVarying) > 0, length(colnames(reportTimeVarying) > 0), format)) {
      reportTimeVarying <-
        dplyr::bind_rows(
          reportTimeVarying |>
            dplyr::filter(.data$continuous == 0) |>
            dplyr::mutate(
              report = formatCountPercent(
                count = .data$sumValue,
                percent = .data$averageValue
              )
            ),
          reportTimeVarying |>
            dplyr::filter(.data$continuous == 1) |>
            dplyr::mutate(
              report = formatDecimalWithComma(
                number = .data$averageValue,
                decimalPlaces = 1,
                round = 1
              )
            )
        ) |>
        dplyr::select(-.data$continuous)
    } else {
      reportTimeVarying <- dplyr::tibble()
    }
  }

  # Process non-time-varying covariates if requested
  if (includeNonTimeVarying) {
    # Check if timeId column exists and filter accordingly
    if ("timeId" %in% colnames(covariateData$covariates)) {
      covariateDataTemp <- covariateData$covariates |>
        dplyr::filter(.data$cohortDefinitionId == cohortId) |>
        dplyr::filter(is.na(.data$timeId))
    } else {
      covariateDataTemp <- covariateData$covariates |>
        dplyr::filter(.data$cohortDefinitionId == cohortId)
    }

    # Binary non-time-varying covariates
    reportNonTimeVarying1 <- covariateDataTemp |>
      dplyr::inner_join(covariateAnalysisId, by = "covariateId") |>
      dplyr::arrange(dplyr::desc(.data$averageValue)) |>
      dplyr::mutate(periodName = "nonTimeVarying") |>
      dplyr::select(
        .data$covariateId,
        .data$covariateName,
        .data$conceptId,
        .data$analysisId,
        .data$analysisName,
        .data$domainId,
        .data$timeId,
        .data$periodName,
        .data$sumValue,
        .data$averageValue
      ) |>
      dplyr::filter(.data$averageValue > minAverageValue) |>
      dplyr::collect() |>
      dplyr::mutate(continuous = 0)

    # Continuous non-time-varying covariates if available
    if (!is.null(covariateData$covariatesContinuous)) {
      # Check if timeId column exists and filter accordingly
      if ("timeId" %in% colnames(covariateData$covariatesContinuous)) {
        covariatesContinuousTemp <- covariateData$covariatesContinuous |>
          dplyr::filter(.data$cohortDefinitionId == cohortId) |>
          dplyr::filter(is.na(.data$timeId))
      } else {
        covariatesContinuousTemp <- covariateData$covariatesContinuous |>
          dplyr::filter(.data$cohortDefinitionId == cohortId)
      }

      # Get statistics for continuous non-time-varying covariates
      reportNonTimeVarying2a <-
        covariatesContinuousTemp |>
        dplyr::inner_join(covariateAnalysisId, by = "covariateId") |>
        dplyr::arrange(dplyr::desc(.data$averageValue)) |>
        dplyr::mutate(periodName = "nonTimeVarying") |>
        dplyr::select(
          "covariateId",
          "covariateName",
          "conceptId",
          "analysisId",
          "analysisName",
          "domainId",
          "timeId",
          "periodName",
          dplyr::all_of(distributionStatistic)
        ) |>
        dplyr::filter(.data$averageValue > minAverageValue) |>
        dplyr::collect() |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(distributionStatistic),
          names_to = "statistic",
          values_to = "averageValue"
        ) |>
        dplyr::mutate(
          statistic = stringr::str_replace(
            string = .data$statistic,
            pattern = "Value",
            replacement = ""
          )
        ) |>
        dplyr::mutate(covariateName = paste0(.data$covariateName, " (", .data$statistic, ")")) |>
        dplyr::select(-.data$statistic)

      # Get count values for continuous non-time-varying covariates
      reportNonTimeVarying2b <-
        covariatesContinuousTemp |>
        dplyr::inner_join(covariateAnalysisId, by = "covariateId") |>
        dplyr::arrange(dplyr::desc(.data$averageValue)) |>
        dplyr::mutate(periodName = "nonTimeVarying") |>
        dplyr::select(
          .data$covariateId,
          .data$timeId,
          .data$periodName,
          .data$countValue
        ) |>
        dplyr::rename(sumValue = .data$countValue) |>
        dplyr::collect()

      # Join statistics with counts for continuous non-time-varying covariates
      reportNonTimeVarying2 <- reportNonTimeVarying2a |>
        dplyr::inner_join(reportNonTimeVarying2b,
          by = c("covariateId", "periodName")
        ) |>
        dplyr::mutate(continuous = 1)
    } else {
      reportNonTimeVarying2 <- dplyr::tibble()
    }

    # Combine binary and continuous non-time-varying covariates
    reportNonTimeVarying <- dplyr::tibble()
    if (nrow(reportNonTimeVarying1) > 0) {
      reportNonTimeVarying <- dplyr::bind_rows(reportNonTimeVarying, reportNonTimeVarying1)
    }
    if (nrow(reportNonTimeVarying2) > 0) {
      reportNonTimeVarying <- dplyr::bind_rows(reportNonTimeVarying, reportNonTimeVarying2)
    }

    # Format the non-time-varying report if needed
    if (all(
      nrow(reportNonTimeVarying) > 0,
      length(colnames(reportNonTimeVarying) > 0),
      format
    )) {
      reportNonTimeVarying <-
        dplyr::bind_rows(
          reportNonTimeVarying |>
            dplyr::filter(.data$continuous == 0) |>
            dplyr::mutate(
              report = formatCountPercent(
                count = .data$sumValue,
                percent = .data$averageValue
              )
            ),
          reportNonTimeVarying |>
            dplyr::filter(.data$continuous == 1) |>
            dplyr::mutate(
              report = formatDecimalWithComma(
                number = .data$averageValue,
                decimalPlaces = 1,
                round = 1
              )
            )
        ) |>
        dplyr::select(-.data$continuous)
    } else {
      reportNonTimeVarying <- dplyr::tibble()
    }
  }

  # Combine time-varying and non-time-varying reports
  report <- dplyr::bind_rows(reportNonTimeVarying, reportTimeVarying)

  # Process conceptId for custom covariates
  if (all(nrow(report) > 0, "conceptId" %in% colnames(report))) {
    report <- report |>
      dplyr::mutate(conceptId = dplyr::if_else(
        condition = (.data$conceptId == 0),
        true = (.data$covariateId - .data$analysisId) / 1000,
        false = .data$conceptId
      ))
  }

  # Clean up
  rm("covariateAnalysisId")

  # Check if we have results
  if (nrow(report) == 0) {
    writeLines("No results")
    return()
  }

  # Store raw report before any table1 formatting
  rawReport <- report

  # Process table1 specifications if provided
  if (!is.null(table1Specifications)) {
    table1Specifications <- table1Specifications |>
      dplyr::mutate(labelId = dplyr::row_number()) |>
      dplyr::relocate(.data$labelId)
    reportTable1 <- c()
    for (i in (1:nrow(table1Specifications))) {
      reportTable1[[i]] <- table1Specifications[i, ] |>
        dplyr::select(.data$labelId, .data$label) |>
        tidyr::crossing(
          report |>
            dplyr::filter(
              .data$covariateId %in% commaSeparatedStringToIntArray(table1Specifications[i, ]$covariateIds)
            )
        )

      if (nrow(reportTable1[[i]]) > 0) {
        reportTable1[[i]] <- dplyr::bind_rows(
          reportTable1[[i]] |>
            dplyr::mutate(source = 2),
          table1Specifications[i, ] |>
            dplyr::mutate(
              covariateId = 0,
              periodName = "",
              covariateName = .data$label
            ) |>
            dplyr::select(
              .data$labelId,
              .data$label,
              .data$covariateId,
              .data$timeId,
              .data$periodName,
              .data$covariateName
            ) |>
            dplyr::mutate(source = 1)
        ) |>
          dplyr::arrange(.data$labelId, .data$label, .data$source) |>
          dplyr::select(-.data$source)
      }
    }
    report <- dplyr::bind_rows(reportTable1)

    idCols <- c(
      "labelId",
      "label",
      "covariateId",
      "covariateName",
      "conceptId",
      "analysisId",
      "analysisName",
      "domainId"
    )
  } else {
    idCols <- c(
      "covariateId",
      "covariateName",
      "conceptId",
      "analysisId",
      "analysisName",
      "domainId"
    )
  }

  # Pivot the report if requested
  if (pivot) {
    report <- report |>
      tidyr::pivot_wider(
        id_cols = .data$idCols,
        names_from = .data$periodName,
        values_from = .data$report
      )
  }

  # Add cohort and database information if provided
  if (!is.null(cohortName)) {
    report <-
      dplyr::bind_rows(
        report |>
          dplyr::slice(0),
        dplyr::tibble(covariateName = cohortName),
        report
      )
  }

  if (!is.null(databaseId)) {
    report <- dplyr::bind_rows(
      report |>
        dplyr::slice(0),
      dplyr::tibble(covariateName = databaseId),
      report
    )
  }

  if (!is.null(reportName)) {
    report <-
      dplyr::bind_rows(
        report |>
          dplyr::slice(0),
        dplyr::tibble(covariateName = reportName),
        report
      )
  }

  # Prepare and return output
  output <- list()
  output$raw <- rawReport
  output$formatted <- report
  return(output)
}
