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


#' Get Common Sequential Time Periods for Feature Extraction Reports
#'
#' This function constructs and returns a tibble of common sequential time periods used in feature extraction reports.
#' The returned tibble defines prior monthly periods, post monthly periods, and a specific day period ("on day of"),
#' each characterized by a unique time identifier, a start day, and an end day.
#'
#' @return A tibble with the following columns:
#'   \item{timeId}{A numeric identifier for the time period.}
#'   \item{startDay}{The start day of the time period relative to a reference date.}
#'   \item{endDay}{The end day of the time period relative to a reference date.}
#'
#' @details
#' These periods are combined and arranged in ascending order based on the \code{timeId} column.
#'
#' @examples
#' \dontrun{
#' # Retrieve common sequential time periods for feature extraction reports
#' timePeriods <- getFeatureExtractionReportCommonSequentialTimePeriods()
#' print(timePeriods)
#' }
#'
getFeatureExtractionReportCommonSequentialTimePeriods <-
  function() {
    # Define prior monthly periods with their time IDs, start days, and end days
    priorMonthlyPeriods <- dplyr::tibble(
      timeId = c(15, 21, 23, 25, 27, 29, 31, 33, 36, 38, 41, 44, 47),
      startDay = c(
        -391,
        -361,
        -331,
        -301,
        -271,
        -241,
        -211,
        -181,
        -151,
        -121,
        -91,
        -61,
        -31
      ),
      endDay = c(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
    )

    # Define post monthly periods with their time IDs, start days, and end days
    postMonthlyPeriods <- dplyr::tibble(
      timeId = c(58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70),
      startDay = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      endDay = c(1, 31, 61, 91, 121, 151, 181, 211, 241, 271, 301, 331, 361)
    )

    # Define a specific time period representing "on day of" (a single day period)
    onDayOf <- dplyr::tibble(
      timeId = 53,
      startDay = 0,
      endDay = 0
    )

    # Combine all time periods and arrange by timeId
    timePeriods <- dplyr::bind_rows(priorMonthlyPeriods, postMonthlyPeriods, onDayOf) |>
      dplyr::arrange(.data$timeId)

    return(timePeriods)
  }
