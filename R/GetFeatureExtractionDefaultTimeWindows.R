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


#' Extract default time windows for feature extraction
#'
#' This function reads a CSV file containing time windows for feature extraction and filters
#' the time windows based on the cumulative and period type parameters.
#'
#' @param cummulative A logical value indicating whether cumulative time windows should be returned.
#'                    Can be TRUE, FALSE, or NULL (default), where NULL returns all records.
#' @param periodTypes A character vector specifying the types of periods to filter by.
#'                    Can be "month", "year", or NULL (default), where NULL returns all records.
#' @return A data frame containing the filtered time windows.
#' @export
getFeatureExtractionDefaultTimeWindows <-
  function(cummulative = NULL,
           periodTypes = NULL,
           selectedCummulative = NULL) {
    # Path to the CSV file within the OhdsiHelpers package
    filePath <-
      system.file("FeatureExtractionTimeWindows.csv", package = "OhdsiHelpers")

    # Reading the CSV file into a data frame
    timeWindows <-
      readr::read_csv(file = filePath, col_types = readr::cols())

    # Assert checks for `cumulative` to be TRUE, FALSE, or NULL
    checkmate::assert_flag(cumulative, null.ok = TRUE)

    # Filter for cumulative time windows if `cummulative` is not NULL
    if (!is.null(cummulative)) {
      timeWindows <-
        timeWindows[timeWindows$sequenceCummulative == cummulative, ]
    }

    # Assert checks for `periodTypes` to be either "month", "year", or NULL
    validPeriods <- c("month", "year")
    if (!is.null(periodTypes)) {
      checkmate::assert_subset(periodTypes, validPeriods)
      timeWindows <-
        timeWindows[timeWindows$period %in% periodTypes, ]
    }

    timeWindows <- timeWindows |>
      dplyr::mutate(periodName = paste0(
        "d",
        .data$startDay |> as.integer(),
        "d",
        .data$endDay |> as.integer()
      ))

    if (isTRUE(selectedCummulative)) {
      filteredDataFrame <- timeWindows |>
        dplyr::select(.data$startDay, .data$endDay) |>
        dplyr::filter(
          .data$startDay %in% c(-391, -301, -181, -91, -31) |
            .data$endDay %in% c(0, 31, 91, 181, 241, 361) |
            (.data$startDay == 0 & .data$endDay == 0)
        ) |>
        dplyr::mutate(selectedCummulative = 1) |>
        dplyr::distinct()

      timeWindows <- timeWindows |>
        dplyr::inner_join(filteredDataFrame, by = c("startDay", "endDay"))
    }

    timeWindows <- timeWindows |>
      dplyr::select(
        .data$startDay,
        .data$endDay,
        .data$periodName,
        .data$windowType
      ) |>
      dplyr::distinct()


    return(timeWindows)
  }
