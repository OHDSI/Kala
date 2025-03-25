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
#' the time windows based on the `cumulative`, `periodTypes`, and `selectedcumulative` parameters.
#' The `cumulative` parameter filters the time windows by matching the `sequencecumulative` column,
#' while `periodTypes` allows filtering by period types (e.g., "month" or "year"). Additionally, if
#' `selectedcumulative` is TRUE, the function further restricts the results to a predefined subset
#' of cumulative time windows.
#'
#' @param cumulative A logical value indicating whether cumulative time windows should be returned.
#'                    TRUE returns only cumulative time windows, FALSE returns non-cumulative windows,
#'                    and NULL (default) returns all records.
#' @param periodTypes A character vector specifying the types of periods to filter by.
#'                    Valid values are "month" or "year". If NULL (default), all period types are returned.
#' @param selectedcumulative A logical flag that, if TRUE, filters the results to include only
#'                            a selected subset of cumulative time windows based on specific startDay
#'                            and endDay criteria.
#'
#' @return A data frame containing the filtered time windows with columns: startDay, endDay, periodName,
#'         and windowType.

#' @export
getFeatureExtractionDefaultTimeWindows <-
  function(cumulative = NULL,
           periodTypes = NULL,
           selectedcumulative = NULL) {
    filePath <-
      system.file("FeatureExtractionTimeWindows.csv",
                  package = utils::packageName()
      )
    
    # Reading the CSV file into a data frame
    timeWindows <-
      readr::read_csv(file = filePath, col_types = readr::cols())
    
    # Assert checks for `cumulative` to be TRUE, FALSE, or NULL
    checkmate::assert_flag(cumulative, null.ok = TRUE)
    
    # Filter for cumulative time windows if `cumulative` is not NULL
    if (!is.null(cumulative)) {
      timeWindows <-
        timeWindows[timeWindows$sequenceCumulative == cumulative, ]
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
    
    if (isTRUE(selectedcumulative)) {
      filteredDataFrame <- timeWindows |>
        dplyr::select(.data$startDay, .data$endDay) |>
        dplyr::filter(
          .data$startDay %in% c(-391, -301, -181, -91, -31) |
            .data$endDay %in% c(0, 31, 91, 181, 241, 361) |
            (.data$startDay == 0 & .data$endDay == 0)
        ) |>
        dplyr::mutate(selectedcumulative = 1) |>
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
