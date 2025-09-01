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


#' Get Covariate Settings Time Windows
#'
#' This function extracts time window information from a covariate settings object. It creates a tibble
#' using the \code{temporalStartDays} and \code{temporalEndDays} provided in the covariate settings and performs
#' a left join with the default time windows from \code{getFeatureExtractionDefaultTimeWindows()}.
#'
#' @param covariateSettings An object (e.g., a list) containing the covariate settings. It must include the elements
#'   \code{temporalStartDays} and \code{temporalEndDays} that define the start and end days for the time windows.
#'
#' @return A tibble with the time window definitions. The tibble contains columns \code{startDay} and \code{endDay},
#'   along with any additional columns from the default time windows as provided by
#'   \code{getFeatureExtractionDefaultTimeWindows()}.
#'
#' @details
#' The function works by first constructing a tibble from the \code{temporalStartDays} and
#' \code{temporalEndDays} in the provided covariate settings. It then merges this tibble with the default
#' time window definitions using a left join on the \code{startDay} and \code{endDay} columns.
#'
#' @examples
#' \dontrun{
#' # Example covariate settings list with temporal start and end days
#' covariateSettings <- list(
#'   temporalStartDays = c(-365, -30, 1),
#'   temporalEndDays = c(-1, 0, 30)
#' )
#'
#' # Retrieve the time windows based on the covariate settings
#' timeWindows <- getCovariateSettingsTimeWindows(covariateSettings)
#' print(timeWindows)
#' }
#'
getCovariateSettingsTimeWindows <- function(covariateSettings) {
  timeWindows <-
    dplyr::tibble(
      startDay = covariateSettings$temporalStartDays,
      endDay = covariateSettings$temporalEndDays
    ) |>
    dplyr::left_join(getFeatureExtractionDefaultTimeWindows(),
      by = c("startDay", "endDay")
    )
  return(timeWindows)
}
