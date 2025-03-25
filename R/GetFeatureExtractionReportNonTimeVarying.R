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


#' Get Non-Time-Varying Feature Extraction Report
#'
#' This function generates a non-time-varying feature extraction report by calling
#' \code{getFeatureExtractionReportInParallel} with predefined settings and then filtering
#' the formatted output to remove specific covariates based on a regular expression pattern.
#'
#' @param covariateDataPath A character string specifying the file path to the covariate data.
#' @param cohortId A numeric or character identifier for the cohort. This ID is used both to filter covariate data and
#'   as part of the file name pattern for retrieving covariate data.
#' @param cohortDefinitionSet A data frame or tibble containing the cohort definition set.
#' @param remove A character string containing a regular expression pattern used to filter out certain covariates
#'   from the formatted report. The default pattern removes labels related to visit counts, certain risk scores,
#'   and demographic time metrics. Defaults to \code{'Visit Count|Chads 2 Vasc|Demographics Index Month|Demographics Post Observation Time|Visit Concept Count|Chads 2|Demographics Prior Observation Time|Dcsi|Demographics Time In Cohort|Demographics Index Year Month'}.
#'
#' @return A list containing the feature extraction report, including both the full formatted output (\code{formattedFull})
#'   and a filtered version (\code{formatted}) where rows matching the \code{remove} pattern in the \code{label} column have been excluded.
#'   If the report generation returns \code{NULL}, then \code{NULL} is returned.
#'
#' @details
#' After generating the report, if a non-\code{NULL} \code{remove} pattern is provided, the function writes a message
#' indicating that it is removing matching labels from the formatted report, and then filters out any rows in the \code{formatted}
#' report whose \code{label} matches the pattern.
#'
#' @examples
#' \dontrun{
#' # Generate a non-time-varying feature extraction report for a given cohort
#' report <- getFeatureExtractionReportNonTimeVarying(
#'   cdmSources = myCdmSources,
#'   covariateDataPath = "path/to/covariateData",
#'   cohortId = 123,
#'   cohortDefinitionSet = myCohortDefinitionSet
#' )
#' # View the formatted report
#' print(report$formatted)
#' }
#'
#' @export
getFeatureExtractionReportNonTimeVarying <-
  function(covariateDataPath,
           cohortId,
           cohortDefinitionSet,
           remove = "Visit Count|Chads 2 Vasc|Demographics Index Month|Demographics Post Observation Time|Visit Concept Count|Chads 2|Demographics Prior Observation Time|Dcsi|Demographics Time In Cohort|Demographics Index Year Month") {
    output <-
      getFeatureExtractionReport(
        covariateDataPath = covariateDataPath,
        includeNonTimeVarying = TRUE,
        minAverageValue = 0.01,
        includedCovariateIds = NULL,
        excludedCovariateIds = NULL,
        table1Specifications = NULL,
        simple = TRUE,
        cohortId = cohortId,
        covariateDataFileNamePattern = paste0(cohortId, "$"),
        cohortDefinitionSet = cohortDefinitionSet,
        cohortName = NULL,
        reportName = NULL,
        format = TRUE
      )
    
    if (is.null(output)) {
      return(NULL)
    }
    
    if (!is.null(remove)) {
      writeLines(paste0("removing from formatted report", remove))
      output$formattedFull <- output$formatted
      output$formatted <- output$formatted |>
        dplyr::filter(stringr::str_detect(
          string = .data$label,
          pattern = remove,
          negate = TRUE
        ))
    }
    
    return(output)
  }
