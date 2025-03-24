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


#' Generate Table 1 Specification Row for Analysis
#'
#' This function creates a specification row for Table 1 using analysis information.
#' It takes an analysis ID, concept IDs, covariate IDs, and a label, then produces a tibble
#' with the analysis ID, label, and a comma-separated string of covariate IDs. If concept IDs are provided,
#' they are combined with the analysis ID to generate additional covariate IDs.
#'
#' @param analysisId A numeric value representing the analysis ID. Must be of length one.
#' @param conceptIds An optional numeric vector representing concept IDs. Defaults to \code{NULL}.
#' @param covariateIds An optional numeric vector representing covariate IDs. Defaults to \code{NULL}.
#'   At least one of \code{conceptIds} or \code{covariateIds} must be provided.
#' @param label A character string specifying a label for the feature cohorts. Defaults to \code{"Feature cohorts"}.
#'
#' @return A tibble (data frame) with a single row and the following columns:
#'   \item{label}{The label for the feature cohorts.}
#'   \item{analysisId}{The provided analysis ID.}
#'   \item{covariateIds}{A comma-separated string of unique covariate IDs derived from the input \code{covariateIds}
#'   and \code{conceptIds} processed as \code{(conceptIds * 1000) + analysisId}.}
#'
#' @details
#' The function validates that at least one of \code{conceptIds} or \code{covariateIds} is provided,
#' and ensures that both \code{analysisId} and \code{label} are single values. If \code{conceptIds} is provided,
#' additional covariate IDs are computed using the formula \code{(conceptIds * 1000) + analysisId}. The resulting
#' covariate IDs (including any provided via \code{covariateIds}) are then made unique and concatenated into a
#' comma-separated string.
#'
#' @examples
#' \dontrun{
#' # Generate a Table 1 specification row using concept IDs only:
#' getTable1SpecificationsRow(analysisId = 1, conceptIds = c(101, 202), label = "Cohorts A")
#'
#' # Generate a Table 1 specification row using both concept IDs and covariate IDs:
#' getTable1SpecificationsRow(analysisId = 2, conceptIds = c(303), covariateIds = c(5000, 6000), label = "Cohorts B")
#' }
#'
#' @export
getTable1SpecificationsRow <- function(analysisId,
                                       conceptIds = NULL,
                                       covariateIds = NULL,
                                       label = "Feature cohorts") {
  if (is.null(conceptIds) & is.null(covariateIds)) {
    stop("please provide atleast conceptIds or covariateIds")
  }

  if (!length(analysisId) == 1) {
    stop("only one analysis id")
  }

  if (!length(label) == 1) {
    stop("only one label")
  }

  covariateIds <- c(covariateIds, (conceptIds * 1000) + analysisId) |>
    unique()

  output <- dplyr::tibble(
    label = label,
    analysisId = analysisId,
    covariateIds = paste0(covariateIds, collapse = ",")
  )
  return(output)
}



#' Generate Table 1 Specifications from Covariate Data
#'
#' This function generates specification rows for Table 1 based on covariate data and reference tables.
#' It extracts distinct analyses from the analysis reference and, for each analysis, collects the associated
#' covariate IDs from the covariate reference. A specification row is then created for each analysis using
#' \code{getTable1SpecificationsRow}. If \code{covariateData} is provided, it is used to extract both the
#' covariate reference (\code{covariateRef}) and the analysis reference (\code{analysisRef}).
#'
#' @param covariateData An optional object (typically a list) that contains \code{covariateRef} and \code{analysisRef}.
#'   If provided, these elements will be collected and used in place of the separately supplied \code{covariateRef} and
#'   \code{analysisRef} parameters.
#' @param covariateRef A data frame or tibble containing covariate reference data. This parameter is overridden if
#'   \code{covariateData} is provided.
#' @param analysisRef A data frame or tibble containing analysis reference data. This parameter is overridden if
#'   \code{covariateData} is provided.
#'
#' @return A tibble with one row per analysis containing Table 1 specification details. The tibble includes:
#'   \item{label}{A formatted label for the analysis (converted from camelCase to Title Case).}
#'   \item{analysisId}{The analysis ID.}
#'   \item{covariateIds}{A comma-separated string of unique covariate IDs associated with the analysis.}
#'
#' @details
#' The function follows these steps:
#'
#' \enumerate{
#'   \item If \code{covariateData} is provided, extract and collect \code{covariateRef} and \code{analysisRef}
#'         from it.
#'   \item Select distinct \code{analysisId} and \code{analysisName} from the analysis reference.
#'   \item For each analysis, filter the covariate reference to retrieve unique covariate IDs corresponding to that analysis.
#'   \item Create a Table 1 specification row using \code{getTable1SpecificationsRow} with the collected covariate IDs.
#'   \item Bind all specification rows into a single tibble.
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming myCovariateData is a list containing covariateRef and analysisRef:
#' table1Specs <- getTable1SpecificationsFromCovariateData(covariateData = myCovariateData)
#' print(table1Specs)
#'
#' # Alternatively, if covariateRef and analysisRef are provided separately:
#' table1Specs <- getTable1SpecificationsFromCovariateData(
#'   covariateRef = myCovariateRef,
#'   analysisRef = myAnalysisRef
#' )
#' print(table1Specs)
#' }
#'
#' @export
getTable1SpecificationsFromCovariateData <-
  function(covariateData = NULL,
           covariateRef = NULL,
           analysisRef = NULL) {
    table1Specifications <- c()

    if (!is.null(covariateData)) {
      covariateRef <- covariateData$covariateRef |>
        dplyr::collect()

      analysisRef <- covariateData$analysisRef |>
        dplyr::collect()
    }

    analysisNames <- analysisRef |>
      dplyr::select(analysisId, analysisName) |>
      dplyr::distinct()

    if (nrow(analysisNames) > 0) {
      for (i in (1:nrow(analysisNames))) {
        analysisName <-
          analysisNames[i, ]

        covariateIds <- covariateRef |>
          dplyr::collect() |>
          dplyr::filter(analysisId %in% analysisName$analysisId) |>
          dplyr::select(.data$covariateId) |>
          dplyr::distinct() |>
          dplyr::collect() |>
          dplyr::pull(.data$covariateId) |> # dont sort
          unique()

        table1Specifications[[i]] <-
          getTable1SpecificationsRow(
            analysisId = analysisName$analysisId,
            conceptIds = NULL,
            covariateIds = covariateIds,
            label = analysisName$analysisName |>
              SqlRender::camelCaseToTitleCase() |>
              stringr::str_trim() |>
              stringr::str_squish()
          )
      }
      table1Specifications <-
        dplyr::bind_rows(table1Specifications)
    }

    return(table1Specifications)
  }
