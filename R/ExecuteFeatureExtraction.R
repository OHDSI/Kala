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

#' Execute feature extraction using temporal covariate settings.
#'
#' @description
#' This function executes the feature extraction on one or more cohortId in a cohort table and returns covariate data.
#'
#' @param connectionDetails   An object of type \code{connectionDetails} as created using
#'                            \code{\link[DatabaseConnector]{createConnectionDetails}}. Can be left NULL if \code{connection} is provided. Both cannot be NULL.
#' @param connection          An object of type \code{connection} as created using
#'                            \code{\link[DatabaseConnector]{connect}}. Can be left NULL if \code{connectionDetails} is provided. Both cannot be NULL.
#' @param cdmDatabaseSchema   Schema name where your patient-level data in OMOP CDM format resides.
#'                            For SQL Server, include both database and schema name (e.g., 'cdm_data.dbo').
#' @param cohortIds           Vector of cohort IDs for which covariate extraction is to be performed.
#' @param cohortDatabaseSchema   Schema name where your cohort tables reside. For SQL Server,
#'                               include both database and schema name (e.g., 'scratch.dbo').
#' @param cohortTable         Name of the table containing cohort data.
#' @param covariateSettings   A FeatureExtraction covariateSettings object. If NULL, cohort-based temporal
#'                            covariate settings will be used.
#' @param addCohortBasedTemporalCovariateSettings   Logical flag indicating whether to add cohort-based
#'                            temporal covariate settings. Default is TRUE.
#' @param covariateCohortDatabaseSchema Schema name where covariate cohort data resides. Default is the same as \code{cohortDatabaseSchema}.
#' @param includeCovariateIds Optional vector of covariate IDs to include.
#' @param covariateCohortTable Name of the table containing covariate cohort data. Default is the same as \code{cohortTable}.
#' @param covariateCohortDefinitionSet A data frame defining covariate cohort definitions used to generate covariate IDs.
#' @param cohortCovariateAnalysisId An integer identifier used to generate covariate IDs. Default is 150.
#' @param tempEmulationSchema Some database platforms (e.g., Oracle, Impala) do not support temporary tables.
#'                            Provide a schema with write privileges where temporary tables can be created.
#'                            Default is obtained from \code{getOption("sqlRenderTempEmulationSchema")}.
#' @param outputFolder        Name of the local folder to place results. Use forward slashes (/); avoid network drives.
#' @param aggregated          Logical flag indicating whether to aggregate covariate data. Default is TRUE.
#' @param rowIdField          Field name used as the row identifier (e.g., "subject_id" or "row_id"). Default is "subject_id".
#' @param incremental         Logical flag indicating whether to skip processing if output for a cohort already exists. Default is TRUE.
#'
#' @return No explicit return value. The function saves the covariate data files to the specified output folder.
#'
#' @examples
#' \dontrun{
#' executeFeatureExtraction(
#'   connectionDetails = myConnectionDetails,
#'   cdmDatabaseSchema = "cdm_data.dbo",
#'   cohortDatabaseSchema = "scratch.dbo",
#'   cohortIds = c(1, 2, 3),
#'   cohortTable = "cohort_table",
#'   outputFolder = "results/output",
#'   covariateSettings = myCovariateSettings
#' )
#' }
#'
#' @export
executeFeatureExtraction <-
  function(connectionDetails = NULL,
           connection = NULL,
           cdmDatabaseSchema,
           cohortDatabaseSchema,
           cohortIds,
           cohortTable,
           covariateSettings = NULL,
           addCohortBasedTemporalCovariateSettings = TRUE,
           covariateCohortDatabaseSchema = cohortDatabaseSchema,
           includeCovariateIds = NULL,
           covariateCohortTable = cohortTable,
           covariateCohortDefinitionSet = NULL,
           cohortCovariateAnalysisId = 150,
           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
           outputFolder,
           aggregated = TRUE,
           rowIdField = "subject_id",
           incremental = TRUE) {
    if (!file.exists(outputFolder)) {
      dir.create(outputFolder, recursive = TRUE)
    }

    if (!aggregated) {
      if (length(cohortIds) > 1) {
        stop("only one cohort id allowed when doing aggregated = FALSE")
      }
    }

    cohortBasedTemporalCovariateSettings <- NULL

    if (addCohortBasedTemporalCovariateSettings) {
      if (!is.null(covariateSettings)) {
        timeWindows <-
          getCovariateSettingsTimeWindows(covariateSettings = covariateSettings)
      } else {
        timeWindows <- getFeatureExtractionDefaultTimeWindows()
      }

      if (is.null(covariateCohortDefinitionSet)) {
        stop(
          "covariateCohortDefinitionSet is NULL. cannot run cohort temporal characterization"
        )
      }
      if (is.null(covariateCohortTable)) {
        stop("covariateCohortTable is NULL. cannot run cohort temporal characterization")
      }
      if (is.null(covariateCohortDatabaseSchema)) {
        stop(
          "covariateCohortDatabaseSchema is NULL. cannot run cohort temporal characterization"
        )
      }
      if (is.null(cohortCovariateAnalysisId)) {
        stop(
          "cohortCovariateAnalysisId is NULL. cannot run cohort temporal characterization"
        )
      }

      covariateCohortDefinitionSet <-
        covariateCohortDefinitionSet |>
        dplyr::mutate(
          covariateId = convertCohortIdToCovariateId(
            cohortIds = cohortId,
            cohortCovariateAnalysisId = cohortCovariateAnalysisId
          )
        )

      if (any(
        !is.null(includeCovariateIds),
        length(includeCovariateIds) > 0
      )) {
        covariateCohortDefinitionSet <- covariateCohortDefinitionSet |>
          dplyr::filter(.data$covariateId %in% c(includeCovariateIds))
      }

      includeCovariateIds <-
        covariateCohortDefinitionSet$covariateId

      cohortBasedTemporalCovariateSettings <-
        getFeatureExtractionDefaultTemporalCohortCovariateSettings(
          timeWindows = timeWindows,
          analysisId = cohortCovariateAnalysisId,
          covariateCohortDatabaseSchema = covariateCohortDatabaseSchema,
          covariateCohortTable = covariateCohortTable,
          covariateCohortDefinitionSet = covariateCohortDefinitionSet,
          valueType = "binary",
          includedCovariateIds = includeCovariateIds
        )
    }

    if (is.null(covariateSettings)) {
      covariateSettings <- cohortBasedTemporalCovariateSettings
    } else {
      covariateSettings <- list(covariateSettings)
    }

    if (!is.null(outputFolder)) {
      ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
      ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
      on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
      on.exit(
        ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE),
        add = TRUE
      )
    }

    if (is.null(connection)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }

    useRowId <- (rowIdField == "row_id")
    if (useRowId) {
      sql <- "
      DROP TABLE  IF EXISTS #cohort_person;

      SELECT ROW_NUMBER() OVER (ORDER BY subject_id, cohort_start_date) AS row_id,
              cohort_definition_id,
              subject_id,
              cohort_start_date,
              cohort_end_date
      INTO #cohort_person
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id IN (@target_cohort_id);

      "
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = sql,
        cohort_database_schema = cohortDatabaseSchema,
        cohort_table = cohortTable,
        target_cohort_id = cohortIds,
        profile = FALSE,
        progressBar = FALSE,
        reportOverallTime = FALSE
      )
      cohortDatabaseSchema <- NULL
      cohortTable <- "#cohort_person"
    }

    ParallelLogger::logInfo(" - Beginning Feature Extraction")

    for (x in (1:length(cohortIds))) {
      cohortId <- cohortIds[[x]]
      ParallelLogger::logInfo(paste0("   - cohort id: ", cohortId))

      skipCohort <- FALSE
      if (incremental) {
        if (file.exists(file.path(outputFolder, cohortId))) {
          skipCohort <- TRUE
        }
      }

      if (!skipCohort) {
        covariateData <-
          FeatureExtraction::getDbCovariateData(
            connection = connection,
            oracleTempSchema = tempEmulationSchema,
            cdmDatabaseSchema = cdmDatabaseSchema,
            cohortDatabaseSchema = cohortDatabaseSchema,
            cdmVersion = 5,
            cohortTable = cohortTable,
            cohortIds = cohortId,
            covariateSettings = covariateSettings,
            aggregated = aggregated,
            cohortTableIsTemp = is.null(cohortDatabaseSchema),
            rowIdField = rowIdField
          )

        dir.create(
          path = file.path(outputFolder),
          showWarnings = FALSE,
          recursive = TRUE
        )
        FeatureExtraction::saveCovariateData(
          covariateData = covariateData,
          file = file.path(
            outputFolder,
            paste0("covariateData_", "cohortId_", cohortId, ".zip")
          )
        )
      } else {
        ParallelLogger::logInfo(paste0("    - skipping cohort id: ", cohortId))
      }
    }

    if (useRowId) {
      sql <- "DROP TABLE  IF EXISTS #cohort_person;"
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = sql,
        profile = FALSE,
        progressBar = FALSE,
        reportOverallTime = FALSE
      )
    }
  }
