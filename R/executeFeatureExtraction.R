#' Execute feature extraction
#'
#' @details
#' This function executes the cohort covariate
#'
#' @param connectionDetails                   An object of type \code{connectionDetails} as created
#'                                            using the
#'                                            \code{\link[DatabaseConnector]{createConnectionDetails}}
#'                                            function in the DatabaseConnector package.
#' @param cdmDatabaseSchema                   Schema name where your patient-level data in OMOP CDM
#'                                            format resides. Note that for SQL Server, this should
#'                                            include both the database and schema name, for example
#'                                            'cdm_data.dbo'.
#' @param cohortDatabaseSchema                Schema with instantiated target cohorts.
#' @param cohortIds                           cohort ids.
#' @param cohortTable                         cohort Table Names
#' @param tempEmulationSchema                 Some database platforms like Oracle and Impala do not
#'                                            truly support temp tables. To emulate temp tables,
#'                                            provide a schema with write privileges where temp tables
#'                                            can be created.
#' @param outputFolder                        Name of local folder to place results; make sure to use
#'                                            forward slashes (/). Do not use a folder on a network
#'                                            drive since this greatly impacts performance.
#' @param covariateSettings                   FeatureExtraction covariateSettings object
#'
#' @export
executeFeatureExtraction <-
  function(connectionDetails = NULL,
           connection = NULL,
           cdmDatabaseSchema,
           vocabularyDatabaseSchema = cdmDatabaseSchema,
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
          covariateId = OhdsiHelpers::convertCohortIdToCovariateId(
            cohortIds = cohortId,
            cohortCovariateAnalysisId = cohortCovariateAnalysisId
          )
        )
      
      if (any(!is.null(includeCovariateIds),
              length(includeCovariateIds) > 0)) {
        covariateCohortDefinitionSet <- covariateCohortDefinitionSet |>
          dplyr::filter(.data$covariateId %in% c(includeCovariateIds))
      }
      
      includeCovariateIds <-
        covariateCohortDefinitionSet$covariateId
      
      cohortBasedTemporalCovariateSettings <-
        getFeatureExtractionDefaultTemporalCohortCovariateSettings(
          timeWindows = getCovariateSettingsTimeWindows(covariateSettings = covariateSettings),
          analysisId = cohortCovariateAnalysisId,
          covariateCohortDatabaseSchema = covariateCohortDatabaseSchema,
          covariateCohortTable = covariateCohortTable,
          covariateCohortDefinitionSet = covariateCohortDefinitionSet,
          valueType = 'binary',
          includedCovariateIds = includeCovariateIds
        )
    }
    
    if (is.null(covariateSettings)) {
      covariateSettings <- cohortBasedTemporalCovariateSettings
    } else {
      covariateSettings <- list(covariateSettings,
                                cohortBasedTemporalCovariateSettings)
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
        FeatureExtraction::saveCovariateData(covariateData = covariateData,
                                             file = file.path(outputFolder, cohortId))
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
