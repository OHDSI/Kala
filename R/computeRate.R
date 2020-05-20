# Copyright 2020 Observational Health Data Sciences and Informatics
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



#' @title
#' Returns measurement per day for cohort
#'
#' @description
#' Returns measurement per day for cohort stratified by age and gender
#'
#' @template connectionDetails
#' @template connection
#' @template cohortDatabaseSchema
#' @template cdmDatabaseSchema
#' @template cohortTable
#' @template oracleTempSchema
#' @param washoutPeriod         The minimum amount of observation time required before the occurrence
#'                              of a cohort entry. This is also used to eliminate immortal time from
#'                              the denominator.
#' @param cohortId              The cohort definition ID used to reference the cohort in the cohort
#'                              table.
#' @param asTsibble             Should the returned data frame be in tsibble format?
#'
#' @return                      If tsibble = TRUE (default), then returns a tsibble (data frame) with keys 
#'                              (gender, ageGroup, washoutPeriod, cohortId), 
#'                              index = calendarDate The keys are parameters used in function call.
#'                              Else tibble
#'                    
#' @export
getTimeSeriesMeasures <- function(connectionDetails = NULL,
                            connection = NULL,
                            cohortDatabaseSchema,
                            cdmDatabaseSchema,
                            cohortTable = 'cohort',
                            oracleTempSchema = NULL,
                            washoutPeriod = 365,
                            cohortId,
                            asTsibble = TRUE) {
  
  startClockTime <- Sys.time()
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::assertScalar(cohortTable, add = errorMessage)
  checkmate::assertCharacter(cohortTable, add = errorMessage)
  checkmate::assertScalar(cohortDatabaseSchema, add = errorMessage)
  checkmate::assertCharacter(cohortDatabaseSchema, add = errorMessage)
  checkmate::assertInt(washoutPeriod, add = errorMessage)
  checkmate::assertLogical(asTsibble, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  cohortSummary <- getCohortSummary(
    connection = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cohortId = cohortId
  )
  
  if (cohortSummary$record == 0) {
    warning("Cohort with ID ", cohortId, " appears to be empty. Was it instantiated?")
    delta <- Sys.time() - startClockTime
    ParallelLogger::logInfo(paste("Computing rates took",
                                  signif(delta, 3),
                                  attr(delta, "units")))
    return(data.frame())
  }
  
  ParallelLogger::logInfo(paste0("Creating reference calendar_period table"))
  startInsertTable <- Sys.time()
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CalendarDates.sql",
                                           packageName = "Kala",
                                           dbms = connection@dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           startDate = cohortSummary$cohortStartDateMin,
                                           endDate = cohortSummary$cohortEndDateMax)
  DatabaseConnector::executeSql(connection = connection,
                                 sql = sql)
  
  delta <- Sys.time() - startInsertTable
  ParallelLogger::logInfo(paste("   took ",
                                signif(delta, 3),
                                attr(delta, "units"))
                          )
  
  ParallelLogger::logInfo(paste0("Calculating Timeseries measures stratified by age and gender and calendar"))
  startCalculation <- Sys.time()
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "ComputeTimeSeriesDay.sql",
                                           packageName = "Kala",
                                           dbms = connection@dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           cohort_table = cohortTable,
                                           washout_period = washoutPeriod,
                                           cohort_id = cohortId
                                           )
  DatabaseConnector::executeSql(connection, sql) #, profile = TRUE
  delta <- Sys.time() - startCalculation
  ParallelLogger::logInfo(paste("   calculation took ",
                                signif(delta, 3),
                                attr(delta, "units"))
  )
  
  sql <- "SELECT * FROM #time_series;"
  timeSeries <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                             sql = sql,
                                                             oracleTempSchema = oracleTempSchema,
                                                             snakeCaseToCamelCase = TRUE) %>% 
    dplyr::tibble()
  
  sql <- "TRUNCATE TABLE #time_series; DROP TABLE #time_series;"
  DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                               sql = sql,
                                               progressBar = FALSE,
                                               reportOverallTime = FALSE,
                                               oracleTempSchema = oracleTempSchema)
  
  timeSeries <- timeSeries %>% 
    dplyr::mutate(ageGroup = paste(formatC(ageGroup*10, width=2, flag="0"),
                                   formatC((ageGroup*10)+9, width=2, flag="0"),
                                   sep = "-"
    ),
    gender = stringr::str_to_sentence(gender)
    )
  
  timeSeries <- timeSeries %>% 
    dplyr::mutate(washoutPeriod = washoutPeriod,
                  cohortId = cohortId)
  
  if (isTRUE(asTsibble)) { 
    timeSeries <- timeSeries %>% 
            tsibble::as_tsibble(key = c(gender, ageGroup, washoutPeriod, cohortId),
                                  validate = TRUE,
                                  index = calendarDate) %>%
              tsibble::group_by_key() %>% 
              tsibble::fill_gaps(in_observation = 0,
                                 at_risk_first = 0,
                                 incidence = 0,
                                 prevalence = 0,
                                 incidence_first = 0,
                                 prevalence_first = 0,
                                 at_risk = 0
                                 )
  }
  delta <- Sys.time() - startClockTime
  ParallelLogger::logInfo(paste("Computing timeseries took ",
                                signif(delta, 3),
                                attr(delta, "units")))
  return(timeSeries)
}