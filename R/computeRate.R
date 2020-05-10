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
#' Compute rate for a cohort
#'
#' @description
#' Returns rate stratified by age and gender
#'
#' @template connectionDetails
#' @template connection
#' @template cohortDatabaseSchema
#' @template cohortTable
#' @template oracleTempSchema
#' @param firstOccurrenceOnly   Use only the first occurrence of the cohort per person?
#' @param washoutPeriod         The minimum amount of observation time required before the occurrence
#'                              of a cohort entry. This is also used to eliminate immortal time from
#'                              the denominator.
#' @param calendarPeriod        A R-object that has two date columns periodBegin, periodEnd where 
#'                              periodEnd >= periodBegin (non-overlapping). Function 
#'                              \code{Kala::convertDateVectorToDateSpan} maybe used to generate this 
#'                              data frame If not provided, a default calendarPeriod 
#'                              will be computed based on calendar years between cohort_start_date 
#'                              and cohort_end_date.
#' @param rateType              Do you want 'incidence' or 'prevalence' for a calendarPeriod? 
#'                              Default = 'incidence'.
#' @param cohortId              The cohort definition ID used to reference the cohort in the cohort
#'                              table.
#'
#' @return                      Returns a data frame of cohort count, person year count, period begin, 
#'                              period end with the following stratifications: 1) no stratification, 
#'                              2) gender stratification, 3) age (10-year) stratification,
#'
#' @export
getRate <- function(connectionDetails = NULL,
                    connection = NULL,
                    cohortDatabaseSchema,
                    cohortTable = 'cohort',
                    oracleTempSchema = NULL,
                    firstOccurrenceOnly = TRUE,
                    washoutPeriod = 365,
                    cohortId,
                    calendarPeriod = NULL,
                    rateType = 'incidence') {
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::assertScalar(cohortTable, add = errorMessage)
  checkmate::assertCharacter(cohortTable, add = errorMessage)
  checkmate::assertScalar(cohortDatabaseSchema, add = errorMessage)
  checkmate::assertCharacter(cohortDatabaseSchema, add = errorMessage)
  checkmate::assertLogical(firstOccurrenceOnly, add = errorMessage)
  checkmate::assertInt(washoutPeriod, add = errorMessage)
  checkmate::assertScalar(rateType, add = errorMessage)
  checkmate::assertChoice(rateType, choices = c('incidence', 'prevalence'), add = errorMessage)
  if (!is.null(calendarPeriod)) {
    checkmate::assertDataFrame(calendarPeriod, min.rows = 1, min.cols = 1, add = errorMessage)
    checkmate::assertNames(names(calendarPeriod), must.include = c('periodBegin', 'periodEnd'), add = errorMessage)
  }
  checkmate::reportAssertions(errorMessage)
  
  start <- Sys.time()
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  cohortSummary <- Kala::getCohortSummary(connection = connection,
                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                          cohortTable = cohortTable,
                                          cohortId = cohortId)
  if (cohortSummary$record == 0) {
    warning("Cohort with ID ", cohortId, " appears to be empty. Was it instantiated?")
    delta <- Sys.time() - start
    ParallelLogger::logInfo(paste("Computing rates took",
                                  signif(delta, 3),
                                  attr(delta, "units")))
    return(data.frame())
  }
  
  if (is.null(calendarPeriod)) {
    calendarPeriod <- Kala::convertDateSpanToDateVector(x = cohortSummary,
                                                        startDate = 'cohortStartDateMin',
                                                        endDate = 'cohortEndDateMax') %>% 
      Kala::convertDateVectorToDateSpan(unit = "year") %>% 
      dplyr::rename(periodBegin = startDate, periodEnd = endDate)
  }
  
  calendarPeriod <- calendarPeriod %>% 
    Kala::collapseDateSpan(startDate = 'periodBegin', endDate = 'periodEnd', gap = 0) %>% 
    dplyr::rename(periodBegin = startDate, periodEnd = endDate)
  
  ParallelLogger::logInfo(paste0("Creating reference calendar_period table"))
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = "calendar_periods",
                                 data = calendarPeriod,
                                 dropTableIfExists = TRUE,
                                 createTable = TRUE,
                                 tempTable = TRUE,
                                 oracleTempSchema = oracleTempSchema,
                                 progressBar = TRUE,
                                 camelCaseToSnakeCase = TRUE,
                                 useMppBulkLoad = FALSE)
  
  ParallelLogger::logInfo(paste0("Calculating rate stratified by age and gender and calendar"))
  
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "ComputeTimeSeries.sql",
                                           packageName = "Kala",
                                           dbms = connection@dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           first_occurrence_only = firstOccurrenceOnly,
                                           washout_period = washoutPeriod,
                                           cohort_id = cohortId,
                                           rateType = rateType)
  DatabaseConnector::executeSql(connection, sql)
  
  sql <- "SELECT * FROM #rates_summary;"
  ratesSummary <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                             sql = sql,
                                                             oracleTempSchema = oracleTempSchema,
                                                             snakeCaseToCamelCase = TRUE) %>% 
    dplyr::tibble()
  
  sql <- "TRUNCATE TABLE #rates_summary; DROP TABLE #rates_summary;"
  DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                               sql = sql,
                                               progressBar = FALSE,
                                               reportOverallTime = FALSE,
                                               oracleTempSchema = oracleTempSchema)
  
  ratesSummary <- ratesSummary %>% 
    dplyr::mutate(ageGroup = paste(formatC(ageGroup*10, width=2, flag="0"),
                                   formatC((ageGroup*10)+9, width=2, flag="0"),
                                   sep = "-"
    ),
    gender = stringr::str_to_sentence(gender)
    )
  
  ratePeriod <- ratesSummary %>% 
    dplyr::group_by(periodBegin, periodEnd) %>% 
    dplyr::summarise(cohortCount = sum(cohortCount),
                     personYears = sum(personYears)
    )
  
  ratePeriodAge <- ratesSummary %>% 
    dplyr::group_by(periodBegin, periodEnd, ageGroup) %>% 
    dplyr::summarise(cohortCount = sum(cohortCount),
                     personYears = sum(personYears)
    )
  
  ratePeriodGender <- ratesSummary %>% 
    dplyr::group_by(periodBegin, periodEnd, gender) %>% 
    dplyr::summarise(cohortCount = sum(cohortCount),
                     personYears = sum(personYears)
    )
  
  ratePeriodAgeGender <- ratesSummary %>% 
    dplyr::group_by(periodBegin, periodEnd, ageGroup, gender) %>% 
    dplyr::summarise(cohortCount = sum(cohortCount),
                     personYears = sum(personYears)
    )
  
  result <- dplyr::bind_rows(ratePeriod,
                             ratePeriodAge,
                             ratePeriodGender,
                             ratePeriodAgeGender) %>% 
    dplyr::tibble() %>% 
    dplyr::mutate(ratePer1000 = 1000 * (cohortCount*1.0)/(personYears*1.0))
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Computing incidence rates took",
                                signif(delta, 3),
                                attr(delta, "units")))
  
  result <- result %>% 
    dplyr::mutate(firstOccurrenceOnly = TRUE,
                  washoutPeriod = 365,
                  rateType = rateType) %>% 
    tsibble::as_tsibble(result, 
                        key = c(gender, ageGroup, 
                                firstOccurrenceOnly, washoutPeriod, 
                                rateType),
                        validate = TRUE,
                        index = 'periodBegin')
  return(result)
}