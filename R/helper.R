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
#' Check if a cohort has been instantiated.
#'
#' @description
#' Returns TRUE when a cohort has records
#'
#' @template connectionDetails
#' @template connection
#' @template cohortDatabaseSchema
#' @template cohortTable
#' @template oracleTempSchema
#' @param cohortId              The cohort definition ID used to reference the cohort in the cohort
#'                              table.
#'
#' @return                      Returns a data frame of cohort count, person year count, 
#'                              cohort_start (max and min dates), cohor_end (max and min dates) 
#' @examples
#' \dontrun{
#' getCohortSummary(connection = connection,
#'                  cohortDatabaseSchema = cohortDatabaseSchema,
#'                  cohortId = 111)
#' }
#' @export
getCohortSummary <- function(connectionDetails = NULL,
                             connection, 
                             cohortDatabaseSchema, 
                             oracleTempSchema = NULL,
                             cohortTable = 'cohort', 
                             cohortId) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(cohortId, add = errorMessage)
  checkmate::assertScalar(cohortTable)
  checkmate::assertCharacter(cohortTable)
  checkmate::assertScalar(cohortDatabaseSchema)
  checkmate::assertCharacter(cohortDatabaseSchema)
  checkmate::reportAssertions(errorMessage)
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  sql <- "SELECT COUNT(*) record,
                 COUNT(DISTINCT subject_id) persons,
                 SUM(CAST(DATEDIFF(DAY, cohort_start_date, cohort_end_date) AS BIGINT)) / 365.25 person_years,
                 MIN(cohort_start_date) cohort_start_date_min,
                 MAX(cohort_start_date) cohort_start_date_max,
                 MIN(cohort_end_date) cohort_end_date_min,
                 MAX(cohort_end_date) cohort_end_date_max
          FROM @cohort_database_schema.@cohort_table 
          WHERE cohort_definition_id = @cohort_id;"
  result <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                       sql = sql,
                                                       snakeCaseToCamelCase = TRUE,
                                                       cohort_database_schema = cohortDatabaseSchema,
                                                       cohort_table = cohortTable,
                                                       cohort_id = cohortId)
  return(result)
}
