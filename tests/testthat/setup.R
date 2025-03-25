library(testthat)
library(dplyr)
library(Kala)


cohortTableName <- paste0("ct_", paste(sample(letters, 10), collapse = ""))

dbms <- getOption("dbms", default = "sqlite")
message("************* Testing on ", dbms, " *************")

if (dir.exists(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))) {
  jdbcDriverFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
} else {
  jdbcDriverFolder <- tempfile("jdbcDrivers")
  dir.create(jdbcDriverFolder, showWarnings = FALSE)
  DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)

  if (!dbms %in% c("sqlite")) {
    DatabaseConnector::downloadJdbcDrivers(dbms, pathToDriver = jdbcDriverFolder)
  }

  withr::defer(
    {
      unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )
}

# Helper functions ------------
getTestResourceFilePath <- function(fileName) {
  return(system.file("testdata", fileName, package = "FeatureExtraction"))
}

# Use this instead of SqlRender directly to avoid errors when running
# individual test files
loadRenderTranslateUnitTestSql <- function(sqlFileName, targetDialect, tempEmulationSchema = NULL, ...) {
  sql <- SqlRender::readSql(system.file("sql/sql_server/unit_tests/", sqlFileName, package = "FeatureExtraction"))
  sql <- SqlRender::render(sql = sql, ...)
  sql <- SqlRender::translate(sql = sql, targetDialect = targetDialect, tempEmulationSchema = tempEmulationSchema)
  return(sql)
}

# create unit test data
createUnitTestData <- function(connectionDetails, cdmDatabaseSchema, ohdsiDatabaseSchema, cohortTable, cohortAttributeTable, attributeDefinitionTable, cohortDefinitionIds = c(1)) {
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- loadRenderTranslateUnitTestSql(
    sqlFileName = "createTestingData.sql",
    targetDialect = connectionDetails$dbms,
    tempEmulationSchema = ohdsiDatabaseSchema,
    attribute_definition_table = attributeDefinitionTable,
    cdm_database_schema = cdmDatabaseSchema,
    cohort_attribute_table = cohortAttributeTable,
    cohort_database_schema = ohdsiDatabaseSchema,
    cohort_definition_ids = cohortDefinitionIds,
    cohort_table = cohortTable
  )
  DatabaseConnector::executeSql(connection, sql)
  return(connection)
}

dropUnitTestData <- function(connection, ohdsiDatabaseSchema, cohortTable, cohortAttributeTable, attributeDefinitionTable) {
  sql <- loadRenderTranslateUnitTestSql(
    sqlFileName = "dropTestingData.sql",
    targetDialect = connection@dbms,
    tempEmulationSchema = ohdsiDatabaseSchema,
    attribute_definition_table = attributeDefinitionTable,
    cohort_attribute_table = cohortAttributeTable,
    cohort_database_schema = ohdsiDatabaseSchema,
    cohort_table = cohortTable
  )
  DatabaseConnector::executeSql(connection, sql)
  DatabaseConnector::disconnect(connection)
}

checkRemoteFileAvailable <- function(remoteFile) {
  try_GET <- function(x, ...) {
    tryCatch(
      httr::GET(url = x, httr::timeout(10), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }
  
  resp <- try_GET(remoteFile)
  if (!is_response(resp)) {
    message(resp)
    return(NULL)
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) {
    message_for_status(resp)
    return(NULL)
  }
  return("success")
}

tableSuffix <- paste0(substr(.Platform$OS.type, 1, 3), format(Sys.time(), "%y%m%d%H%M%S"), sample(1:100, 1))
cohortTable <- paste0("#fe", tableSuffix)
cohortAttributeTable <- paste0("c_attr_", tableSuffix)
attributeDefinitionTable <- paste0("attr_def_", tableSuffix)

if (dbms == "sqlite") {
  if (!is.null(checkRemoteFileAvailable("https://raw.githubusercontent.com/OHDSI/EunomiaDatasets/main/datasets/GiBleed/GiBleed_5.3.zip"))) {
    eunomiaConnectionDetails <- Eunomia::getEunomiaConnectionDetails(databaseFile = "testEunomia.sqlite")
    eunomiaCdmDatabaseSchema <- "main"
    eunomiaOhdsiDatabaseSchema <- "main"
    eunomiaConnection <- createUnitTestData(eunomiaConnectionDetails, eunomiaCdmDatabaseSchema, eunomiaOhdsiDatabaseSchema, cohortTable, cohortAttributeTable, attributeDefinitionTable)
    Eunomia::createCohorts(
      connectionDetails = eunomiaConnectionDetails,
      cdmDatabaseSchema = eunomiaCdmDatabaseSchema,
      cohortDatabaseSchema = eunomiaOhdsiDatabaseSchema,
      cohortTable = "cohort"
    )
  }
  withr::defer(
    {
      if (exists("eunomiaConnection")) {
        dropUnitTestData(eunomiaConnection, eunomiaOhdsiDatabaseSchema, cohortTable, cohortAttributeTable, attributeDefinitionTable)
        unlink("testEunomia.sqlite", recursive = TRUE, force = TRUE)
      }
    },
    testthat::teardown_env()
  )
}