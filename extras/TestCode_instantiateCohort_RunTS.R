
######################################################################################
######################################################################################
######################################################################################
######################################################################################
filePathSourceFiles <- "E:/ignore"
# instantiate a cohort in scratch, get ts data and store it as an Andromeda object
library(magrittr)
source(paste0(filePathSourceFiles, '/ignoreThisFile.R'))
connectionDetails = NULL
connection = NULL

resultsFolder = "E:/andromeda/timeSeries"
dir.create(path = resultsFolder, showWarnings = FALSE, recursive = TRUE)


connectionDetailsMetaData <- redShiftConnectionDetailsMetaData %>% 
  dplyr::filter(sourceKey %in% c('OPTUM_EXTENDED_DOD', #'CPRD', , 'IBM_MDCD', 'IBM_MDCR'
                                 # 'IQVIA_AUSTRALIA_EMR', 'IQVIA_FRANCE_DA',
                                 #'IQVIA_GERMANY_DA', 
                                 'IBM_CCAE',
                                 'OPTUM_PANTHER')
  )

cohort <- 'cohort'
washoutPeriod <- 365
# rateTypes <- c('incidence')#,'prevalence'
cohortIds <- c(11658,15996,
               16014,
               16011,
               16013,
               16012,
               16010,
               10608,15942,15943,15264,11652,15266,15265,14637,11643)

z <- 0
for (k in (1:nrow(connectionDetailsMetaData))) { #k = 1
  
  connectionDetailMetaData <- connectionDetailsMetaData %>% dplyr::slice(k)
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = connectionDetailMetaData$dbms,
                                                                  user = Sys.getenv("user"),
                                                                  password = Sys.getenv("passwordSecureAWS"),
                                                                  server = connectionDetailMetaData$server,
                                                                  port = connectionDetailMetaData$port
  )
  print(z)
  print(paste0("working on source name = ", connectionDetailMetaData$sourceName))
  

  for (i in (1:length(cohortIds))) { # i = 1
  
    
      cohortId <- cohortIds[i]
    # get cohorts SQL from WebApi
    sql <- ROhdsiWebApi::getCohortDefinitionSql(baseUrl = Sys.getenv("baseUrl"), 
                                                cohortId = cohortId, 
                                                generateStats = FALSE
    )
    cohortName <- ROhdsiWebApi::getCohortDefinition(baseUrl = Sys.getenv("baseUrl"),cohortId = cohortId)$name
    print(paste0("..cohortName: ",cohortName))
    
    
    # instantiate the cohort table if needed
    tablesInTargetSchema = tidyr::tibble(name = DatabaseConnector::getTableNames(connection = DatabaseConnector::connect(connectionDetails = connectionDetails), 
                                     databaseSchema = connectionDetailMetaData$cohortDatabaseSchema) %>% toupper()
                          ) %>% 
                          dplyr::filter(name == toupper(cohort))
    
    if (nrow(tablesInTargetSchema) == 0) {
      CohortDiagnostics::createCohortTable(connection = DatabaseConnector::connect(connectionDetails = connectionDetails), 
                                           cohortDatabaseSchema = connectionDetailMetaData$cohortDatabaseSchema,
                                           cohortTable = cohort)
    }
    
    
    # instantiate the cohort
    DatabaseConnector::renderTranslateExecuteSql(connection = DatabaseConnector::connect(connectionDetails = connectionDetails),
                                                 sql = sql,
                                                 vocabulary_database_schema = connectionDetailMetaData$vocabDatabaseSchema,
                                                 cdm_database_schema = connectionDetailMetaData$cdmDatabaseSchema,
                                                 target_cohort_table = cohort,
                                                 target_database_schema = connectionDetailMetaData$cohortDatabaseSchema,
                                                 target_cohort_id = cohortId)
    print("....generated cohort")
    # get results for both rateType
      
    print(Sys.time())
      result <- Kala::getTimeSeriesMeasures(connectionDetails = connectionDetails, 
                                      cohortDatabaseSchema = connectionDetailMetaData$cohortDatabaseSchema, 
                                      cdmDatabaseSchema = connectionDetailMetaData$cdmDatabaseSchema,
                                      cohortTable = cohort, 
                                      oracleTempSchema = NULL, 
                                      washoutPeriod = washoutPeriod, 
                                      cohortId = cohortId,
                                      asTsibble = FALSE) 
      result <- result %>% 
        dplyr::mutate(cohortName = cohortName,
                      sourceKey = connectionDetailMetaData$sourceKey,
                      sourceName = connectionDetailMetaData$sourceName
        )
      print(paste0("........compute completed"))
      if (z == 0) {
        storeAndromeda <- Andromeda::andromeda(timeSeries = result)
        Andromeda::saveAndromeda(andromeda = storeAndromeda, 
                                 fileName = paste0(resultsFolder, '/results.zip')
        )
      } else {
        storeAndromeda <- Andromeda::loadAndromeda(paste0(resultsFolder, '/results.zip'))
        Andromeda::appendToTable(storeAndromeda$timeSeries, result)
        Andromeda::saveAndromeda(andromeda = storeAndromeda, 
                                 fileName = paste0(resultsFolder, '/results.zip')
                                 )
      }
  }
}
