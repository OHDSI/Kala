library(Kala)

#Testing new logger
ParallelLogger::addDefaultErrorReportLogger()

# PDW --------------------------------------------------------
connectionDetails <- createConnectionDetails(dbms = "pdw",
                                             server = Sys.getenv("PDW_SERVER"),
                                             port = Sys.getenv("PDW_PORT"))
oracleTempSchema <- NULL
workDatabaseSchema <- "scratch.dbo"
