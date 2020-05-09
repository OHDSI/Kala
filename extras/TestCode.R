library(Kala)

#Testing new logger
ParallelLogger::addDefaultErrorReportLogger()

# PDW --------------------------------------------------------
connectionDetails <- createConnectionDetails(dbms = "pdw",
                                             server = Sys.getenv("PDW_SERVER"),
                                             port = Sys.getenv("PDW_PORT"))
oracleTempSchema <- NULL
workDatabaseSchema <- "scratch.dbo"

# create date span for testing
startDate = lubridate::ymd(c("2020-01-01","2020-03-01","2020-03-10","2020-03-20","2020-04-10","2020-05-01","2020-05-05","2020-05-10","2020-07-07"))
endDate = lubridate::ymd(c("2020-01-21","2020-03-21","2020-03-31","2020-04-09","2020-04-30","2020-05-21","2020-05-26","2020-05-30","2020-07-14"))
id = c(1,2,2,1,1,1,1,2,2)
dateSpan <- tidyr::tibble(id, startDate, endDate)

# collapseDateSpan
collapseData <- Kala::collapseDateSpan(x = dateSpan)
x = collapseData

# convert Date span to vector
dateVector <- Kala::convertDateSpanToDateVector(x = dateSpan, startDate = 'startDate', endDate = 'endDate')

# convert Date vector to date span - with default
dateSpan2 <- Kala::convertDateVectorToDateSpan(x = dateVector)


# convert Date vector to date span - with default
dateSpan3 <- Kala::convertDateVectorToDateSpan(x = dateVector, unit = "week")


# convert Date vector to date span - with default
dateSpan4 <- Kala::convertDateVectorToDateSpan(x = dateVector, unit = "weeks", week_start = 7)
