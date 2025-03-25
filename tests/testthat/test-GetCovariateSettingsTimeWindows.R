library(testthat)
library(Kala)

context("Tests for Getting Time Windows from Cov Settings")

test_that("Non-cumulative", {
  covariateSettings <- list(
    temporalStartDays = c(-365, -30, 1),
    temporalEndDays = c(-1, 0, 30)
  )
  
  expected <- tibble(
    startDay = c(-365,-365,-30,1),
    endDay = c(-1,-1,-0,30),
    periodName = c("d-365d-1","d-365d-1","d-30d0","d1d30"),
    windowType = c("one year prior","sequence of yearly intervals prior","short term prior","sequence of 30-day intervals after")
  )
  
  result <- getCovariateSettingsTimeWindows(covariateSettings = covariateSettings)
  
  expect_equal(result, expected)
})

