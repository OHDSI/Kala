library(testthat)
library(Kala)

context("Tests for Default Time Covariates")

test_that("Non-cumulative", {
  cumulative <- FALSE

  expected <- tibble(
    startDay = c(NA, -30, -30, -60, -90, -120, 31, 61, 91, -61, -91, -121, -151, -181, -211, -241, -271, -301, -331, -361, -391, 1, 31, 61, 91, 121, 151, 181, 211, 241, 271, 301, 331, -365, -730, -1095, -1460, -1825, -2190, -2555, -2920, -3285, -3650, -4015, -364, 1, 366, 731, 1096, 1461, 1826, 2191, 2556, 2921, 3286),
    endDay = c(NA, -1, -1, -31, -61, -91, 60, 90, 120, -32, -62, -92, -122, -152, -182, -212, -242, -272, -302, -332, -362, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360, -1, -366, -731, -1096, -1461, -1826, -2191, -2556, -2921, -3286, -3651, 0, 365, 730, 1095, 1460, 1825, 2190, 2555, 2920, 3285, 3650),
    periodName = c("dNAdNA", "d-30d-1", "d-30d-1", "d-60d-31", "d-90d-61", "d-120d-91", "d31d60", "d61d90", "d91d120", "d-61d-32", "d-91d-62", "d-121d-92", "d-151d-122", "d-181d-152", "d-211d-182", "d-241d-212", "d-271d-242", "d-301d-272", "d-331d-302", "d-361d-332", "d-391d-362", "d1d30", "d31d60", "d61d90", "d91d120", "d121d150", "d151d180", "d181d210", "d211d240", "d241d270", "d271d300", "d301d330", "d331d360", "d-365d-1", "d-730d-366", "d-1095d-731", "d-1460d-1096", "d-1825d-1461", "d-2190d-1826", "d-2555d-2191", "d-2920d-2556", "d-3285d-2921", "d-3650d-3286", "d-4015d-3651", "d-364d0", "d1d365", "d366d730", "d731d1095", "d1096d1460", "d1461d1825", "d1826d2190", "d2191d2555", "d2556d2920", "d2921d3285", "d3286d3650"),
    windowType = c(NA, "30 day prior not including day 0", "last 30 days not including day 0", "last 60 days not including day 0", "last 90 days not including day 0", "last 120 days not including day 0", "31-60 days after", "61-90 days after", "91-120 days after", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of 30-day intervals after", "sequence of yearly intervals prior", "sequence of yearly intervals prior", "sequence of yearly intervals prior", "sequence of yearly intervals prior", "sequence of yearly intervals prior", "sequence of yearly intervals prior", "sequence of yearly intervals prior", "sequence of yearly intervals prior", "sequence of yearly intervals prior", "sequence of yearly intervals prior", "sequence of yearly intervals prior", "sequence of yearly intervals after", "sequence of yearly intervals after", "sequence of yearly intervals after", "sequence of yearly intervals after", "sequence of yearly intervals after", "sequence of yearly intervals after", "sequence of yearly intervals after", "sequence of yearly intervals after", "sequence of yearly intervals after", "sequence of yearly intervals after", "sequence of yearly intervals after")
  )

  result <- Kala::getFeatureExtractionDefaultTimeWindows(cumulative)
  expect_equal(result, expected)
})

test_that("Cumulative", {
  cumulative <- TRUE

  expected <- tibble(
    startDay = c(NA, -365, -180, -30, 0, -31, -61, -91, -121, -151, -181, -211, -241, -271, -301, -331, -361, -391, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    endDay = c(NA, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 31, 61, 91, 121, 151, 181, 211, 241, 271, 301, 331, 361),
    periodName = c("dNAdNA", "d-365d0", "d-180d0", "d-30d0", "d0d0", "d-31d-1", "d-61d-1", "d-91d-1", "d-121d-1", "d-151d-1", "d-181d-1", "d-211d-1", "d-241d-1", "d-271d-1", "d-301d-1", "d-331d-1", "d-361d-1", "d-391d-1", "d1d1", "d1d31", "d1d61", "d1d91", "d1d121", "d1d151", "d1d181", "d1d211", "d1d241", "d1d271", "d1d301", "d1d331", "d1d361"),
    windowType = c(NA, "long term prior", "medium term prior", "short term prior", "index date only", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence")
  )

  result <- Kala::getFeatureExtractionDefaultTimeWindows(cumulative)
  expect_equal(result, expected)
})

test_that("Period Types", {
  cumulative <- TRUE
  periodTypes <- "month"

  expected <- tibble(
    startDay = c(0, -31, -61, -91, -121, -151, -181, -211, -241, -271, -301, -331, -361, -391, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    endDay = c(0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 31, 61, 91, 121, 151, 181, 211, 241, 271, 301, 331, 361),
    periodName = c("d0d0", "d-31d-1", "d-61d-1", "d-91d-1", "d-121d-1", "d-151d-1", "d-181d-1", "d-211d-1", "d-241d-1", "d-271d-1", "d-301d-1", "d-331d-1", "d-361d-1", "d-391d-1", "d1d1", "d1d31", "d1d61", "d1d91", "d1d121", "d1d151", "d1d181", "d1d211", "d1d241", "d1d271", "d1d301", "d1d331", "d1d361"),
    windowType = c("index date only", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence")
  )

  result <- Kala::getFeatureExtractionDefaultTimeWindows(cumulative, periodTypes)
  expect_equal(result, expected)
})

test_that("Selected Cumulative", {
  selectedCumulative <- TRUE

  expected <- tibble(
    startDay = c(-9999, -365, -180, -30, 0, -31, -91, -181, -301, -391, -29, -31, -91, -181, -301, -391, 1, 1, 1, 1, 1, -364),
    endDay = c(0, 0, 0, 0, 0, -2, -62, -152, -272, -362, 0, -1, -1, -1, -1, -1, 31, 91, 181, 241, 361, 0),
    periodName = c("d-9999d0", "d-365d0", "d-180d0", "d-30d0", "d0d0", "d-31d-2", "d-91d-62", "d-181d-152", "d-301d-272", "d-391d-362", "d-29d0", "d-31d-1", "d-91d-1", "d-181d-1", "d-301d-1", "d-391d-1", "d1d31", "d1d91", "d1d181", "d1d241", "d1d361", "d-364d0"),
    windowType = c("anytime prior", "long term prior", "medium term prior", "short term prior", "index date only", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals after", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 30-day intervals prior", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of 1's matching length of above sequence", "sequence of yearly intervals after")
  )

  result <- Kala::getFeatureExtractionDefaultTimeWindows(selectedcumulative = selectedCumulative)
  expect_equal(result, expected)
})
