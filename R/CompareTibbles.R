# Copyright 2025 Observational Health Data Sciences and Informatics
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


#' Compare Two Tibbles for Differences
#'
#' This function compares two tibbles (or data frames) and returns a list describing the differences
#' between them. It first checks whether the two tibbles have identical columns (ignoring order) and
#' reports any additional columns found in either tibble. If the columns are identical, it sorts the rows
#' of both tibbles and performs a row-wise comparison. The output includes whether the tibbles are identical,
#' the difference in row counts, and the specific rows that are present in one tibble but not in the other.
#'
#' @param tibble1 A tibble or data frame to be compared.
#' @param tibble2 A tibble or data frame to be compared.
#'
#' @return A list with the following elements:
#'   \item{additionalColumnsInFirst}{A character vector of column names present in \code{tibble1} but not in \code{tibble2}.}
#'   \item{additionalColumnsInSecond}{A character vector of column names present in \code{tibble2} but not in \code{tibble1}.}
#'   \item{identical}{A logical value indicating whether the two tibbles are identical after aligning columns and sorting rows.}
#'   \item{additionalRowsInFirst}{(If not identical) The difference in the number of rows in \code{tibble1} compared to \code{tibble2}.}
#'   \item{additionalRowsInSecond}{(If not identical) The difference in the number of rows in \code{tibble2} compared to \code{tibble1}.}
#'   \item{presentInFirstNotSecond}{(If not identical) The rows present in \code{tibble1} but not in \code{tibble2}.}
#'   \item{presentInSecondNotFirst}{(If not identical) The rows present in \code{tibble2} but not in \code{tibble1}.}
#'
#' @details
#' The function works as follows:
#' \enumerate{
#'   \item It extracts and sorts the column names from both tibbles.
#'   \item It identifies additional columns in either tibble and stores them in the result.
#'   \item If the sets of columns differ, it returns immediately after marking the tibbles as not identical.
#'   \item If the columns are identical, it sorts the rows of both tibbles using \code{do.call(order, tibble)},
#'         then compares the sorted tibbles row-wise.
#'   \item If the tibbles are not identical, it calculates the differences in row counts and identifies the rows
#'         that are present in one tibble but not in the other.
#' }
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Create example tibbles with identical columns in different orders
#' tib1 <- tibble(x = c(1, 2, 3), y = c("a", "b", "c"))
#' tib2 <- tibble(y = c("a", "b", "c"), x = c(1, 2, 3))
#'
#' # Compare tibbles (should be identical)
#' compareTibbles(tib1, tib2)
#'
#' # Modify tib2 by adding an extra row
#' tib2 <- tib2 |> dplyr::add_row(x = 4, y = "d")
#'
#' # Compare again (differences in rows will be reported)
#' compareTibbles(tib1, tib2)
#' }
#'
compareTibbles <- function(tibble1, tibble2) {
  # Initialize result list
  result <- list()

  # Check if columns are identical (ignoring order)
  columns1 <- sort(names(tibble1))
  columns2 <- sort(names(tibble2))

  # Find additional columns
  additionalColumns1 <- dplyr::setdiff(columns1, columns2)
  additionalColumns2 <- dplyr::setdiff(columns2, columns1)

  result$additionalColumnsInFirst <- additionalColumns1
  result$additionalColumnsInSecond <- additionalColumns2

  # If columns are not identical, return result
  if (!identical(columns1, columns2)) {
    result$identical <- FALSE
    message("The two tibbles have different columns")
    return(result)
  }

  # Sort columns
  tibble1 <- tibble1[, columns1]
  tibble2 <- tibble2[, columns2]

  # Sort rows
  tibble1 <- tibble1[do.call(order, tibble1), ]
  tibble2 <- tibble2[do.call(order, tibble2), ]

  # Compare rows
  identicalRows <- identical(tibble1, tibble2)
  result$identical <- identicalRows

  if (identicalRows) {
    return(result)
  }

  # Find additional rows
  additionalRows1 <- nrow(tibble1) - nrow(tibble2)
  additionalRows2 <- nrow(tibble2) - nrow(tibble1)

  result$additionalRowsInFirst <- additionalRows1
  result$additionalRowsInSecond <- additionalRows2

  # Find rows present in first but not in second
  diffRows1 <- dplyr::setdiff(tibble1, tibble2)

  # Find rows present in second but not in first
  diffRows2 <- dplyr::setdiff(tibble2, tibble1)

  result$presentInFirstNotSecond <- diffRows1
  result$presentInSecondNotFirst <- diffRows2

  return(result)
}
