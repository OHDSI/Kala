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

#' Format Count and Percentage as a Single String
#'
#' This function formats a count and a percentage value into a single string. The count is formatted
#' with commas using \code{formatIntegerWithComma}, and the percentage is formatted using \code{formatPercent}
#' with a specified number of decimal digits. The resulting string is in the format: "formattedCount (formattedPercent)".
#'
#' @param count A numeric value representing the count. It is formatted using \code{formatIntegerWithComma}.
#' @param percent A numeric value representing the percentage. It is formatted using \code{formatPercent}.
#' @param percentDigits An integer specifying the number of digits to display for the percentage. Defaults to \code{1}.
#'
#' @return A character string combining the formatted count and percentage in the format:
#'   \code{"formattedCount (formattedPercent)"}.
#'
#' @details
#' The function concatenates the outputs of these helpers with additional formatting.
#'
#' @examples
#' \dontrun{
#' # Example assuming the helper functions are defined:
#' result <- formatCountPercent(123456, 0.789, percentDigits = 2)
#' # Might return "123,456 (78.90%)"
#' }
#'
formatCountPercent <- function(count, percent, percentDigits = 1) {
  return(paste0(
    formatIntegerWithComma(count),
    " (",
    formatPercent(percent, digits = percentDigits),
    ")"
  ))
}



#' Format an Integer with Comma Separators
#'
#' This function formats a numeric value by first truncating any fractional part.
#' If the input number is a whole number (i.e., it has no fractional part), the function
#' inserts commas as thousand separators using \code{formatC} with \code{format = "d"} and \code{big.mark = ","}.
#' For non-whole numbers, only the integer portion is returned as a character string without commas.
#'
#' @param number A numeric value to be formatted as an integer.
#'
#' @return A character string representing the formatted integer with commas.
#'
#' @examples
#' formatIntegerWithComma(1234567)
#' # [1] "1,234,567"
#'
formatIntegerWithComma <- function(number) {
  # Truncate the number to its integer part:
  truncated <- as.integer(number)
  # Check which numbers are whole (no fractional part)
  isWhole <- (number %% 1 == 0)
  # For whole numbers, format with commas; for non-whole numbers, return as character without commas
  result <- ifelse(isWhole, formatC(truncated, format = "d", big.mark = ","), as.character(truncated))
  return(result)
}


formatPercent <- function(x,
                          digits = 2,
                          format = "f",
                          ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
