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
#' This function relies on two helper functions:
#' \itemize{
#'   \item \code{formatIntegerWithComma}: Formats a numeric count with commas as thousand separators.
#'   \item \code{formatPercent}: Formats a numeric percentage to a string representation with a specified number of digits.
#' }
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
#' This function formats a numeric value as an integer and inserts commas as thousand separators.
#' It utilizes \code{formatC} with \code{format = "d"} and \code{big.mark = ","} to produce a human-readable string.
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
  return(formatC(number, format = "d", big.mark = ","))
}


#' Format a Number as a Percentage String
#'
#' This function converts a numeric value into a percentage string by multiplying the value by 100,
#' formatting it with a specified number of digits, and appending a percent sign ("%").
#'
#' @param x A numeric value representing a proportion (e.g., 0.25 for 25%).
#' @param digits An integer specifying the number of digits after the decimal point. Defaults to \code{2}.
#' @param format A character string indicating the format to be used by \code{formatC}. Defaults to \code{"f"}.
#' @param ... Additional arguments passed to \code{formatC} for further customization.
#'
#' @return A character string representing the formatted percentage value with a trailing percent sign.
#'
#' @details
#' The function multiplies the input \code{x} by 100 to convert it to a percentage,
#' formats the resulting number using \code{formatC} with the specified \code{digits} and \code{format},
#' and finally appends a "%" sign to create a complete percentage string.
#'
#' @examples
#' formatPercent(0.1234)
#' # "12.34%"
#'
#' formatPercent(0.5, digits = 0)
#' # "50%"
#'
formatPercent <- function(x,
                          digits = 2,
                          format = "f",
                          ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
