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

#' Format a Decimal Number with Commas and Fixed Decimal Places
#'
#' This function formats a decimal number by splitting it into an integer part and a decimal part,
#' formatting the integer part with commas as thousand separators, and formatting the decimal part to a fixed
#' number of digits. The decimal part can either be rounded or truncated based on the \code{round} parameter.
#'
#' @param number A numeric value to be formatted.
#' @param decimalPlaces An integer specifying the number of digits to display after the decimal point. Defaults to \code{1}.
#' @param round A logical value indicating whether the decimal portion should be rounded. If \code{FALSE},
#'   the decimal part will be truncated instead. Defaults to \code{TRUE}.
#'
#' @return A character string representing the formatted number with commas as thousand separators for the integer part
#'   and a period separating the integer and decimal parts.
#'
#' @details
#' The function splits the number into its integer and decimal components. The integer part is formatted using
#' \code{formatC} with commas inserted as thousand separators. The decimal part is processed either by rounding or truncation,
#' then converted to a string with fixed decimal places. Finally, the two parts are concatenated with a period separator.
#'
#' @examples
#' formatDecimalWithComma(1234567.8912)
#' # Might return "1,234,567.9"
#'
#' formatDecimalWithComma(1234567.8912, decimalPlaces = 2, round = FALSE)
#' # Might return "1,234,567.89"
#'
formatDecimalWithComma <- function(number,
                                   decimalPlaces = 1,
                                   round = TRUE) {
  integerPart <- floor(number)
  decimalPart <- number - integerPart

  if (round) {
    decimalPart <- round(decimalPart, decimalPlaces)
  } else {
    decimalPart <- trunc(decimalPart * 10^decimalPlaces) / 10^decimalPlaces
  }

  formattedIntegerPart <- formatC(integerPart, format = "d", big.mark = ",")
  decimalPartAsString <- formatC(decimalPart, format = "f", digits = decimalPlaces)
  formattedDecimalPart <- substr(decimalPartAsString, 3, nchar(decimalPartAsString))

  return(paste(formattedIntegerPart, formattedDecimalPart, sep = "."))
}
