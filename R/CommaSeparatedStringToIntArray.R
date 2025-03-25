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

#' Convert a Comma-Separated String to a Numeric Vector
#'
#' This function takes a comma-separated string, splits it into individual elements,
#' removes any empty components, and converts the remaining elements into numeric values.
#'
#' @param inputString A character string containing numeric values separated by commas.
#'
#' @return A numeric vector with each element corresponding to a number extracted from the input string.
#'
#' @examples
#' commaSeparatedStringToIntArray("1,2,3,4")
#' # [1] 1 2 3 4
#' 
#' @export
commaSeparatedStringToIntArray <- function(inputString) {
  # Split the string into elements based on commas
  stringElements <- strsplit(inputString, ",")[[1]]
  # Remove empty elements
  stringElements <- stringElements[stringElements != ""]
  # Convert elements to numeric values
  integerArray <- as.double(stringElements)
  return(integerArray)
}
