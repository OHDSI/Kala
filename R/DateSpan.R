# Copyright 2020 Observational Health Data Sciences and Informatics
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

#' @title
#' Generates non overlapping date spans from date span.
#' 
#' @description
#' This function takes an input data frame object with two date columns 
#' corresponding to startDate and endDate, where endDate >= startDate. 
#' It outputs a dataframe with non overlapping startDate and endDate - both dates
#' inclusive. Optionally, the input and output data frame may be grouped by a group
#' variable.
#' 
#' @param x             A dataframe with atleast two date columns called startDate, endDate.
#' @param startDate     A date column in the dataframe x, that refers to the startDate. Default = 'startDate'
#' @param endDate       A date column in the dataframe x, that refers to the endDate. Default = 'endDate'
#' @param gap           (Optional) Default = 1.
#' @param group         (optional) if there is a grouping varible.
#' @return              A tibble with two date columns startDate, endDate. if a group variable
#'                      is provided, it will be part of returned object.
#' @examples
#' \dontrun{
#' #If your data is in database, provide connection.
#' connection <- collapseDateSpan(x = dataFrame,
#'                                startDate = start,
#'                                endDate = end)
#' }
#' @export
collapseDateSpan <- function(x,
                             startDate = 'startDate',
                             endDate = 'endDate',
                             gap = 1,
                             group = NULL) {
    
    errorMessage <- checkmate::makeAssertCollection()
    checkmate::assertDataFrame(x, any.missing = FALSE, min.cols = 2, min.rows = 1, add = errorMessage)
    x %>% dplyr::select(!!rlang::sym(startDate)) %>% dplyr::pull() %>% checkmate::assertDate(add = errorMessage)
    x %>% dplyr::select(!!rlang::sym(endDate)) %>% dplyr::pull() %>% checkmate::assertDate(add = errorMessage)
    checkmate::assertInt(x = gap, lower = 0, add = errorMessage)
    checkmate::reportAssertions(errorMessage)
    
    if (is.null(group)) {
      result <- x %>%
        dplyr::select(!!rlang::sym(startDate),
                      !!rlang::sym(endDate)) %>%
        dplyr::filter(!!rlang::sym(endDate) >=
                        !!rlang::sym(startDate)) %>%
        dplyr::arrange(!!rlang::sym(startDate)) %>%
        dplyr::mutate(idTemp = c(0, cumsum(
          as.numeric(dplyr::lead(!!rlang::sym(startDate))) >
            cummax(as.numeric(!!rlang::sym(endDate)+gap))
        )[-dplyr::n()])) %>%
        dplyr::group_by(idTemp) %>%
        dplyr::summarise(startDate = min(!!rlang::sym(startDate)),
                         endDate = max(!!rlang::sym(endDate))) %>%
        dplyr::select(-idTemp) %>%
        dplyr::ungroup()
    } else {
      result <- x %>%
        dplyr::select(!!rlang::sym(group),
                      !!rlang::sym(startDate),
                      !!rlang::sym(endDate)) %>%
        dplyr::filter(!!rlang::sym(endDate) >= !!rlang::sym(startDate)) %>%
        dplyr::group_by(!!rlang::sym(group)) %>%
        dplyr::arrange(!!rlang::sym(startDate), .by_group = TRUE) %>%
        dplyr::mutate(idTemp = c(0, cumsum(
          as.numeric(dplyr::lead(!!rlang::sym(startDate)+gap)) >
            cummax(as.numeric(!!rlang::sym(endDate)))
        )[-dplyr::n()])) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!rlang::sym(group), idTemp) %>%
        dplyr::summarise(startDate = min(!!rlang::sym(startDate)),
                         endDate = max(!!rlang::sym(endDate))) %>%
        dplyr::select(-idTemp) %>%
        dplyr::ungroup()
    }
  return(result)
}





#' @title
#' Generates a vector of dates from date span
#' 
#' @description
#' This function takes an input a data frame object with atleast two date columns 
#' corresponding to startDate and endDate, where endDate >= startDate. 
#' It outputs a vector of unique dates.
#' 
#' @param x             A dataframe with atleast two date columns called startDate, endDate.
#' @param startDate     A date column in the dataframe x, that refers to the startDate. Default = 'startDate'
#' @param endDate       A date column in the dataframe x, that refers to the endDate. Default = 'endDate'
#' 
#' @return              A vector of dates.
#' @examples
#' \dontrun{
#' connection <- convertDateSpanToDateVector(x = dataFrame)
#' }
#' @export
convertDateSpanToDateVector <- function(x,
                                        startDate = 'startDate', 
                                        endDate = 'endDate') {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(x, any.missing = FALSE, min.rows = 1, min.cols = 2, 
                             add = errorMessage)
  x %>% dplyr::select(!!rlang::sym(startDate)) %>% dplyr::pull() %>% checkmate::assertDate(add = errorMessage)
  x %>% dplyr::select(!!rlang::sym(endDate)) %>% dplyr::pull() %>% checkmate::assertDate(add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  result <- x %>%
    dplyr::distinct() %>%
    dplyr::mutate(dates = purrr::map2(
      .x = !!rlang::sym(startDate),
      .y = !!rlang::sym(endDate),
      .f = seq.Date,
      by = "day"
    )) %>%
    dplyr::select(dates) %>%
    tidyr::unnest(cols = c(dates)) %>%
    dplyr::distinct() %>%
    dplyr::arrange() %>%
    dplyr::select(dates) %>%
    dplyr::pull()
  return(result)
}



#' @title
#' Converts a vector of dates into date spans
#' 
#' @description
#' This function takes an input vector of dates and outputs a data frame (tibble) with
#' date spans represented by startDate and endDate - both dates inclusive. Optionally, 
#' the date spans maybe rounded using the parameters and conventions in \code{lubridate::floor_date}.
#' 
#' @param x             A vector of dates.
#' @param unit          (Optional) A character string specifying a calendar unit such as 
#'                      day, week, month, quarter and year. Options are from 
#'                      \code{lubridate::floor_date}.
#' @param week_start    (Optional) when unit is weeks, specify the reference day. 7 (default) 
#'                      represents Sunday and 1 represents Monday. Options from 
#'                      \code{lubridate::floor_date}.
#' @return              A tibble with two date columns startDate, endDate. 
#' @examples
#' \dontrun{
#' connection <- convertDateVectorToDateSpan(x = dataFrame,
#'                                           date = date)
#' }
#' @export
convertDateVectorToDateSpan <- function(x,
                                        unit = NULL,
                                        week_start = NULL) {
    errorMessage <- checkmate::makeAssertCollection()
    checkmate::assertVector(x, any.missing = FALSE, add = errorMessage)
    checkmate::assertDate(x, add = errorMessage)
    
    if (any(!is.null(unit), !is.null(week_start))) {
        checkmate::assertScalar(unit, add = errorMessage)
        checkmate::assertCharacter(unit, len = 1, add = errorMessage)
        if (!is.null(week_start)) {
          checkmate::assertInt(week_start, add = errorMessage)
          checkmate::assertChoice(x = week_start, choices = c(1,7), add = errorMessage)
        }
    }
    checkmate::reportAssertions(errorMessage)
 
    result <- tidyr::tibble(date = x) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(lagDifference = c(1, diff(date))) %>%
      dplyr::mutate(interrupted = cumsum(lagDifference != 1)) %>%
      dplyr::group_by(interrupted) %>%
      dplyr::summarise(startDate = min(date),
                       endDate = max(date)
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::select(-interrupted)
    
    if (!is.null(unit)) {
      calendarSpan <- tidyr::tibble(date = x) %>%
        dplyr::distinct() %>% 
        dplyr::mutate(floorDate = lubridate::floor_date(x = date,
                                                        unit = unit,
                                                        week_start = week_start
                                                       ),
                      ceilingDate = lubridate::ceiling_date(x = date,
                                                            unit = unit,
                                                            week_start = week_start) - 1
                      ) %>% 
        dplyr::select(floorDate, ceilingDate) %>% 
        dplyr::distinct()
      
      result <- tidyr::crossing(result, calendarSpan) %>% 
        dplyr::filter(floorDate <= endDate &
                       ceilingDate >= startDate
                      ) %>% 
        dplyr::mutate(start = dplyr::case_when(floorDate < startDate ~ startDate,
                                               TRUE ~ floorDate
        ),
        end = dplyr::case_when(ceilingDate > endDate ~ endDate,
                               TRUE ~ ceilingDate)
        ) %>% 
        dplyr::select(start, end) %>% 
        dplyr::rename(startDate = start, endDate = end)
    }
    return(result)
}

