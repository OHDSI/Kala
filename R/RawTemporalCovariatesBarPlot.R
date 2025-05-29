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

#' Create Bar Plot of temporal covariates
#'
#' This function draws bar plot of the raw temporal covariates returned by \code{getFeatureExtractionReportByTimeWindows} function call,
#' or \code{raw} part of \code{getFeatureExtractionReportByTimeWindows} with possible modifications.
#' It orders covariates by timeId and returns resulting bar plot object
#'@param covariateData A data frame or list containing temporal covariates. If a list, it must include a `raw` element
#'  that is a data frame. The data frame must include the following columns:
#'  - `covariateId` (numeric): Unique identifier for the covariate.
#'  - `periodName` (character): Name of each time period.
#'  - `timeId` (numeric): A numeric identifier for the time period.
#'  - `<covariateName>` and `<covariateValueToPlot>` data, can have different names, which could be provided as function arguments.
#' @param covariateId A vector of covariate ids, that will be plotted. If `NULL`, all available covariates in the object will be plotted.
#'   Default: `NULL`.
#' @param covariateName The name of the covariate column in the \code{covariateData} object. Used to group covariates and draw the color legend 
#'   Default: `"covariateName"`.
#' @param covariateValueToPlot The name of the value column to be drawn on the y-axis
#'   Default: `"sumValue"`.
#' @param barLabel The name of the column with text information that can be depicted next to each bar.
#'   Default: `"report"`.
#' @param barPosition Bar position adjustment. A string naming the position adjustment. To give the position as a string, 
#'   strip the function name of the ggplot2::position_ prefix. For example, to use position_jitter(), give the position as "jitter".
#'   Default: `"stack"`.
#' @param xAxisLabel The name of x axis. 
#'   Default: `"Time Window"`.
#' @param yAxisLabel The name of y axis.
#'   Default: `NULL` (will fallback to `covariateValueToPlot` name).
#' @param plotTitle The title of the plot
#'   Default: `NULL` (no title).
#'
#' @returns ggplot object
#' 
#' @examples
#' \dontrun{
#' # Get time windows report
#' timeWindowsFeatureExtractionReport <- Kala::getFeatureExtractionReportByTimeWindows(
#'   covariateData = covariateResultsAggregated,
#'   includedCovariateIds = includedCovariateIds,
#'   cohortId = cohortId
#' )
#' # plot temporal covariates
#' rawTemporalCovariatesBarPlot(timeWindowsFeatureExtractionReport)
#' 
#' 
#' # Filter and mutate data before plotting
#' covDataForPlot <- timeWindowsFeatureExtractionReport$raw |>
#'  dplyr::mutate(pct = scales::percent(x = averageValue, accuracy = 0.1)) |>
#'  dplyr::filter(timeId %in% c(61:85))                                            
#' }
#' # plot temporal covariates
#' rawTemporalCovariatesBarPlot(covDataForPlot, barLabel = "pct")
#' 
#' # provide different `barPosition`
#' rawTemporalCovariatesBarPlot(covDataForPlot, barLabel = "pct", barPosition = "dodge")
#' 
#' 
#' @export
rawTemporalCovariatesBarPlot <- function(covariateData,
                                         covariateIds = NULL,
                                         covariateName = "covariateName",
                                         covariateValueToPlot = "sumValue",
                                         barLabel = "report",
                                         barPosition = "stack",
                                         xAxisLabel = "Time Window",
                                         yAxisLabel = NULL,
                                         plotTitle = NULL){
  if (inherits(covariateData, "list")){
    if("raw" %in% names(covariateData)){
      covariateData <- covariateData$raw
    } else {
      stop("Provided covariateData list object doesn't contain `raw` item")
    }
  }
  if(!inherits(covariateData, "data.frame")){
    stop("Unrecognized `covariateData` object format. 
  Make sure that its format complies with `getFeatureExtractionReportByTimeWindows` function output.")
  }
  
  dataColNames <- names(covariateData)
  requiredColNames <- c(covariateName, "periodName", covariateValueToPlot, "timeId")
  if (!is.null(covariateIds)) requiredColNames <- c("covariateId", requiredColNames)
  if(!is.null(barLabel)) requiredColNames <- c(requiredColNames, barLabel)
  if (!all(requiredColNames %in% dataColNames)) {
    stop(paste0("Missing required columns: ", paste(setdiff(requiredColNames, dataColNames), collapse = ", ")))
  }
  
  if(!is.null(covariateIds)){
    covariateData <- dplyr::filter(covariateData, covariateId %in% covariateIds)
  }
  if (nrow(covariateData) == 0) {
    stop("No data to plot. Check your `covariateIds` or `covariateData` input.")
  }
  
  if(barPosition == "stack"){
    covariateData <- covariateData |>
      dplyr::group_by(periodName) |>                                                  
      dplyr::arrange(periodName, desc(!!rlang::sym(covariateName))) |>
      dplyr::mutate(y_pos = cumsum(!!rlang::sym(covariateValueToPlot))- 0.5 * !!rlang::sym(covariateValueToPlot)) |>
      dplyr::ungroup()
  }
  
  covariateData$periodName <- reorder(covariateData$periodName, covariateData$timeId)
  plt <- ggplot2::ggplot(covariateData, 
                         ggplot2::aes(
                           x = periodName, 
                           y = !!rlang::sym(covariateValueToPlot),
                           fill = !!rlang::sym(covariateName)
                         )
  ) +
    ggplot2::geom_bar(stat = "identity", position = barPosition) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "bottom")
  
  if(!is.null(barLabel))  {
    if(barPosition == "stack"){
      plt <- plt + ggplot2::geom_text(ggplot2::aes(y = y_pos,
                                                   label = !!rlang::sym(barLabel)), vjust = -0.5)
    } else {
      plt <- plt + ggplot2::geom_text(ggplot2::aes(label = !!rlang::sym(barLabel)), vjust = -0.5)
    }
  }
  
  if(is.null(yAxisLabel)){yAxisLabel <- covariateValueToPlot}
  plt <- plt + ggplot2::labs(x = xAxisLabel, y = yAxisLabel, title = plotTitle)
  plt
}
