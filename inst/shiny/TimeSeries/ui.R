library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(tsibble)

ui = fluidPage(
    dateRangeInput("dr", label = "f", start = today() - 60, end = today()),
    plotlyOutput("plot")
)