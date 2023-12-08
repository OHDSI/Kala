library(shiny)
library(shinydashboard)
library(DT)

addInfo <- function(item, infoId) {
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button", 
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

dashboardPage(
  dashboardHeader(title = "Kala - Time series"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                if (exists("timeSeries")) addInfo(menuItem("Time Series", tabName = "timeSeries"), "timeSeriesInfo"),
                menuItem("Database information", tabName = "databaseInformation"), 
                conditionalPanel(condition = "input.tabs!='incidenceRate'",
                                 selectInput("database", "Database", database$databaseId, selectize = FALSE)
                )
    )
  ),
  dashboardBody(
      tabItem(tabName = "timeSeries",
              box(
                title = "Time Series", width = NULL, status = "primary",
                tags$table(style = "width: 100%",
                           tags$tr(
                  tags$td(valign = "bottom",
                          checkboxGroupInput(inputId = "irStratification", 
                                             label = "Stratify by",
                                             choices = c("Age", "Gender", "Calendar Year"),
                                             selected = c("Age", "Gender", "Calendar Year"),
                                             inline = TRUE)
                  ),
                  tags$td(HTML("&nbsp;&nbsp;&nbsp;&nbsp;")),
                  tags$td(valign = "bottom", style = "text-align: right",
                          checkboxInput("irYscaleFixed", "Use same y-scale across databases")
                  )
                )),
                htmlOutput("hoverInfoIr"),
                plotOutput("incidenceRatePlot", height = 700, hover = hoverOpts("plotHoverIr", delay = 100, delayType = "debounce"))
              )
    )
  )
)
