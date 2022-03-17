library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(data.table)
library(readr)

dashboardPage(
    dashboardHeader(title = 'US Covid Tracker'), 
    dashboardSidebar(
        # dashboard components
        radioButtons('outcome', 
                    'Outcome of Interest', 
                    choices = c('cases', 'deaths'), 
                    selected = 'cases',
                    inline = TRUE),
        selectInput('state', 
                    'State of Interest', 
                    choices = NULL, 
                    selected = 'All States' 
                    ),
        collapsed = TRUE
    ), 
    dashboardBody(
        # Top row
        fluidRow(
            # First box with plot of cases or deaths over time
            tabBox(
                width = 12, 
                id = "tabPlots",
                tabPanel("Cases", plotlyOutput("casesPlot")), 
                tabPanel("Deaths", plotlyOutput("deathsPlot")))
            # box(width = 6,
            #     status = "warning",
            #     height = "600",
            #     title = "mydata",
            #     solidHeader = TRUE,
            #     footer = "Data from The New York Times, based on reports from state and local health agencies.",
            #     column(width = 12, DT::dataTableOutput("mydata"),style = "height:495px; overflow-y: scroll;overflow-x: scroll;")
            # )
        ),
        fluidRow(
            valueBoxOutput(width = 4, "numDays"), # Can only display a value and text
            infoBoxOutput(width = 4, "ncol"), 
            infoBoxOutput(width = 4, "nrecords")
        )
    )
)
