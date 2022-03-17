library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(data.table)
library(readr)

dashboardPage(
    dashboardHeader(title = 'COVID-19 Tracker'), 
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")), 
            menuItem("Data", tabName = "data", icon = icon("table"))
        ),
        # dashboard components
        # selectInput('state', 
        #             'State of Interest', 
        #             choices = NULL, 
        #             selected = 'All States' 
        #             ),
        collapsed = TRUE
    ), 
    dashboardBody(
        tabItems(
            # Dashboard Tab
            tabItem(tabName = "dashboard", 
                
                # Top row
                fluidRow(
                    infoBoxOutput(width = 4, "numDays"), # Can only display a value and text
                    infoBoxOutput(width = 4, "numCases"), 
                    infoBoxOutput(width = 4, "numDeaths"), 
                    
                ),
                fluidRow(
                    # First box with plot of cases or deaths over time
                    tabBox(
                        width = 12, 
                        id = "tabPlots",
                        tabPanel("Cases", plotlyOutput("casesPlot")), 
                        tabPanel("Deaths", plotlyOutput("deathsPlot")))
                ),

    
            ), 
            
            # Data Tab
            tabItem(tabName = "data", 
                fluidRow(
                    box(width = 12, 
                        status = "warning",
                        title = "Cases and Deaths",
                        solidHeader = TRUE,
                        footer = "Data from The New York Times, based on reports from state and local health agencies.",
                        column(width = 12, DT::dataTableOutput("mydata"),style = "height:495px; overflow-y: scroll;overflow-x: scroll;")
                    )
                )
            )
            
        )
    )
)
