library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(data.table)
library(readr)
library(tidyverse)
library(covidcast)
library(lubridate)

dashboardPage(
    dashboardHeader(title = 'COVID-19 Tracker, USA'), 
    
    # Sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")), 
            menuItem("Map", tabName = "map", icon = icon("map")), 
            menuItem("Data", tabName = "data", icon = icon("table"))
        ),
        collapsed = TRUE
    ), 
    
    # Body
    dashboardBody(
        tabItems(
            # Dashboard Tab
            tabItem(tabName = "dashboard", 
                
                # Top row - InfoBoxes
                fluidRow(
                    infoBoxOutput(width = 4, "numDays"), 
                    infoBoxOutput(width = 4, "numCases"), 
                    infoBoxOutput(width = 4, "numDeaths"), 
                    
                ),
                
                # Second row - plot of cases and deaths over time
                fluidRow(
                    tabBox(
                        width = 12, 
                        id = "tabPlots",
                        tabPanel("Cases", plotlyOutput("casesPlot")), 
                        tabPanel("Deaths", plotlyOutput("deathsPlot")))
                ),
            ), 
            
            # Map Tab
            tabItem(tabName = "map", 
                # First row map plots
                fluidRow(
                    tabBox(
                        width = 12,
                        id = "mapPlots",
                        tabPanel("Cases", plotlyOutput("casesMap")),
                        tabPanel("Death", plotlyOutput("deathsMap")))
                ), 
                
            ), 

            # Data Tab
            tabItem(tabName = "data", 
                fluidRow(
                    box(width = 12, 
                        status = "warning",
                        title = "Cases and Deaths in the US",
                        solidHeader = TRUE,
                        footer = "Data from The New York Times, based on reports from state and local health agencies.",
                        column(width = 12, 
                            DT::dataTableOutput("mydata"),
                            style = "height:495px; overflow-y: scroll;overflow-x: scroll;")
                    )
                ), 
                
                # Second row - datatable
                fluidRow(
                    box(width = 12, 
                        status = "warning",
                        title = "Cases and Deaths by State",
                        solidHeader = TRUE,
                        footer = "Data from The New York Times, based on reports from state and local health agencies.",
                        column(width = 12, 
                            DT::dataTableOutput("prepareddata"),
                            style = "height:495px; overflow-y: scroll;overflow-x: scroll;")
                    )
                ),
            )
        )
    )
)
