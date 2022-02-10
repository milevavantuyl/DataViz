# ui.R for a Gapminder-style application of fertility and life expectancy
# Author: Mileva Van Tuyl
# Version 1.0, 1/29/22

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

navbarPage("GAPMINDER", 
    # "Plot tab"
    tabPanel("Plot", 
        sidebarLayout(
            sidebarPanel(
                # Sidebar panel with sidebar selectors 
                selectInput(
                    "regionselector",
                    "Select a Region",
                    choices = NULL),
                uiOutput("countryselector")
            ), 
            mainPanel(
                # Main panel with scatter plot 
                plotlyOutput("scatterplot")
            )
        )
    ),
    
    # "Data tab"
    tabPanel("Data", 
        sidebarPanel(
            # Sidebar panel with  selectors 
            selectInput(
                "regionselector2",
                "Select a Region",
                choices = NULL),
            uiOutput("countryselector2"),
            checkboxInput('displayyear', 
                'Filter by year?', 
                value = FALSE),
            conditionalPanel(
                condition = "input.displayyear == true", 
                uiOutput("yearselector2")
            ),
            checkboxGroupInput(
                "columnselector", 
                "Columns to Display", 
                choices = NULL
            )
        ), 
        mainPanel(
            # Main panel with data table 
            fluidRow(
                box(width = 12, 
                    title = "Data", 
                    solidHeader = TRUE, 
                    DTOutput("mydata")
                    )
            )
        )
    )
)