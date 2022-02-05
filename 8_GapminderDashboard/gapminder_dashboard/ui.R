# ui.R for a Gapminder-style dashboard of fertitlity and life expectancy
# Author: Mileva Van Tuyl
# Version 1.0, 1/29/22

library(shiny)
library(shinydashboard)
library(plotly)

navbarPage("GAPMINDER", 
    # "Plot tab"
    tabPanel("Plot", 
        sidebarLayout(
            sidebarPanel(
                # Sidebar panel, insert sidebar selectors here
                uiOutput("regionselector"),
                uiOutput("countryselector")
                # uiOutput("yearselector")
            ), 
            mainPanel(
                # Main panel, insert scatter plot here
                plotlyOutput("scatterplot")
            )
        )
    ),
    
    # "Data tab"
    tabPanel("Data", 
        sidebarPanel(), 
        mainPanel(
            # Main panel, insert data table here
            fluidRow(
                box(width = 12, 
                    title = "Data", 
                    solidHeader = TRUE, 
                    footer = "Read File (non-reactive)", 
                    tableOutput("mydata")
                    )
            )
        )
    )
)