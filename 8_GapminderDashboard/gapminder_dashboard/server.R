# server.R for a Gapminder-style app of fertility and life expectancy
# Author: Mileva Van Tuyl
# Version 1.0, 1/29/22

library(shiny)
library(shinydashboard)

library(readr)
library(tidyr)
library(plotly)
library(data.table)
library(DT)

# Read data
df <- fread('./data/WorldBankData.csv')

shinyServer(function(input, output, session) {
  
  ## Update UI components (for Plot and Data tabs)
  updateSelectInput(session, 
    'regionselector', 
    choices = c("All Regions", sort(unique(df$Region))))
  
  updateSelectInput(session, 
    'regionselector2', 
    choices = c("All Regions", sort(unique(df$Region))))
  
  updateCheckboxGroupInput(session, 
    'columnselector', 
    choices = names(df), 
    selected = names(df))
  
  
  ## Create "Country" UI component based on the selected "Region" for the plot tab
  observeEvent(list(input$regionselector), {

    # Render dropdown for "Country"
    output$countryselector <- renderUI({

      # When region = null (occurs at outset), countryselector is not created
      # When region != null, generate the "country" dropdown
      if (!is.null(input$regionselector)) {
  
        # All Regions is selected: display a list of all countries 
        if (input$regionselector == "All Regions"){
          countries <-
            df %>%
            select(Country) %>%
            distinct() %>%
            arrange(Country) # Alphabetical order
        } else {
          # A specific region is selected: displays the countries in that region
          countries <-
            df %>%
            filter(Region == input$regionselector) %>%
            select(Country) %>%
            distinct() %>%
            arrange(Country) # Alphabetical order
        }

        # Populate "country" dropdown UI component
        selectInput(
          "countryselector",
          "Select a Country",
          choices = c('All Countries in Region', countries)
        )
      }
    })
  })
  
  ## Scatter plot
  observeEvent(list(input$regionselector, input$countryselector), {
    output$scatterplot <- renderPlotly({
      
      # Data for all countries 
      dataplot <- df %>% drop_na()
      
      # Data for all countries in a given region
      if (!is.null(input$regionselector) && input$regionselector != "All Regions"){
        dataplot <- df %>% filter(Region == input$regionselector)
      }

      # Data for a single country 
      if (!is.null(input$countryselector) && input$countryselector != "All Countries in Region"){
        dataplot <- dataplot %>% filter(Country == input$countryselector)
      }

      # create base plot
      base <- plot_ly(data = dataplot,
        x = ~Fertility,
        y = ~LifeExpectancy,
        size = ~Population,
        color = ~Region, 
        opacity = 0.8,
        frame = ~Year,
        text = paste0("<b>Country:</b> ", dataplot$Country, "<br>",
          "<b>Region:</b> ", dataplot$Region, "<br>",
          "<b>Fertility:</b> ", dataplot$Fertility, "<br>",
          "<b>Life Expectancy:</b> ", dataplot$LifeExpectancy, "<br>",
          "<b>Population:</b> ", dataplot$Population, "<br>"),
        hoverinfo = 'text',
        type = 'scatter', 
        mode = 'markers'
        )
      # Add animations and labels to scatterplot
      scatterplot <- base %>%
          layout(
            title = list(text = "Life Expectancy vs. Fertility", xref = "paper"),
            xaxis = list(title = "Fertility \n (Births per Women)"),
            yaxis = list(title = "Life Expectancy (Years)"),
            legend = list(title = list(text = "Region")),
            showlegend = TRUE) %>%
          config(displayModeBar = F) %>%
          animation_opts(500, redraw = FALSE)

      return (scatterplot)
    })
  })
  
  
  ## Create "Country" dropdown UI component based on the selected "Region" 
  observeEvent(list(input$regionselector2), {
    
    # Render dropdown for "Country"
    output$countryselector2 <- renderUI({
      
      # When region = null (occurs at outset), countryselector is not created
      # When region != null, generate the "country" dropdown
      if (!is.null(input$regionselector2)) {
        
        # Obtain a list of countries based on "Region" selector
        if (input$regionselector2 == "All Regions"){
          countries <-
            df %>%
            select(Country) %>%
            distinct() %>%
            arrange(Country)
        } else {
          countries <-
            df %>%
            filter(Region == input$regionselector2) %>%
            select(Country) %>%
            distinct() %>%
            arrange(Country) # Alphabetical order
        }
        
        # Populate "country" dropdown UI component
        selectInput(
          "countryselector2",
          "Select a Country",
          choices = c('All Countries in Region', countries)
        )
      }
    })
    
  })
  
  # "Year" slider UI component
  output$yearselector2 <- renderUI({
    sliderInput("yearselector2",
      "Year",
      min = min(df$Year),
      max = max(df$Year),
      value = 2000,
      step = 1,
      sep = '',
    )
  })
  
  # Data table
  output$mydata <- renderDT({
    data <- as.data.frame(df)

    # Data for all countries in a given region
    if (!is.null(input$regionselector2) && input$regionselector2 != "All Regions"){
      data <- data %>% filter(Region == input$regionselector2)
    }
    
    # Data for a single country within the specified region
    if (!is.null(input$countryselector2) && input$countryselector2 != "All Countries in Region"){
      data <- data %>% filter(Country == input$countryselector2)
    }
    
    # Data for a single year
    if (input$displayyear && !is.null(input$yearselector2)){
      data <- data %>% filter(Year == input$yearselector2)
    }
    
    # Display selected columns
    datatable(data[, input$columnselector])
    })
})
