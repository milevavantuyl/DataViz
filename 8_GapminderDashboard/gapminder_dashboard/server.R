# server.R for a Gapminder-style dashboard of fertitlity and life expectancy
# Author: Mileva Van Tuyl
# Version 1.0, 1/29/22

library(shiny)
library(shinydashboard)

library(readr)
library(tidyr)
library(plotly)
library(data.table)

df <- fread('./data/WorldBankData.csv')
shinyServer(function(input, output, session) {
  
  ## Populate "Region" dropdown UI component
  updateSelectInput(session, 'regionselector', choices = c("All Regions", sort(unique(df$Region))))
  
  ## Create "Country" dropdown UI component based on the selected "Region" 
  observeEvent(list(input$regionselector), {

    # Render dropdown for "Country"
    output$countryselector <- renderUI({

      # When region = null (occurs at outset), countryselector is not created
      # When region != null, generate the "country" dropdown
      if (!is.null(input$regionselector)) {
  
        # Obtain a list of countries based on "Region" selector
        if (input$regionselector == "All Regions"){
          countries <-
            df %>%
            select(Country) %>%
            distinct() %>%
            arrange(Country) # Alphabetical order
        } else {
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
  
  ## Create the scatterplot. It will update if regionselector or countryselector is modified
  observeEvent(list(input$regionselector, input$countryselector), {
    output$scatterplot <- renderPlotly({
      
      # Data for all countries 
      dataplot <- df %>% drop_na()
      
      # Data for all countries in a given region
      if (!is.null(input$regionselector) && input$regionselector != "All Regions"){
        dataplot <- df %>% filter(Region == input$regionselector)
      }

      # Data for a single country within the specified region
      if (!is.null(input$countryselector) && input$countryselector != "All Countries in Region"){
        dataplot <- dataplot %>% filter(Country == input$countryselector)
      }
      
      # create base plot
      base <- plot_ly(data = dataplot,
        x = ~Fertility,
        y = ~LifeExpectancy,
        size = ~Population,
        # color = ~df()$Region %>% unique(),
        color = ~Region,
        frame = ~Year,
        text = paste0("<b>Country:</b> ", dataplot$Country, "<br>",
          "<b>Region:</b> ", dataplot$Region, "<br>",
          "<b>Fertility:</b> ", dataplot$Fertility, "<br>",
          "<b>Life Expectancy:</b> ", dataplot$LifeExpectancy, "<br>",
          "<b>Population:</b> ", dataplot$Population, "<br>"),
        hoverinfo = 'text',
        type = 'scatter',
        mode = 'markers')
      
      # Add animations and labels to scatterplot 
      scatterplot <- base %>%
          layout(
            title = list(text = "Life Expectancy vs. Fertility", xref = "paper"),
            xaxis = list(title = "Fertility \n (Births per Women)"),
            yaxis = list(title = "Life Expectancy (Years)"),
            legend = list(title = list(text = "Region"))) %>%
          config(displayModeBar = F) %>%
          animation_opts(500, redraw = FALSE)
    })
  })
  
  # modify this based on user selections
  output$mydata <- renderTable({df})
  
})
