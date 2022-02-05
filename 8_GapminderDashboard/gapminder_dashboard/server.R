# server.R for a Gapminder-style dashboard of fertitlity and life expectancy
# Author: Mileva Van Tuyl
# Version 1.0, 1/29/22

library(shiny)
library(shinydashboard)

library(readr)
library(tidyr)
library(plotly)

# df <- read_csv('./data/WorldBankData.csv')

shinyServer(function(input, output, session) {
  df <- reactiveFileReader(
    intervalMillis = 30000,
    session = session,
    filePath = './data/WorldBankData.csv',
    readFunc = read_csv
  )
  
  # updateSelectInput(session, "regionselector", choices = unique(df$Region))
  
  # "Region" dropdown UI component
  output$regionselector <- renderUI({
    regions <- unique(df()$Region)
    selectInput(
      "regionselector",
      "Select a Region",
      choices = c("All Regions", sort(regions))
    )
  })
  
  # "Year" slider UI component
  output$yearselector <- renderUI({
    sliderInput("yearselector", 
      "Year", 
      min = min(df()$Year), 
      max = max(df()$Year), 
      value = 2000, 
      step = 1, 
      sep = '', 
      animate = animationOptions(interval = 1000)
    )
  })
  
  # "Country" dropdown UI component based on the selected "Region" 
  observeEvent(list(input$regionselector), {
    # Debugging, input region
    print(input$regionselector)

    # Render dropdown for "Country"
    output$countryselector <- renderUI({

      # Obtain a list of countries based on "Region" selector
      if (input$regionselector == "All Regions"){
        countries <-
          df() %>%
          select(Country) %>%
          distinct() %>%
          arrange(Country)
      } else {
        countries <-
          df() %>%
          filter(Region == input$regionselector) %>%
          select(Country) %>%
          distinct() %>%
          arrange(Country)
      }

      selectInput(
        "countryselector",
        "Select a Country",
        choices = countries
      )
      
      # # Render the "Country" dropdown list
      # if (input$regionselector == "All Regions") {
      #   df_filtered <- df()
      # } else {
      #   df_filtered <- df() %>% filter(Region == input$regionselector)
      # }
      # 
      # countries <- df_filtered %>%
      #   select(Country) %>%
      #   distinct() %>%
      #   arrange(Country)
      # 
      # output$countryselector <- renderUI({
      #   selectInput(
      #     "countryselector", 
      #     "Select a Country", 
      #     choices = countries
      #   )
      # })
      
    })
  })
  
  observeEvent(list(input$regionselector, input$yearselector), {
  output$scatterplot <- renderPlotly({
    
    # Selected year
    year <- input$yearselector
    print(year)
    
    # Processing data
    dataplot <- df() %>% 
      filter(Year == year) %>% 
      filter(Region == input$regionselector)
    
    # create plotly plot
    scatterplot <- plot_ly(data = dataplot,
      type = 'scatter', 
      mode = 'markers',
      x = ~Fertility, 
      y = ~LifeExpectancy, 
      color = ~Region, 
      size = ~Population, 
      text = paste0("<b>Country:</b> ", dataplot$Country, "<br>", 
        "<b>Region:</b> ", dataplot$Region, "<br>", 
        "<b>Fertility:</b> ", dataplot$Fertility, "<br>", 
        "<b>Life Expectancy:</b> ", dataplot$LifeExpectancy, "<br>", 
        "<b>Population:</b> ", dataplot$Population, "<br>"), 
      hoverinfo = 'text') %>%
      layout(
        title = list(text = paste0("Life Expectancy vs. Fertility in ", year), xref = "paper"),
        xaxis = list(title = "Fertility \n (Births per Women)"), 
        yaxis = list(title = "Life Expectancy (Years)"), 
        legend = list(title = list(text = "Region"))) %>%
      config(displayModeBar = F)
  })})
  
  # modify this based on user selections
  output$mydata <- renderTable({df()})
  
})
