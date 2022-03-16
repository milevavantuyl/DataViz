library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(data.table)
library(readr)

# HIGH: 3 info boxes with statistics
# HIGH: To Do - Case vs. deaths on the server side
# MEDIUM: Change results based on state selected
# LOW: Add a second tab with the data

shinyServer(function(input, output, session) {

    # state_df <- reactiveFileReader(
    #     intervalMillis = 20000, 
    #     session = session, 
    #     filePath = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/live/us-states.csv', 
    #     readFunc = fread)
    
    us_df <- reactiveFileReader(
        intervalMillis = 20000, 
        session = session, 
        # filePath = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us.csv',
        filePath = './us.csv',
        readFunc = fread)
        
    output$mydata <- renderDataTable({us_df()}, 
        options = list(paging = FALSE))
    
    output$myplot <- renderPlotly({
        dataplot <- us_df()
        
        p <- plot_ly(data = dataplot, 
            type = 'bar',
            marker = list(color = 'darkcyan', line = list(color = 'white', width = 0.2)),
            opacity = 0.6, 
            x = ~date, 
            y = ~cases, 
            name = "Daily Cases")
        
        p <- p %>% add_trace(
            x = ~date, 
            y = ~cases_avg,
            type = 'scatter', 
            mode = 'lines',
            opacity = 1.0, 
            line = list(color = 'darkcyan'),
            marker = list(color = 'darkcyan', opacity = 0), 
            name = "7-Day Avg.")
        
        p <- p %>%
            layout(xaxis = list(title = 'Date',
                zerolinecolor = 'black',
                zerolinewidth = 2),
                yaxis = list(title = 'Cases'),
                title = 'Frequency of Cases')
        
        p <- p %>%
            layout(hovermode="x unified")
        
        return(p)
    })
    

    
    
    
    
    
    
    

})
