library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(data.table)
library(readr)

# HIGH: 3 info boxes with statistics (days since first case, total cases, total deaths, cases/ deaths in past 24hrs)
# MEDIUM: Add a second tab with the data
# MEDIUM-LOW: Add a third tab (should really be the second) with vaccination data. Percent vaccinated. Efficacy. And info boxes.
# LOW: Change results based on state selected

shinyServer(function(input, output, session) {
    
    us_df <- reactiveFileReader(
        intervalMillis = 20000, 
        session = session, 
        filePath = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us.csv',
        # filePath = './us.csv',
        readFunc = fread)
        
    output$mydata <- renderDataTable({us_df()}, 
        options = list(paging = FALSE))
    
    output$casesPlot <- renderPlotly({
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
            name = "7-Day Avg")
        
        p <- p %>%
            layout(xaxis = list(title = 'Date',
                zerolinecolor = 'black',
                zerolinewidth = 2),
                yaxis = list(title = 'Cases'),
                title = 'New Reported Cases')
        
        p <- p %>%
            layout(hovermode="x unified", 
                    yaxis = list(rangemode = 'nonnegative'))
        
        return(p)
    })
    
    output$deathsPlot <- renderPlotly({
        
    dataplot <- us_df()
        
    p <- plot_ly(data = dataplot, 
        type = 'bar',
        marker = list(color = 'darkred', line = list(color = 'white', width = 0.2)),
        opacity = 0.6, 
        x = ~date, 
        y = ~deaths, 
        name = "Daily Deaths")
    
    p <- p %>% add_trace(
        x = ~date, 
        y = ~deaths_avg,
        type = 'scatter', 
        mode = 'lines',
        opacity = 1.0, 
        line = list(color = 'darkred'),
        marker = list(color = 'darkred', opacity = 0), 
        name = "7-Day Avg")
    
    p <- p %>%
        layout(xaxis = list(title = 'Date',
            zerolinecolor = 'black',
            zerolinewidth = 2),
            yaxis = list(title = 'Deaths'),
            title = 'New Reported Deaths')
    
    p <- p %>%
        layout(hovermode="x unified", 
               yaxis = list(rangemode = 'nonnegative'))
    
    return(p)
    })
    
    output$numDays <- renderValueBox({
        
        firstCase <- as.IDate('2020-01-21')
        now <- as.IDate(Sys.Date())
        days <- now - firstCase
    
        valueBox (
            value = days, 
            subtitle = paste("days since first case in the U.S."), 
            icon = icon("calendar")
        )
    })
    
})
