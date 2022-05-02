library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(data.table)
library(readr)

shinyServer(function(input, output, session) {
    
    # Read NYTimes data 
    us_df <- reactiveFileReader(
        intervalMillis = 20000, 
        session = session, 
        filePath = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us.csv',
        readFunc = fread)
    
    # Data as a paginated data table
    output$mydata <- renderDataTable({us_df()}, 
        options = list(pageLength = 10, info = FALSE,
            lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")) ) )
    
    # Time series plot of COVID cases in the US
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
    
    # Time series plot of COVID deaths in the US
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
    
    # Info box: number of days since the first COVID case in the US
    output$numDays <- renderValueBox({
        firstCase <- as.IDate('2020-01-21')
        now <- as.IDate(Sys.Date())
        days <- now - firstCase
        
        infoBox (
            title = "days since first case", 
            value = days,
            icon = icon("calendar"), 
            color = 'purple'
        )
    })
    
    # Info box: total number of COVID cases in the US
    output$numCases <- renderInfoBox({
        totalCases <- sum(us_df()$cases)
        formatted_value <- format(round(totalCases,0), big.mark=",") # in thousands
        
        infoBox (
            title = "Total Cases", 
            value = formatted_value,
            icon = icon("head-side-cough"), 
            color = "orange"
        )
    })
    
    # Info box: total number of COVID deaths in the US
    output$numDeaths <- renderInfoBox({
        totalDeaths <- sum(us_df()$deaths)
        formatted_value <- format(round(totalDeaths,0), big.mark=",") # in thousands
        
        infoBox (
            title = "Total Deaths",
            value = formatted_value,
            icon = icon("virus"), 
            color = "red"
        )
    })
})
