library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(data.table)
library(readr)
library(tidyverse)
library(covidcast)
library(lubridate)

# 2019 population data from census
state_pop_2019 <- state_census %>% 
    select(STATE, NAME, POPESTIMATE2019)

shinyServer(function(input, output, session) {
    
    # Read NYTimes US-level data 
    us_df <- reactiveFileReader(
        intervalMillis = 20000, 
        session = session, 
        filePath = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us.csv',
        readFunc = fread)
    
    # Read NYTimes state-level covid data
    state_covid <- reactiveFileReader(
        intervalMillis = 20000, 
        session = session, 
        filePath = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv',
        readFunc = fread)
    
    # Prepare state-level covid data, aggregating by month
    state_covid_prepared <- reactive({
        state_covid() %>%
            mutate(month_year = floor_date(as_date(date), "month")) %>%
            group_by(fips, month_year) %>% 
            summarise(monthly_cases=sum(cases), monthly_deaths = sum(deaths), .groups = "keep")
    })
    
    # All state level data for plotting
    state_data_prepared <- reactive({
        left_join(state_covid_prepared(), 
                state_pop_2019, 
                by = c("fips"="STATE"), 
                keep = FALSE) %>%
        mutate(monthly_cases_per10000= round(monthly_cases/POPESTIMATE2019 * 10000), 2) %>%
        mutate(monthly_deaths_per10000 = round(monthly_deaths/POPESTIMATE2019 * 10000), 2) %>%
        mutate(state.abbreviation = state.abb[match(NAME,state.name)]) %>%
        select(fips, NAME, state.abbreviation, month_year, monthly_cases_per10000, monthly_deaths_per10000)
    })
    
    # Render slider input from dates in data
    output$dateselector <- renderUI({
        sliderInput("dateselector", 
            "Month:", 
            min = min(state_data_prepared()$month_year), 
            max = max(state_data_prepared()$month_year), 
            value = max(state_data_prepared()$month_year), 
            timeFormat = "%b %Y")
    })
    
    # State level data as a paginated data table
    output$prepareddata <- renderDataTable({state_covid()}, 
        options = list(pageLength = 10, info = FALSE,
            lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")) ) )
    
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
                title = 'New Reported COVID-19 Cases')
        
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
                title = 'New Reported COVID-19 Deaths')
        
        p <- p %>%
            layout(hovermode="x unified", 
                yaxis = list(rangemode = 'nonnegative'))
        
        return(p)
    })
    
    # Map of Covid cases in the US 
    output$casesMap <- renderPlotly({
        
        # Gets user selected month
        # handles case when dateselector is null/ being populated
        if (is.na(input$dateselector)){
            selected_month <- strftime(floor_date(max(state_data_prepared()$month_year)))
        } else {
            selected_month <- strftime(floor_date(input$dateselector, "month"))
        }
        
        # Filter by given month
        df <- state_data_prepared() %>% 
            filter(month_year == selected_month)
        
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
        )
        
        # Create choropleth
        fig <- plot_geo(df, locationmode = 'USA-states')
        fig <- fig %>% add_trace(
            type="choropleth",
            text = paste0("<b>State:</b> ", df$NAME, "<br>",
                "<b>Monthly Cases (per 10000):</b> ", df$monthly_cases_per10000),
            hoverinfo = 'text',
            locations=df$state.abbreviation,
            z= df$monthly_cases_per10000,
            color = df$monthly_cases_per10000,
            colorscale="Blues",
            reversescale = TRUE,
            marker=list(line=list(
                color = 'black',
                width=0.5)
            )
        )
        fig <- fig %>% colorbar(title = "Cases per 10000")
        fig <- fig %>% layout(
            geo = g, 
            title = "Monthly COVID-19 Cases by State"
        ) 

        
        return(fig)
    })
    
    # Map of Covid deaths in the US 
    output$deathsMap <- renderPlotly({
        
        # Gets user selected month
        # handles case when dateselector is null/ being populated
        if (is.na(input$dateselector)){
            selected_month <- strftime(floor_date(max(state_data_prepared()$month_year)))
        } else {
            selected_month <- strftime(floor_date(input$dateselector, "month"))
        }
        
        df <- state_data_prepared() %>% 
            filter(month_year == selected_month)
        
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
        )
        
        fig <- plot_geo(df, locationmode = 'USA-states')
        fig <- fig %>% add_trace(
            type="choropleth",
            locations=df$state.abbreviation,
            z=df$monthly_deaths_per10000,
            text = paste0("<b>State:</b> ", df$NAME, "<br>",
                "<b>Monthly Deaths (per 10000):</b> ", df$monthly_deaths_per10000),
            hoverinfo = 'text',
            color = df$monthly_deaths_per10000,
            colorscale="inferno",
            marker=list(line=list(
                color = 'black',
                width=0.5)
            )
        )
        fig <- fig %>% colorbar(title = "Deaths per 10000")
        fig <- fig %>% layout(
            geo = g, 
            title = "Monthly COVID-19 Deaths by State"
        )
        
        return(fig)
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
