library(shiny)
library(plotly)

# Read data
data <- read.csv("./WorldBankData.csv")

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("World Bank Data"),

    # Sidebar with a slider input for year
    sidebarLayout(
        sidebarPanel(
            selectInput("year",
                        "Year",
                        choices = (data %>% filter(Year != 2018) %>% select(Year) %>% unique() %>% arrange(Year)),
                        selected = 2000)
        ),

        # Show a scatter plot for the selected year
        mainPanel(
           plotlyOutput("scatterPlot")
        )
    )
)

# Define server logic 
server <- function(input, output) {

    output$scatterPlot <- renderPlotly({
        
    # Selected year
    year <- input$year
    print(year)

    # Processing data
    dataplot <- data %>% 
        filter(Year == year)
    
    # create plotly plot
    scatterPlot <- plot_ly(data = dataplot,
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
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
