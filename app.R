library(shinydashboard)
library(quantmod)
library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Demo Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stocks Example", tabName = "stock_dashboard", icon = icon("line-chart")),
      menuItem("Map Example", tabName = "map_dashboard", icon = icon("globe")),
      menuItem("Another Example", tabName = "histogram_dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "stock_dashboard",
            fluidRow(
              box(width = 3,
                helpText("Select a stock to examine. 
        Information will be collected from yahoo finance."),
                
                textInput("symb", "Symbol", "SPY"),
                
                dateRangeInput("dates", 
                               "Date range",
                               start = "2013-01-01", 
                               end = as.character(Sys.Date())),
                
                br(),
                
                checkboxInput("log", "Plot y axis on log scale", 
                              value = FALSE),
                
                checkboxInput("adjust", 
                              "Adjust prices for inflation", value = FALSE)
              ),
              box(width = 9,
                plotOutput("plot")
              )
            )
      ),
      
      # Second tab content
      tabItem(tabName = "map_dashboard",
              fluidRow(
                box(title = "Map Example", width = 9, solidHeader = TRUE, status = "warning", collapsible = TRUE,
                    plotOutput("map")
                ),
                box(width = 3, status = "warning", 
                    selectInput("var", 
                                label = "Choose a variable to display",
                                choices = c("Percent White", "Percent Black",
                                            "Percent Hispanic", "Percent Asian"),
                                selected = "Percent White"),
                    sliderInput("range", 
                                label = "Range of interest:",
                                min = 0, max = 100, value = c(0, 100))
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "histogram_dashboard",
            fluidRow(
              valueBox(
                # The value comes from the server via uiOutput
                uiOutput("orderNum"), "New Orders", icon = icon("credit-card")
              )
            ),
            fluidRow(
              box(title = "Plot", solidHeader = TRUE, status = "primary", collapsible = TRUE,
                  plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls", solidHeader = TRUE, status = "primary", collapsible = TRUE,
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
      )
    )
  )
)

server <- function(input, output) {
  
  dataInput <- reactive({  
    getSymbols(input$symb, src = "yahoo", 
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  finalInput <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())
  })
  
  output$plot <- renderPlot({
    chartSeries(finalInput(), theme = chartTheme("white"), 
                type = "line", log.scale = input$log, TA = NULL)
  })
  
  output$orderNum <- renderText({ 10*2 })
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$map <- renderPlot({
    data <- switch(input$var, 
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$var, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")
    
    legend <- switch(input$var, 
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    percent_map(var = data, 
                color = color, 
                legend.title = legend, 
                max = input$range[2], 
                min = input$range[1])
  })
}

shinyApp(ui, server)