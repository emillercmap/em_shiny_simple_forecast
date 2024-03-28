library(shiny)
library(ggplot2)
library(tidyverse)
library(cmapplot)
library(shinythemes)
library(plotly)
library(ggtext)
library(usethis)
library(forcats)


# read data
df <- read_rds("trendstidy.rdata")

# turn off scientific notation

trend_names <- c("Linear Trend: 2000 - 2010", "Linear Trend: 1990 - 2010",
                 "Exponential Trend: 2000 - 2010", "Exponential Trend: 1990 - 2010")
# APP BUILDING

# Define UI for application that draws a graph of parsimonious predictions
ui <- fluidPage(
  theme = shinytheme("lumen"),
  
  # Application title
  titlePanel("Municipal Population Trend Extrapolation Tool"),
  
  # Sidebar with a drop-down menu for munis and check boxes for trends
  fluidRow(
    column(4,
      selectizeInput("muni", "What municipality would you like to view?", df$muni),
      checkboxGroupInput("trend", "What trends would you like to see?", 
                         choices = c("Linear Trend: 2000 - 2010", "Linear Trend: 1990 - 2010",
                                     "Exponential Trend: 2000 - 2010", "Exponential Trend: 1990 - 2010"),
      ),
    )
    ),
    
    # Show a plot of the generated distribution
  fluidRow(
    column(12,
           plotly::plotlyOutput("distPlot")),
    htmlOutput("text1")
    
  ),
  
  # Include CSS output
  uiOutput("css")
  )



# Define server logic
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    validate(
      need(input$muni, "Please select a municipality")
    )
    validate(
      need(input$trend, "Please select trends")
    )
    
    # Filter data based on user input trends
    filtered_data <- df %>%
      filter(muni == input$muni, trend %in% input$trend)
    
    # Filter census data
    census_data <- df %>%
      filter(muni == input$muni, trend == "Census Data 1990-2020")
    
    # Bind census data to filtered_data
    final_data <- bind_rows(filtered_data, census_data)
    
    # Check for NA values in "value" column when "year" is equal to 1990
    if (any(is.na(final_data$value[final_data$year == 1990]))) {
      return(NULL)
    }
    
    p <- ggplot(data = final_data) +
      geom_line(mapping = aes(x = year,
                              y = value,
                              color = trend),
                linewidth = 1.5)+
      scale_y_continuous(labels = scales::comma) +
      expand_limits(y = 0) +  # Ensures y-axis starts at 0
      scale_color_manual(values = c("Linear Trend: 2000 - 2010" = "#00665c",
                                    "Linear Trend: 1990 - 2010" = "#b7e572",
                                    "Exponential Trend: 2000 - 2010" = "#3f0030",
                                    "Exponential Trend: 1990 - 2010" = "#36d8ca")) +
      theme_cmap(xlab = "Year",
                 ylab = "Population") +
      labs(
        title = "Population trend extrapolations through 2050 for municipalities in northeastern Illinois",
        caption = "Source: US Census Bureau Decennial Census",
        x = "Year",
        y = "Population",
        color = "Trends"
      ) +
      theme(plot.title = element_text(size = 22)) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(p)
  })
  
  output$text1 <- renderUI({
    
    # Filter data based on user input trends
    filtered_data <- df %>%
      filter(muni == input$muni, trend %in% input$trend)
    
    # Filter census data
    census_data <- df %>%
      filter(muni == input$muni, trend == "Census Data 1990-2020")
    
    # Bind census data to filtered_data
    final_data <- bind_rows(filtered_data, census_data)
    
    # Check for NA values in "value" column when "year" is equal to 1990
    if (any(is.na(final_data$value[final_data$year == 1990]))) {
      HTML("<div class='warning'>Insufficient data to form projections</div>")
    } else {
      NULL
    }
  })
  
  # Add CSS styling for the text
  output$css <- renderUI({
    HTML("
         <style>
         .warning {
           position: absolute;
           top: 500px; 
           left: 500px; 
           font-size: 18px; 
           color: red;
           font-weight: bold; 
         }
         </style>
         ")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)