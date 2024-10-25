library(shiny)
library(tidyverse)

WDI
WDI <- read.csv("wdi.csv")
WDI <- filter(WDI, !is.na(WDI$itnet_pop) & !is.na(WDI$gdpPercap) & !is.na(WDI$gini))
WDI <- WDI %>% group_by(region, year)  %>%
  pivot_longer(c("gdpPercap", "gini"), names_to ="dataset", values_to = "index_values")
WDI_FINAL <- WDI

WDI_FINAL$region <- as.factor(WDI$region)
WDI_FINAL$dataset <- as.factor(WDI$dataset)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Impact of Technology on Economy"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Select Year",
                  min = min(WDI_FINAL$year),
                  max = max(WDI_FINAL$year),
                  value = min(WDI_FINAL$year),
                  step=10),
      
      checkboxGroupInput("region",
                         "Select Region",
                         choices = levels(WDI_FINAL$region),
                         selected = levels(WDI_FINAL$region)),
      
      selectInput("dataset", "Select Dataset", 
                  choices = levels(WDI_FINAL$dataset))
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("WDIPlot")
    )
  )
)



server <- function(input, output) {
  
  output$WDIPlot <- renderPlot({
    WDI_FINAL %>% 
      filter(year == input$year) %>%
      filter(region %in% input$region) %>%
      filter(dataset %in% input$dataset) %>%
      ggplot(aes(x = itnet_pop,
                 y = index_values)) +
      geom_point(aes(colour = region))+
      
    
    
    
    
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


