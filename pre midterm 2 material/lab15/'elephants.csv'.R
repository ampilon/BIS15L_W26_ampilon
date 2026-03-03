library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)



elephants <- read_csv("data/elephants_data/elephants.csv") %>%
  clean_names()



ui <- dashboardPage(
  
  dashboardHeader(title = "Age or Height by Sex"),
  
  dashboardSidebar(  
    
    selectInput("y",
                "Select a variable of interest",
                choices = c("age",
                            "height"),
                selected = "age")
  ),
  
  dashboardBody(
    
    
    plotOutput("plot", width = "500px", height = "400px")
    
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    elephants %>%
      ggplot(aes(x= sex, y= .data[[input$y]]))+
      geom_boxplot(fill = "lightgreen",
                   color = "black") +
      theme_classic()
    
  })
}

shinyApp(ui, server)