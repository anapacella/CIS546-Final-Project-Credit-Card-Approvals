#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(ggthemes)
library(plotly)

credit <- read.csv("CreditCard1.csv", stringsAsFactors = T)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Credit Card Approvals"),
        
        
        radioButtons("Gender", label = h3("Gender"),
                     choices = c("Male" = "Male", "Female" = "Female"), 
                     selected = 0),
        
        radioButtons("Married", label = h3("Married"),
                     choices = c("Yes" = "Yes", "No" = "No"), 
                     selected = 0),
    
        radioButtons("Approved", label = h3("Approved"),
                    choices = c("Yes" = "Yes", "No" = "No"), 
                    selected = 0),
    
        selectInput("Ethnicity", label = h3("Ethnicity"), 
                    choices = list("White" = "White", "Black" = "Black", "Latino" = "Latino", "Asian" = "Asian", "Other" = "Other"), 
                    selected = "White"),
        
        
        sliderInput(inputId = "Age",
                    label = "Age:",
                    min = 0,
                    max = 100,
                    value = 50),

     
        plotlyOutput("distPlot")
        )

# Define server logic required to draw a histogram

server <- function(input, output){
    
    output$distPlot <- renderPlotly({
        plot <- credit %>% filter(Age >= input$Age, 
                                  Ethnicity == input$Ethnicity)%>%
            
            ggplot() +
            geom_point(mapping = aes(x = YearsEmployed, y = Debt,
                                     size = CreditScore, 
                                     color = Approved)) + 
            labs(title = "Credit Card Approval",
                 x = "Years Employed",
                 y = "Debt")
        
        ggplotly(plot)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
