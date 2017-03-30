##### TEST TEST TEST

library(shiny)
library(ggplot2)
library(lubridate)
library(zoo)





ui <- fluidPage(
                
                fluidRow(mainPanel(submitButton("Sýna gögn"))),
                    
                fluidRow(mainPanel( verbatimTextOutput("allt")))

          )


server <- function(input, output) { 
        
        observeEvent(input$do, {

                output$textinn <- renderText({"texti.e.1ar"})
        print("texti.e.1ar")
       
         })
    
}    
     

shinyApp(ui = ui, server = server) # this launches your app
