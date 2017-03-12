


data_raw <- structure(list(Group.1 = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 1L, 2L, 3L, 4L, 5L, 7L, 1L, 2L, 3L, 4L, 5L, 6L, 7L), 
                        .Label = c("2016-07-01", "2016-08-01", "2016-09-01", "2016-10-01", "2016-11-01", "2016-12-01", "2017-01-01"), 
                        class = "factor"), Group.2 = c(101, 101, 101, 101, 101, 101, 101, 103, 103, 103, 103, 103, 103, 105, 105, 105, 105, 105, 105, 105), 
                        x = c("57976", "42933", "49731", "46808", "48938", "41937", "41932", "48900", "37625", "57700", "41700", "57600", "48800", "46102", "35796", "42490", "46755", "46757", "46810", "45232")), 
                        .Names = c("Group.1", "Group.2", "x"), 
                        row.names = c(NA, 20L), class = "data.frame")


library(shiny)
library(ggplot2)

ui <- fluidPage(
        
        titlePanel("Legends"),
        
        sidebarLayout(position = "left",
                      sidebarPanel(checkboxGroupInput("checkGroup", 
                                                      label = h3("Postnumber"), 
                                                      choices = list( 
                                                              "101" = 101, 
                                                              "103" = 103, 
                                                              "105" = 105),
                                                        selected = "101"
                                                     )),
                 mainPanel("main panel", 
                        plotOutput('myplot'))
        ))

server <- function(input, output) { 
        
        output$myplot <- renderPlot({
                
        grouped_data <- data_raw[data_raw$Group.2 %in% input$checkGroup,]
        
        p <- ggplot(grouped_data, aes(Group.1, x, group = Group.2)) +
                scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                                    labels=c("postnr 101", "postnr 103", "postnr 105"))
        p + geom_line() +
                theme(axis.title.y=element_text(margin=margin(0,20,0,0))) +
                geom_line(data = grouped_data, aes(colour = factor(Group.2)))
        
})
        }
shinyApp(ui = ui, server = server) # this launches your app