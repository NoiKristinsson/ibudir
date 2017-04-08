library(shiny)

ui = fluidPage(
        titlePanel("Uploading Files"),
        sidebarLayout(
                sidebarPanel(
                        downloadButton("download1", "DB 1 - Download"),
                        downloadButton("download2", "DB 2 - Download")
                ),
                mainPanel("Nothing here in this example")
                ))

server = function(input, output, session){
        
        ##### This would be sought by uploading a file, but for simplicity I have excluded that.
        ##### This is why I've located it inside the render part of the program
        splitted <- reactive({
                        my.data <- structure(list(name = c("John", "Silvia", "Dana", "Daniel", "Sarah", "Jake", "Zulu"), 
                                job = c("Waiter", "Pilot", "Programmer", "Programmer", "Singer", "Actor", "Scientist"), 
                                age = c(23L, 45L, 21L, 26L, 23L, 15L, 34L), 
                                number = c(125L, 643L, 834L, 238L, 193L, 964L, 293L), 
                                car = c("Saab", "Volvo", "Toyota", "Saab", "Lada", "Porche", "Ferrari"), 
                                tvshow = c("Lost", "The Walking Dead", "Friends","Band of Brothers", "That 70's show", "The Walking Dead", "Buffy the vampire slayer")), 
                                .Names = c("name", "job", "age", "number", "car", "tvshow"), 
                                row.names = c(NA, -7L), class = c("tbl_df", "tbl", "data.frame"))
                                })
        ###### There would be some calculations and manipulation of the data before next step.
        ### here I split workflow in two, to create two distinct dataframes for downloading
        
        my.data1 <- reactive({ 
                my.data.new1 <- data.frame(matrix("", nrow = nrow(splitted()), ncol= 3 ))
        colnames(my.data.new1) <- c("My Name", "Cars", "Postnumber")
        my.data.new1$`My Name` <- paste0("My name is ", splitted()$name)
        my.data.new1$Cars <- splitted()$car
        my.data.new1$Postnumber <- paste0("Postnumber ", splitted()$number)
        my.data.new1
                                })
        
        
        ##### Here is data frame 2
        my.data2 <- reactive({
        my.data.new2 <- data.frame(matrix("", nrow = nrow(splitted()), ncol= 2 ))
        colnames(my.data.new2) <- c("Name", "Age")
        my.data.new2$Name <- splitted()$name 
        my.data.new2$Age <- splitted()$age
        my.data.new2
                                })        
        
        output$download1 <- downloadHandler(
                filename = function() { paste0("DB_1", ".csv") },
                content = function(file) {
                        write.csv(my.data1(), file)
                                        })
        outputOptions(output, 'download1', suspendWhenHidden=FALSE)
        output$download2 <- downloadHandler(
                filename = function() { paste0("DB_2", ".csv") },
                content = function(file) {
                        write.csv(my.data2(), file)
                                        })
        
        } 
        
        


shinyApp(ui = ui, server = server) # this launches your app