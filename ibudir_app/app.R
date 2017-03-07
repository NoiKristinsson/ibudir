        library(shiny)
        library(ggplot2)
        
        cleandata <- read.csv("data/data_clean.csv")
        maxsize <- max(cleandata$size)
        minsize <- min(cleandata$size)
        postnr_mean <- c(101:116, 170, 172)
        dagsetningar <- as.character(unique(unlist(cleandata$date)))
        
        ## Clean up the datafile
        cleandata <- cleandata[, c("postnr", "verd", "size", "fermverd", "date")]
        
        ui <- fluidPage(
                
                verd <- 
                titlePanel("Þróun á íbúðarverði"),
                
                sidebarLayout(position = "left",
                              sidebarPanel(checkboxGroupInput("checkGroup", 
                                                label = h3("Póstnúmer"), 
                                                choices = list( 
                                                                "Meðaltal" = 999,
                                                                "101" = 101, 
                                                                "103" = 103, 
                                                                "104" = 104,
                                                                "105" = 105
                                                               ),
                                                selected = c("101")
                                                                ),
                                           selectInput("dataType", 
                                                label = h3("Tegund gagna"),
                                                choices = list("Verð" = 1, "Fermetraverð" = 2),
                                                selected = 1
                                                ),
                                           sliderInput("size",
                                                       label = h3("Stærð í fm2"),
                                                       min = minsize,
                                                       max = maxsize,
                                                       value = c(75, 125)
                                                       
                                           )
                                           ),
                              mainPanel("main panel", 
                                        plotOutput('myplot'))
                              
                              
                ))
                
        server <- function(input, output) { 
        
        output$myplot <- renderPlot({
              
                ### Bara fyrir meðaltal
                prep_trend <- cleandata[cleandata$postnr %in% postnr_mean,]
                prep_trend <- prep_trend[prep_trend$size > input$size[1] & prep_trend$size < input$size[2],]
                prep_trend$postnr <- 999
                
              
                #aggr_mean$Group.2 <- rep(999,nrow(aggr_mean))
                #aggr_mean[,c(1,3,2)]
                #merge(aggr_data, aggr_mean)

                cleandata <- rbind(cleandata, prep_trend)
                
                prep <- cleandata[cleandata$postnr %in% input$checkGroup,]
                prep <- prep[prep$size > input$size[1] & prep$size < input$size[2],]
                
                if(input$dataType == 1)
                        {
                aggr_data <- aggregate(prep$verd, by=list(prep$date, prep$postnr), mean)
                y.name <- "Verð"
                
                } else {
                
                aggr_data <- aggregate(prep$fermverd, by=list(prep$date, prep$postnr), mean)
                y.name <- "Fermetraverð"
                
                ## Hér er plot af Aggregated gögnum
                        }
                
        options(scipen=5)
        
 
        observe(p)
        p <- ggplot(aggr_data, aes(Group.1, x, group = Group.2))
        p + geom_line() +
                theme(axis.title.y=element_text(margin=margin(0,20,0,0))) +
                scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
                labs(x = "Dagsetning", y = y.name, labels=c("Meðaltal", "101", "103")) +
                geom_line(data = aggr_data, aes(colour = factor(Group.2))) +
                xlim(dagsetningar)
  
                      
               
        })   
                } #the server
        
        shinyApp(ui = ui, server = server) # this launches your app
        
