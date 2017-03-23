### LÍNURITIÐ

library(shiny)
library(ggplot2)
        
cleandata <- read.csv("data/data_clean.csv")

#fyrir roundup út af maxsize og minsize
mround <- function(x,base){ 
        base*round(x/base) 
} 
maxsize <- max(cleandata$size)
maxsize <- mround(maxsize,5)
minsize <- min(cleandata$size)
minsize <- mround(minsize,5)
postnr_mean <- c(101:116, 170, 172)
dagsetningar <- as.character(unique(unlist(cleandata$date)))
        
## Clean up the datafile
cleandata <- cleandata[, c("postnr", "verd", "size", "fermverd", "date")]
        
        ui <- fluidPage(
                
                titlePanel("Þróun á íbúðarverði"),
                
                sidebarLayout(position = "left",
                              sidebarPanel(checkboxGroupInput("checkGroup", 
                                                label = h3("Póstnúmer"), 
                                                choices = list( 
                                                                "Meðaltal" = 999,
                                                                "101 - Miðborg" = 101,
                                                                "103 - Háaleitis- og Bústaðahverfi" = 103,
                                                                "104 - Laugardalur" = 104,
                                                                "105 - Hlíðar" = 105,
                                                                "107 - Vesturbær" = 107,
                                                                "108 - Háaleitis- og Bústaðahverfi" = 108, 
                                                                "109 - Breiðholt" = 109, 
                                                                "110 - Árbær" = 110,
                                                                "111 - Breiðholt" = 111,
                                                                "112 - Grafarvogur" = 112,
                                                                "113 - Grafarholt og Úlfársdalur" = 113,
                                                                "116 - Kjalarnes" = 116,
                                                                "170 - Seltjarnarnes" = 170
                                                               ),
                                                selected = c("Meðaltal")
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
                                                       step = 5,
                                                       value = c(75, 125)
                                                       
                                           )
                                           ),
                              mainPanel("main panel", 
                                        plotOutput('myplot'))
                              
                              
                ))
                
        server <- function(input, output) { 
        
        output$myplot <- renderPlot({
              
        if(is.null(input$checkGroup)) {ggplot()} else {
                ### Bara fyrir meðaltal
                prep_trend <- cleandata[cleandata$postnr %in% postnr_mean,]
                prep_trend <- prep_trend[prep_trend$size > input$size[1] & prep_trend$size < input$size[2],]
                prep_trend$postnr <- 999
                

                cleandata <- rbind(cleandata, prep_trend)
                
                prep <- cleandata[cleandata$postnr %in% input$checkGroup,]
                prep <- prep[prep$size > input$size[1] & prep$size < input$size[2],]
                
                
                if(input$dataType == 1)
                        {
                aggr_data <- aggregate(prep$verd, by=list(prep$date, prep$postnr), mean)
                aggr_data$Group.2 <- paste("postnr ", aggr_data$Group.2, sep = "")
                aggr_data[aggr_data == "postnr 999"] <- "Meðaltal"
                y.name <- "Verð"
                
                } else {
                
                aggr_data <- aggregate(prep$fermverd, by=list(prep$date, prep$postnr), mean)
                aggr_data$Group.2 <- paste("postnr ", aggr_data$Group.2, sep = "")
                aggr_data[aggr_data == "postnr 999"] <- "Meðaltal"
                y.name <- "Fermetraverð"
                
                ## Hér er plot af Aggregated gögnum
                        }
                
                aggr_data$Group.2 <- factor(aggr_data$Group.2,
                                            levels = c("Meðaltal", "postnr 101", "postnr 103", "postnr 104", "postnr 105", "postnr 107", "postnr 108", "postnr 109", "postnr 110", "postnr 111", "postnr 112", "postnr 113", "postnr 116", "postnr 170"),
                                            ordered = TRUE)
 
        options(scipen=5)
        
       
        observe(p)
        ggplot(aggr_data,
               aes_string(x = "Group.1", y = "x", group = "Group.2",
                          colour = "Group.2")) +
                geom_line() + 
                scale_colour_manual(values=c("#E31A1C", "#8ABFDB", "#2171B5", "#A1D99B", "#238B45", "#FDAE6B", "#D94801", "#BCBDDC", "#54278F", "#E7298A", "#8C510A", "#FFFFBF", "#DE77AE", "#666666"),
                                guide = guide_legend(title = "Postnr"), drop = FALSE) +
                                guides(colour = guide_legend(override.aes = list(size=3))) +
                                xlab("Dagsetningar") +
                                ylab(y.name) +        
                theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
      
        }
        })   
                } #the server
        
        shinyApp(ui = ui, server = server) # this launches your app
        
