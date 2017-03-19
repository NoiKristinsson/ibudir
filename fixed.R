




server <- function(input, output) { 
        output$myplot <- renderPlot({
                
                grouped_data <- data_raw[data_raw$Group.2 %in% input$checkGroup,]
                
                grouped_data$Group.2 <- paste("postnr ", grouped_data$Group.2, sep = "")
                
                grouped_data$Group.2 <- factor(grouped_data$Group.2,
                                               levels = c("postnr 101", "postnr 103", "postnr 105"),
                                               ordered = TRUE) 
                
                ggplot(grouped_data,
                       aes_string(x = "Group.1", y = "x", group = "Group.2",
                                  colour = "Group.2")) +
                        geom_line() + 
                        scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                                            guide = guide_legend(title = "Postnr"), drop = FALSE) +
                        theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
                
        }) }