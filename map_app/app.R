library(shiny)
library(ggmap)
ui <- fluidPage(
        fluidRow(div(align="center", titlePanel("Meðal verð á íbúðum í þessu svæði"))),
        
        fluidRow(mainPanel(plotOutput("streetMap"))),
        fluidRow(mainPanel(
                verbatimTextOutput("nText")
                
        )),
        fluidRow(
                
                
                column(4,
                       textInput("street", label = h3("Götuheiti og númer"), 
                                 value = ("Götuheiti og númer"))),
                
                
                column(2,  numericInput("postnr", 
                                        label = h3("Póstnúmer"), 
                                        value = 101)),
                
                column(2, numericInput("fermetrar", 
                                       label = h3("fermetrar"), 
                                       step = 0.1, 
                                       value = 75.5,
                                       min= 101))),
        
        fluidRow(actionButton("goButton", "Go!"))
        
        
)


##############################################################
##############################################################
##############################################################

server <- function(input, output) {
        
        
        streetMap <- eventReactive(input$goButton, {
                
                print("Test button map")
                mymap <- ggmap(get_map(c(v.punktur, s.punktur , a.punktur, n.punktur), source = "google"))
                print(mymap)
        })
        
        output$streetMap <- renderPlot({
                streetMap() 
        })
        
        # builds a reactive expression that only invalidates 
        # when the value of input$goButton becomes out of date 
        # (i.e., when the button is pressed)
        ntext <- eventReactive(input$goButton, {
                print(input$fermetrar)
                data_clean <- read.csv("data/data_clean.csv")
                street.db <- read.csv("data/iceland_streets.csv")
                
                
                fm2.minna <- input$fermetrar - 15
                fm2.staerra <- input$fermetrar + 15
                
                
                input.postnr <- as.numeric(input$postnr)
                
                street.number <- as.numeric(gsub("\\D", "", input$street))
                street.name <- sub(" +$", "", gsub("\\d", "", input$street))
                
                
                center_long <- street.db$LON[street.db$POSTCODE == input$postnr & street.db$NUMBER == street.number & street.db$STREET == street.name]
                center_lat <- street.db$LAT[street.db$POSTCODE == input$postnr & street.db$NUMBER == street.number & street.db$STREET == street.name]
                
                ##  0,002701 er jafnt og 300m á lat
                ##  0,006208 er jafnt og 300m á long
                
                lat300 <- 0.002701
                long300 <- 0.006208
                
                
                ## Create the square around the center house
                ## Vestur er -21 og áfram mínus --- Austur er því -21 og plúsa við það
                ## 
                ## Norður er plús, suður er mínus.
                ##
                ##
                ## Fá "kassann" um miðjuna
                ##
                a.punktur <- center_long + long300
                v.punktur <- center_long - long300
                s.punktur <- center_lat - lat300
                n.punktur <- center_lat + lat300
                
                # a.punktur <- -21.9003
                # v.punktur <- -21.91271
                # s.punktur <- 64.12896
                # n.punktur <- 64.13436
                ## skila lista af götum sem eru innan þessara fjögurra punkta
                gotulisti <- unique(droplevels(street.db$STREET[street.db$LAT < n.punktur & street.db$LAT > s.punktur & street.db$LON < a.punktur & street.db$LON > v.punktur ]))
                gotulisti <- as.list(levels(gotulisti))
                
                
                for.mean <- data_clean$verd[data_clean$gotuheiti %in% gotulisti & data_clean$size > fm2.minna & data_clean$size < fm2.staerra]
                length(for.mean)
                the.mean <- mean(for.mean)
                the.mean <- prettyNum(the.mean, big.mark=".", decimal.mark = ",", scientific=FALSE)
                the.mean <- paste0(the.mean, "kr.-")
                print(the.mean)
                
        })
        
        
        output$nText <- renderText({
                ntext()
        })
        
        
        
}

shinyApp(ui = ui, server = server) # this launches your app

