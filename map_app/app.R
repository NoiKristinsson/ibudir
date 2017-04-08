#### MAP FORRITIÐ

library(shiny)
library(ggmap)

data_clean <- read.csv("data/data_clean.csv")
street.db <- read.csv("data/iceland_streets.csv")
punktar_react <- reactiveValues()





ui <- fluidPage(
        fluidRow(div(align="center", titlePanel("Meðal verð á íbúðum í þessu svæði"))),
        
        fluidRow(mainPanel(plotOutput("streetMap"))),
        fluidRow(mainPanel(
                #verbatimTextOutput("nText")
                htmlOutput("nText")
                
        )),
        fluidRow(
                
                
                column(4,
                       textInput("street", label = h3("Götuheiti og númer"), 
                                 value = ("Götuheiti og númer"))),
                
                
                column(2,  numericInput("postnr", 
                                        label = h3("Póstnúmer"), 
                                        value = 101)),
                
                column(2, numericInput("fermetrar", 
                                       label = h3("Fermetrar"), 
                                       step = 0.1, 
                                       value = 75.5,
                                       min= 101)),
                br(), br(), br(),
                column(2, fluidRow(actionButton("goButton", "Leita")))),
        
        fluidRow(helpText("Skrifaðu Götu, húsnúmer, póstnúmer og stærð og fáðu meðalverðið á svæðinu í kring.",
                          br(),
                          "Það kemur villa ef:",
                          br(),
                          "Ekki nógu mörg hús hafa verið til sölu á svæðinu",
                          br(),
                          "Vitlaust heimilisfang er slegið inn.",
                          br(),
                          "Við útreikning á stærð er miða við +/- 15 fm2"
                          ))
        
        
        
)


##############################################################
##############################################################
##############################################################

server <- function(input, output) {
        
        
       
        
        # builds a reactive expression that only invalidates 
        # when the value of input$goButton becomes out of date 
        # (i.e., when the button is pressed)
        ntext <- eventReactive(input$goButton, {
                #print(input$fermetrar)
                
                fm2.minna <- input$fermetrar - 15
                fm2.staerra <- input$fermetrar + 15
                
                
                input.postnr <- as.numeric(input$postnr)
                
                street.number <- as.numeric(gsub("\\D", "", input$street))
                street.name <- sub(" +$", "", gsub("\\d", "", input$street))
                
                
                center_long <- street.db$LON[street.db$POSTCODE == input$postnr & street.db$NUMBER == street.number & street.db$STREET == street.name]
                center_lat <- street.db$LAT[street.db$POSTCODE == input$postnr & street.db$NUMBER == street.number & street.db$STREET == street.name]
                count <- 1
                lat300 <- 0
                long300 <- 0
                count <- 0
                
                repeat{
                ##  0,002701 er jafnt og 300m á lat
                ##  0,006208 er jafnt og 300m á long
                
                lat300 <- lat300 + 0.002701
                long300 <- long300 + 0.006208
                count <- count + 1
                #print(lat300)
                
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
                
                punktar_react$a <- a.punktur
                punktar_react$v <- v.punktur
                punktar_react$s <- s.punktur
                punktar_react$n <- n.punktur
                
                # a.punktur <- -21.9003
                # v.punktur <- -21.91271
                # s.punktur <- 64.12896
                # n.punktur <- 64.13436
                ## skila lista af götum sem eru innan þessara fjögurra punkta
                gotulisti <- unique(droplevels(street.db$STREET[street.db$LAT < n.punktur & street.db$LAT > s.punktur & street.db$LON < a.punktur & street.db$LON > v.punktur ]))
                gotulisti <- as.list(levels(gotulisti))
                #print(paste0("fjoldi husa ", length(gotulisti)))
                
               
                
                for.mean <- data_clean$verd[data_clean$gotuheiti %in% gotulisti & data_clean$size > fm2.minna & data_clean$size < fm2.staerra]
                fjoldi.husa <- length(for.mean)
                #print(count)
                if(count > 1 | length(for.mean) > 5){
                        break
                        }
                }
                
                the.mean <- mean(for.mean)
                the.mean <- prettyNum(the.mean, big.mark=".", decimal.mark = ",", scientific=FALSE)
                the.mean <- paste0("Meðalverð á svæðinu (sjá kort) er: ", the.mean, "kr.-")
                area.searched <- count * 300
                #area <- paste0("leitað var í ", area.searched,"m ", "radíus")
                
                c(the.mean, area.searched, fjoldi.husa)
                
        })
        
        streetMap <- eventReactive(input$goButton, {
        
                mymap <- ggmap(get_map(c(punktar_react$v, punktar_react$s, punktar_react$a, punktar_react$n), source = "google"))
                print(mymap)
        })
        
        output$nText <- renderPrint({
                
                setn2 <- paste0("leitað var í ", ntext()[2],"m ", "radíus")
                setn3 <- paste0("notast var við gögn frá ", ntext()[3], " húsum/íbúðum")
                HTML(paste(ntext()[1], setn2, setn3, sep = "<br/>" ))
        })
    
        output$streetMap <- renderPlot({
                streetMap() 
        })
        
        #output$search.area <- eventReactive(input$goButton, {
        #        paste0("leitað var í ", area.searched,"m ", "radíus")
        #})
        
}

shinyApp(ui = ui, server = server) # this launches your app