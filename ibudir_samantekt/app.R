##### fyrir hvert póstnúmer fyrir sig

library(shiny)
library(ggplot2)
library(lubridate)

data_clean <- read.csv("data/data_clean.csv", colClasses = c("date"="Date"))
mround <- function(x,base){ 
        base*round(x/base) 
} 
maxsize <- max(data_clean$size)
maxsize <- mround(maxsize,5)
minsize <- min(data_clean$size)
minsize <- mround(minsize,5)

maxdate <- max(data_clean$date)
startdate <- min(data_clean$date) %m-% months(1)
six.m.ago <- maxdate %m-% months(6)
six.m.later <- maxdate %m+% months(12)
date_length <- (as.yearmon(six.m.later) - as.yearmon(six.m.ago))*12
all_postnr <- as.list(sort(unique(data_clean$postnr)))

test <- data_clean
data_clean$plot.month <- (as.yearmon(data_clean$date) - as.yearmon(startdate))*12
date.f.plot <- seq(as.Date(six.m.ago), by = "month", length.out = date_length)



ui <- fluidPage(
        sidebarLayout(position = "left",
                    sidebarPanel(selectInput("select", label = h3("Select box"), 
                                              choices = setNames(all_postnr, paste0("Póstnúmer ", all_postnr))
                                             ),
                    sliderInput("size",
                                label = h3("Stærð í fm2"),
                                min = minsize,
                                max = maxsize,
                                step = 5,
                                value = c(75, 125)),
                    actionButton("goButton", "Sýna gögn")
                    ),
             
              
                    
                    mainPanel("main panel", 
          plotOutput('myplot'))))


server <- function(input, output) { 

        biggerfm <- 100
        smallerfm <- 75
        posturinn <- 105
        
        
        ## Sex mánuði aftur í tímann
        data_clean <- data_clean[data_clean$date >= six.m.ago,] 
                
        ### RVK meðalverð
        postnr_mean <- c(101:116, 170, 172)
        prep_trend <- data_clean[data_clean$postnr %in% postnr_mean,]
        prep_trend <- prep_trend[prep_trend$size > smallerfm & prep_trend$size < biggerfm,]
        
        rvk_mean <- mean(prep_trend$verd)
        rvk_fm_mean <- mean(prep_trend$fermverd)
        
        
        
        ### Trim the Data
        trimmed_data <- data_clean[data_clean$size > smallerfm & data_clean$size < biggerfm,]
        trimmed_data <- trimmed_data[trimmed_data$postnr == posturinn,]
        
        trimmed_mean <- mean(trimmed_data$verd)
        trimmed_fm_mean <- mean(trimmed_data$fermverd)
        
        max_verd <- max(trimmed_data$verd)
        min_verd <- min(trimmed_data$verd)
        
        ### gera dataframe úr þessu
        means <- c(rvk_mean, trimmed_mean)
        means.fm <- c(rvk_fm_mean, trimmed_fm_mean)
        
        means.df <- data.frame(means, means.fm)
        rownames(means.df) <- c("Meðaltal í Reykjavík", "Meðaltal í póstnúmeri")
        colnames(means.df) <- c("meðalverð", "meðalfermetraverð")
        
        ### get number of rooms
        herbergi.1 <- nrow(trimmed_data[trimmed_data$herbergi == 1,])
        herbergi.2 <- nrow(trimmed_data[trimmed_data$herbergi == 2,])
        herbergi.3 <- nrow(trimmed_data[trimmed_data$herbergi == 3,])
        herbergi.4 <- nrow(trimmed_data[trimmed_data$herbergi == 4,])
        herbergi.5 <- nrow(trimmed_data[trimmed_data$herbergi >= 5,])
        herbergi <- c(herbergi.1, herbergi.2, herbergi.3, herbergi.4, herbergi.5)
        herbergi.names <- c("eitt herbergi", "tvö herbergi", "þrjú herbergi", "fjögur herbergi", "fimm herbergi eða fleiri")
        
        herbergin.df <- data.frame(herbergi.names, herbergi)
        }
        unique(trimmed_data$date)
        str(maxdate)
        the.coef <- coef(lm(trimmed_data$verd ~ trimmed_data$plot.month))
        inter <- the.coef[1]
        slope <- the.coef[2]
        mean.in.10 <- inter + (18*slope)
        mean.today <- inter + (6*slope)
        
        price.increase <- ((mean.in.10-mean.today)/mean.today)*100
        
        ggplot(data = trimmed_data, aes(plot.month, verd )) +
                geom_point() + 
                coord_cartesian(xlim = c(1, 18)) + 
                geom_abline(intercept = inter, slope = slope)
               
        #scale_x_date(limits = c(NA, as.Date(six.m.later)))
                
        
        lm(formula = trimmed_data$verd ~ trimmed_data$plot.month, data = trimmed_data)
     

shinyApp(ui = ui, server = server) # this launches your app