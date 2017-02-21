require(XML)
require(RCurl)
library(RMySQL)


blanda_ready <- data.frame()
blanda <- data.frame()

website <- "http://www.mbl.is/fasteignir/leit/?q=abfafabeaea3e6435c02ce698e13fe6a"


fixedURL <- getURL(website, followlocation=TRUE)
parsed.html <- htmlParse(fixedURL, encoding='UTF-8')

### Hérna byrja ég á því að skoða hvað það eru margar síður á fasteignum mbl ###

theline <- xpathApply(parsed.html,"//div[@class='info']/small", xmlValue)
if (is.null(theline)) {
        num.pages <- 1
        total.pages <- 1
} else
{
theline <- gsub('\\n', ' ', theline) ### breyta \n í bil
theline <- sub("\\s+$", "", theline) ### fjarlægja bilið sem hefur birst í endann
total.pages <- as.numeric(tail(strsplit(theline,split=" ")[[1]],1)) #### vinna seinasta orðið úr setningunni og nota það sem tölustaf.
num.pages <- c(1:total.pages)}

### Progress bar
#t <- 1
#pb <- txtProgressBar(1, total.pages, style=3)

#### hér er loopan til að sækja allt

for (i in num.pages){
        
        #The progress bar first
##        t <- (t+1)
##        Sys.sleep(0.02)
##        setTxtProgressBar(pb, t)  
        
        website <- paste0("http://www.mbl.is/fasteignir/leit/?page=", 1, "&q=abfafabeaea3e6435c02ce698e13fe6a")
        
        fixedURL <- getURL(website, followlocation=TRUE)
        parsed.html <- htmlParse(fixedURL, encoding='UTF-8')
        gotuheiti <- gsub('.{1}$', '', as.list(xpathApply(parsed.html, "//h4", xmlValue)))
        postnr <- as.numeric(gsub("[^0-9]", "", as.list(xpathApply(parsed.html, "//h5", xmlValue))))
        tegund <- xpathApply(parsed.html,"//div[@class='realestate-properties']/span[contains(.,'Tegund')]/strong", xmlValue)
        verd <- as.numeric(gsub("[^0-9]", "", xpathApply(parsed.html,"//div[@class='realestate-properties']/span[contains(.,'Verð')]/strong", xmlValue)))
        size <- as.numeric(gsub('.{3}$', '', xpathApply(parsed.html,"//div[@class='realestate-properties']/span[contains(.,'Stærð')]/strong", xmlValue)))
        herbergi <- as.numeric(xpathApply(parsed.html,"//div[@class='realestate-properties']/span[contains(.,'Herbergi')]/strong", xmlValue))
        
        # gera lista af dagsetningum
        nyskrad <- rep(list(as.character(Sys.Date())), length(gotuheiti))
        
        options(scipen=999)
        
        blanda <- as.data.frame(cbind(gotuheiti, postnr, tegund, verd, herbergi, size, nyskrad))
        
        ## hérna er fermverd búið til.
        fermverd <- round(as.numeric(blanda$verd)/as.numeric(blanda$size), digits = 0)
        fermverd <- as.list(fermverd)
        blanda$fermverd <- fermverd
        blanda_ready <- rbind(blanda_ready, blanda)
}
###keep only unique
blanda_ready <- unique(blanda_ready)

### unlist so I can save as cvs
blanda_ready$gotuheiti <- unlist(blanda_ready$gotuheiti)
blanda_ready$postnr <- unlist(blanda_ready$postnr)
blanda_ready$tegund <- unlist(blanda_ready$tegund) 
blanda_ready$verd <- unlist(blanda_ready$verd)
blanda_ready$herbergi <- unlist(blanda_ready$herbergi) 
blanda_ready$size <- unlist(blanda_ready$size)
blanda_ready$fermverd <- unlist(blanda_ready$fermverd)
blanda_ready$nyskrad <- unlist(blanda_ready$nyskrad)

# Breyta öllum inf í NA
blanda_ready[ blanda_ready == "Inf" ] = NA


# Remove ALL NA cases
blanda_ready <- na.omit(blanda_ready)

save.name <- paste0("/var/www/skrapari/ibudir/nyskradar_ibudir_verd_", Sys.Date(), ".csv")

write.csv2(blanda_ready, file = save.name)

Sys.sleep(3.0)

mydb = dbConnect(MySQL(), user='noi', password='noipassword', host='127.0.0.1', dbname="noi")
Sys.sleep(2.0)
dbWriteTable(mydb, "wp_ibudir", blanda_ready, append = TRUE, row.names = F, )



print("----")
print("ibudir -- nyskradar -- scraper successful")
print(date())
