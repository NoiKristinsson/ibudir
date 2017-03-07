read.csv("")


setwd("~/forritun/ibudir")
ibudir.data <- data.frame()
file_list <- list.files("ibudir")

for (file in file_list){
        
                temp_dataset <-read.csv2(paste0("ibudir/", file), header=TRUE)
                ibudir.data<-rbind(ibudir.data, temp_dataset)
                rm(temp_dataset)
}


## Hér er það sem er stefnt á að verði ekki með í forritinu.
## 1) Eignir í útlöndum sem eru með póstnúmer 1000
## 2) Aðrar eignir en heimili, t.d Sumarhús, Atvinnuhús, Jörð/Lóð, Hesthús, Fyrirt.
## 3) íbúðir sem kosta minna en fimm milljón til að sigta út leiguíbúðir (ekki séð íbúð í gögnunum á undir því verði).
## 4) Íbúðir með 0 herbergi
## 5) Fermetra verð undir 50.000 var í yfirgnæfandi tilfellum óbyggð hús og voru tekin út úr jöfnunni.
## 6) margt yfir 500 fm hefur verið að valda skekkju þar sem það eru oft lóðir innifaldar og var ákveðið að draga mörking þar
## 7) íbúðir yfir 169 milljónir voru oft á tíðum outliers sem hægt var að útskýra, t.a.m var oft lítið að marka fermetrafjölda og þess háttar

ibudir.clean <- subset(ibudir.data, 
                        postnr != 1000 & 
                               tegund != "Sumarhús" & 
                               tegund != "Atvinnuhús" & 
                               tegund != "Jörð/Lóð" &
                               tegund != "Hesthús" &
                               tegund != "Fyrirt." &
                               verd > 5000000 &
                               herbergi > 0 &
                               fermverd > 50000 &
                               size < 500 &
                               verd < 151000000
                                
                               )

# Set all dates to the first of the month for easy grouping
ibudir.clean$date <- format(as.Date(ibudir.clean$nyskrad), "%Y-%m-01")

ibudir.clean <- subset(ibudir.clean, date != "2017-02-01")



##rm(ibudir.clean$mydate)


##ibudir.clean <- subset(ibudir.clean, select = -mydate  )

write.csv(ibudir.clean, "data_clean.csv", fileEncoding = "UTF-8")
