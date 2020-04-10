# Libraries
library(readxl)
library(tidyverse)
library(writexl)

#### adjust Datasheet--------------------------------------
Datasheet_original<-read_excel("data/Datasheet_original.xlsx")


#add Bin_num column
Bin_num<-sub(".*\\-","",Datasheet_original$Bin)
Bin_num<-sub("\\ BP.*","",Bin_num)
Datasheet<-Datasheet_original%>%mutate(Bin_num=Bin_num)
Datasheet$Bin_num<-as.numeric(Datasheet$Bin_num)


#save new version of Datasheet
write_xlsx(Datasheet,"Datasheet.xlsx")


#### add country, lat, long to Datasheet--------------------
#Df with metadata for all sites (even the ones that are not in Datasheet)
LAPD_Andes <- read_excel("data/LAPD_Andes_MY.xlsx")
LAPD_Andes$LAPD_ID<-as.numeric(LAPD_Andes$LAPD_ID)

#add missing metadata to LAPD_Andes
#altitude
which(is.na(LAPD_Andes$Altitude))
LAPD_Andes[72,"Altitude"]=2608 #value found in the original paper
#latitude
view(LAPD_Andes%>%filter(is.na(Latitude)))
LAPD_Andes[70,"Latitude"]=4.58545
LAPD_Andes[70,"Longitude"]=-75.19558
LAPD_Andes[71,"Latitude"]=-0.57946
LAPD_Andes[71,"Longitude"]=-78.24397
LAPD_Andes[72,"Latitude"]=--00.25405
LAPD_Andes[72,"Longitude"]=-78.01075

#to check wich Sites have different names
Sitename=distinct(Datasheet,`Site Name`)
Nomatch<-match(Sitename$`Site Name`,LAPD_Andes$SiteName)
which(is.na(Nomatch))
Sites_Nomatch<-Sitename[c(8,11, 14, 23, 34, 35, 38),]
Sites_Nomatch$`Site Name`
LAPD_Andes$SiteName

#rename Datasheet Site Names according to the ones in LAPD_Andes
to_replace<-c("Cerro Toledo", "El Tiro\r\n" , "Laguna Natosas forest", "Pedras Blancas","Huila", "Paramo de Agua Blanca III", "Paramo de pena Negra I")
with_this<-c("Cerro Toledo CT", "El Tiro", "Lagunas Natosas Forest", "Piedras Blancas", "Huila\r\n", "Agua Blanca PAB III", "Paramo de Pena Negra 1")
Datasheet$`Site Name`<-Datasheet$`Site Name`%>%plyr::mapvalues(to_replace,with_this)

#add columns with country, latitude and longitude to Datasheet, per each site
Datasheet$Country<-LAPD_Andes$Country[match(Datasheet$`Site Name`,LAPD_Andes$SiteName)]
Datasheet$Latitude<-LAPD_Andes$Latitude[match(Datasheet$`Site Name`,LAPD_Andes$SiteName)]
Datasheet$Longitude<-LAPD_Andes$Longitude[match(Datasheet$`Site Name`,LAPD_Andes$SiteName)]

#see if there something NA left, and which site it corresponds to
view(filter(Datasheet,is.na(Country)))

#save datasheet and LAPD_Andes
write_xlsx(Datasheet,"Datasheet.xlsx")
write_xlsx(LAPD_Andes,"LAPD_Andes_MY.xlsx")

#### new shorter datasheet------------------------------------
#select record spanning last 12000 yr
Datasheet_shaved<-Datasheet%>%filter(Bin_num<12500)

#select only indicators actually found
#Step 1: Filter indicators from dataset
Datasheet.indicators <- Datasheet %>%
select(.,-c("LAPD_ID","Site Name",
"Reference (short)","Bin", "Bin_num", "Latitude","Longitude", "Country" ))

# Step 2: Convert counts from character to numeric
Datasheet.indicators <- apply (Datasheet.indicators,2,
                    FUN= function(x) as.numeric(unlist(x)))

# Step 4: Make datasheet with site vs number of times an indicator is counted  
Datasheet.indicators <- as.data.frame(Datasheet.indicators)

# calculate per human indicator the sum
DF.indicators.SUM <-data.frame(IN=apply(Datasheet.indicators, 2, 
                                       FUN = function(x) sum(x,na.rm = T)))
DF.indicators.SUM$IN.name <- row.names(DF.indicators.SUM) %>% as.factor() 

#datasheet with only indicators actually found
DF.zeros<-(DF.indicators.SUM%>%filter(IN==0)) 
Todrop<-as.vector(DF.zeros$IN.name)
Datasheet_shaved<-Datasheet_shaved[,!names(Datasheet_shaved)%in% Todrop]


#add column with records lenght to Datasheet_shaved-------
length_record<-Datasheet_shaved%>%group_by(`Site Name`)%>%summarise(max(Bin_num))
length_record$Length<-length_record$`max(Bin_num)`
Datasheet_shaved$Length<-length_record$Length[match(Datasheet_shaved$`Site Name`,length_record$`Site Name`)]

write_xlsx(Datasheet_shaved,"Datasheet_shaved.xlsx")